import google.generativeai as genai
import os
import json
import re
import time
import pandas as pd
from collections import deque
import sys
from thefuzz import process

# =========================
# 1. CONFIGURACIÓN GENERAL
# =========================

# Rutas
BASE_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__))) # camara-cl-api
PROJECT_ROOT = os.path.dirname(BASE_DIR) # Root del proyecto

BCN_JSON_PATH = os.path.join(PROJECT_ROOT, "bcn_diputados.json")
CSV_PATH = os.path.join(PROJECT_ROOT, "Harvard Dataverse", "Ideology estimates", "ideologia_congreso_chile_2002_2026_wide_format.csv")
PROMPT_FILE = os.path.join(BASE_DIR, "src", "prompts", "experience_prompt.txt")
OUTPUT_DIR = os.path.join(PROJECT_ROOT, "outputs-analyze-experience")

# Configuración API
MAX_REQUESTS_PER_MINUTE = 15
SECONDS_WINDOW = 60
INITIAL_RETRY_DELAY = 5
MAX_RETRY_DELAY = 60

MODEL_NAME = "gemini-2.5-flash-lite"

# API Key
GOOGLE_API_KEY = os.getenv("GOOGLE_API_KEY")
if not GOOGLE_API_KEY:
    # Intenta leer de un archivo .env o similar si es necesario, o asume que está en el entorno
    print("ADVERTENCIA: Variable de entorno GOOGLE_API_KEY no encontrada. Asegúrate de tenerla configurada.")

try:
    genai.configure(api_key=GOOGLE_API_KEY)
except Exception as config_err:
    raise RuntimeError(f"Error configurando la API: {config_err}")

print(f"Usando el modelo: {MODEL_NAME}")

# =========================
# 2. CARGA DE DATOS Y PROMPT
# =========================

def load_prompt():
    try:
        with open(PROMPT_FILE, "r", encoding="utf-8") as f_prompt:
            return f_prompt.read()
    except FileNotFoundError:
        raise FileNotFoundError(f"No se encontró el prompt: '{PROMPT_FILE}'")

def load_data():
    print("Cargando datos...")
    # Cargar JSON BCN
    with open(BCN_JSON_PATH, 'r', encoding='utf-8') as f:
        bcn_data = json.load(f)
    
    # Cargar CSV Harvard
    df = pd.read_csv(CSV_PATH)
    csv_names = df[['DiputadoId', 'Name']].to_dict('records')
    
    return bcn_data, csv_names

# =========================
# 3. MATCHING Y PRE-PROCESAMIENTO
# =========================

def match_diputados(csv_names, bcn_data):
    """
    Empareja nombres del CSV con perfiles del JSON usando fuzzy matching.
    """
    print("Realizando matching de nombres...")
    
    # Crear diccionario de búsqueda rápida para BCN {nombre_completo: perfil}
    bcn_lookup = {}
    bcn_names = []
    for p in bcn_data:
        name = p['antecedentes_personales']['nombre_completo']
        if name:
            bcn_lookup[name] = p
            bcn_names.append(name)
    
    matched_data = []
    
    for item in csv_names:
        csv_id = item['DiputadoId']
        csv_name = item['Name']
        
        # Fuzzy match
        match, score = process.extractOne(csv_name, bcn_names)
        
        if score >= 85: # Umbral de similitud
            bcn_profile = bcn_lookup[match]
            
            # Construir objeto limpio para la IA
            diputado_input = {
                "id_csv": csv_id,
                "nombres": {
                    "csv": csv_name,
                    "bcn": match
                },
                "datos_biograficos": {
                    "profesion": bcn_profile['antecedentes_personales'].get('profesion'),
                    "grado_academico": bcn_profile['antecedentes_personales'].get('grado_academico'),
                    "resena_biografica": bcn_profile['biografia'],
                    "trayectoria_parlamentaria": [
                        {k: v for k, v in t.items() if k in ['cargo', 'anio_inicio', 'anio_termino', 'partido']}
                        for t in bcn_profile.get('trayectoria_parlamentaria', [])
                    ],
                    "cargos_publicos": bcn_profile.get('cargos_publicos', [])
                }
            }
            matched_data.append(diputado_input)
        else:
            print(f"  No match found for: {csv_name} (Best: {match}, Score: {score})")
            
    print(f"Total matched: {len(matched_data)} / {len(csv_names)}")
    return matched_data

# =========================
# 4. GESTIÓN DE API (RATE LIMIT Y LLAMADAS)
# =========================

request_timestamps = deque()

def ensure_rate_limit():
    """Aplica control de frecuencia."""
    try:
        current_time = time.monotonic()
        while request_timestamps and current_time - request_timestamps[0] > SECONDS_WINDOW:
            request_timestamps.popleft()

        if len(request_timestamps) >= MAX_REQUESTS_PER_MINUTE:
            wait_time = max(0, SECONDS_WINDOW - (current_time - request_timestamps[0])) + 0.2
            print(f"  -- Rate Limit. Esperando {wait_time:.1f}s...", end="\r")
            time.sleep(wait_time)
    except Exception as e:
        print(f"  !! Error control frecuencia: {e}")

def call_model_with_batch(model, batch_data, prompt_base, max_attempts=5):
    """
    Envía un batch de datos a la API.
    """
    # Convertir batch a JSON string
    batch_json_str = json.dumps(batch_data, ensure_ascii=False, indent=2)
    full_prompt = f"{prompt_base}\n\n**Analiza los siguientes datos:**\n\n{batch_json_str}"
    
    response_text = None
    error_str = None

    for attempt in range(1, max_attempts + 1):
        ensure_rate_limit()

        try:
            print(f"  Intento {attempt}/{max_attempts}: Enviando batch...")
            
            generation_config = genai.types.GenerationConfig(
                temperature=0.1,
                response_mime_type="application/json" # Forzar JSON output
            )

            response = model.generate_content(
                full_prompt,
                generation_config=generation_config
            )

            request_timestamps.append(time.monotonic())
            response_text = response.text
            break

        except Exception as e:
            msg = str(e)
            print(f"  !! Error: {msg[:100]}...")
            
            is_retryable = any(x in msg.lower() for x in ["429", "500", "503", "overloaded", "quota"])
            
            if is_retryable and attempt < max_attempts:
                # Extraer tiempo de espera sugerido si existe
                wait_time = INITIAL_RETRY_DELAY * attempt
                if "retry in" in msg.lower():
                    try:
                        match = re.search(r"retry in (\d+(\.\d+)?)s", msg.lower())
                        if match:
                            wait_time = float(match.group(1)) + 1.0 # +1s buffer
                    except:
                        pass
                
                print(f"  Reintentando en {wait_time:.1f}s...")
                time.sleep(wait_time)
                continue
            else:
                error_str = msg
                break
    
    return response_text, error_str

def parse_response(response_text):
    if not response_text:
        return None
    try:
        # Limpiar posibles bloques de código markdown
        clean_text = response_text.replace("```json", "").replace("```", "").strip()
        return json.loads(clean_text)
    except json.JSONDecodeError:
        return None

# =========================
# 5. EJECUCIÓN PRINCIPAL
# =========================

def main():
    # 1. Setup
    if not os.path.exists(OUTPUT_DIR):
        os.makedirs(OUTPUT_DIR)
    
    prompt_base = load_prompt()
    bcn_data, csv_names = load_data()
    
    # 2. Match
    matched_diputados = match_diputados(csv_names, bcn_data)
    
    # 3. Batching
    BATCH_SIZE = 10
    batches = [matched_diputados[i:i + BATCH_SIZE] for i in range(0, len(matched_diputados), BATCH_SIZE)]
    
    print(f"Se procesarán {len(batches)} lotes.")
    
    # Inicializar modelo
    model = genai.GenerativeModel(MODEL_NAME)
    
    # 4. Loop
    
    all_results = []
    
    for i, batch in enumerate(batches):
        print(f"\n--- Procesando Lote {i+1}/{len(batches)} ({len(batch)} diputados) ---")
        
        # Verificar si ya existe output (opcional, para resume)
        batch_file = os.path.join(OUTPUT_DIR, f"batch_{i}.json")
        if os.path.exists(batch_file):
            print("  Lote ya procesado. Saltando.")
            continue
            
        response_text, error = call_model_with_batch(model, batch, prompt_base)
        
        if error:
            print(f"  Error procesando lote {i}: {error}")
            continue
            
        parsed_data = parse_response(response_text)
        
        if parsed_data:
            # Guardar resultado parcial
            with open(batch_file, 'w', encoding='utf-8') as f:
                json.dump(parsed_data, f, ensure_ascii=False, indent=2)
            print(f"  Lote {i+1} guardado exitosamente.")
            all_results.extend(parsed_data)
        else:
            print("  Error parseando respuesta JSON del modelo.")

    print("\n--- Proceso finalizado ---")

if __name__ == "__main__":
    main()

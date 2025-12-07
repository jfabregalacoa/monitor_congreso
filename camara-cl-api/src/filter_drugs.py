import os
import sys
import json
import glob
import logging
import tempfile
from collections import Counter
from typing import Dict, Any, List, Optional, Tuple

sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
try:
    from src.utils import setup_logging  # type: ignore
except ImportError:
    try:
        from utils import setup_logging  # type: ignore
    except ImportError:
        def setup_logging():
            logging.basicConfig(
                level=logging.INFO,
                format="%(asctime)s - %(levelname)s - %(message)s"
            )

import requests
from urllib.parse import urlparse, urlunparse

# Additional imports for vote aggregation and XML parsing
try:
    import pandas as pd  # type: ignore
    HAS_PANDAS = True
except ImportError:
    HAS_PANDAS = False

import xml.etree.ElementTree as ET

# Global caches to avoid repeated I/O and network calls
period_df_cache: Dict[str, Any] = {}
militancia_cache: Dict[str, Any] = {}

# Intentar importar librerías para extracción de texto
try:
    from docx import Document
    HAS_DOCX = True
except ImportError:
    HAS_DOCX = False

try:
    import fitz  # PyMuPDF
    HAS_PYMUPDF = True
except ImportError:
    HAS_PYMUPDF = False

# Para Windows con Microsoft Word instalado
try:
    import win32com.client
    HAS_WIN32COM = True
except ImportError:
    HAS_WIN32COM = False


# =========================
#  KEYWORDS (SIN TABACO)
# =========================

DRUG_KEYWORDS_RAW = [
    # genérico drogas / consumo
    "droga", "drogas",
    "drogadiccion", "drogodependencia",
    "consumo de drogas", "consumidor de drogas",
    "farmacodependencia", "farmaco dependiente",

    # sustancias ilícitas / estupefacientes
    "sustancia ilicita", "sustancias ilicitas",
    "sustancia ilícita", "sustancias ilícitas",
    "sustancias prohibidas",
    "estupefaciente", "estupefacientes",

    # narcotráfico / microtráfico
    "narcotrafico", "narco", "narcotráfico",
    "microtrafico", "microtráfico",
    "trafico de drogas", "tráfico de drogas",

    # psicoactivos / psicotrópicos
    "psicoactivo", "psicoactivos",
    "psicotropico", "psicotrópico", "psicotropicos", "psicotrópicos",
    "medicamentos controlados",
    "benzodiacepina", "benzodiacepinas",
    "benzodiazepina", "benzodiazepinas",
    "clonazepam", "alprazolam", "diazepam",

    # cannabis / marihuana
    "marihuana", "mariguana",
    "cannabis", "cannabis sativa",
    "canamo", "cáñamo",
    "hachis", "hachís",

    # otras drogas específicas
    "cocaina", "cocaína",
    "pasta base", "crack",
    "opio", "heroina", "heroína",
    "lsd", "acido lisergico", "ácido lisérgico",
    "mdma", "extasis", "éxtasis",
    "ketamina",
    "inhalante", "inhalantes", "solvente", "solventes",

    # anfetaminas y estimulantes
    "anfetamina", "anfetaminas",
    "metanfetamina", "metanfetaminas",
    "tusi", "2c-b",

    # alcohol
    "alcohol", "alcoholes",
    "bebida alcoholica", "bebidas alcoholicas",
    "bebida alcohólica", "bebidas alcohólicas",
    "licor", "licores",

    # ley / políticas de drogas
    "ley de drogas",
    "control de drogas",
    "prevencion del consumo de drogas",
    "prevención del consumo de drogas",

    # SENDA / servicio drogas
    "servicio nacional para la prevencion del consumo y trafico de drogas",
    "prevención del consumo y tráfico de drogas",
    "uso de drogas"
]


# =========================
#  FUNCIONES AUXILIARES
# =========================

def normalize_text(text: Optional[str]) -> str:
    """Pasa a minúsculas y elimina tildes/ñ para comparar con keywords."""
    if not text:
        return ""
    text = text.lower()
    replacements = str.maketrans(
        "áéíóúüñ",
        "aeiouun"
    )
    return text.translate(replacements)


DRUG_KEYWORDS = [normalize_text(k) for k in DRUG_KEYWORDS_RAW]

# ======================================================================
#  VOTE CATEGORIZATION AND PARTY AGGREGATION HELPERS
#
#  These helper functions make it possible to parse the roll‑call matrices
#  (where each vote appears as a column keyed by its ID) and to map the
#  raw vote strings (e.g. "Afirmativo", "En Contra", "Abstención") to a
#  simplified set of categories (apruebo, rechazo, abstencion, otro). They
#  also fetch the party affiliation of a diputado at the time of a vote
#  by calling the Cámara de Diputados web service【390651737376531†L8-L14】.  Caches
#  are used to minimise redundant file loading and network requests.
# ======================================================================

def map_vote_to_category(raw_vote: Any) -> str:
    """
    Map a raw vote string to one of the categories: 'apruebo', 'rechazo',
    'abstencion' or 'otro'. Afirmativo -> apruebo; En Contra -> rechazo;
    Abstención -> abstencion; anything else (Dispensado, Pareja,
    Incompatible, etc.) -> otro.
    """
    if not isinstance(raw_vote, str):
        return 'otro'
    norm = normalize_text(raw_vote).strip()
    if not norm:
        return 'otro'
    if norm.startswith('afirmativo'):
        return 'apruebo'
    if norm.startswith('en contra'):
        return 'rechazo'
    if norm.startswith('abstencion'):
        return 'abstencion'
    return 'otro'


def load_matrix_for_period(period: str, details_dir: str) -> Optional[Any]:
    """
    Load the roll‑call matrix CSV for a given period.  The CSV files are
    typically named 'matriz__periodo_YYYY_MM.csv' and reside either in the
    same directory as the details file, in the project root's
    'Harvard Dataverse/Roll calls' directory, or alongside this script.  The
    DataFrame is cached so that repeated calls for the same period avoid
    reloading the file.
    """
    if not HAS_PANDAS:
        return None
    if period in period_df_cache:
        return period_df_cache[period]
    matrix_filename = f"matriz__periodo_{period}.csv"
    # Determine candidate locations
    script_dir = os.path.dirname(os.path.abspath(__file__))
    project_root = os.path.dirname(os.path.dirname(script_dir))
    candidate_paths = [
        os.path.join(details_dir, matrix_filename),
        os.path.join(script_dir, matrix_filename),
        os.path.join(os.getcwd(), matrix_filename),
        os.path.join(project_root, 'Harvard Dataverse', 'Roll calls', matrix_filename),
    ]
    for candidate in candidate_paths:
        if os.path.exists(candidate):
            try:
                df = pd.read_csv(candidate)
                period_df_cache[period] = df
                return df
            except Exception:
                # Continue searching on failure
                continue
    # Not found
    return None


def get_vote_categories(period: str, vote_id: str, details_dir: str) -> Dict[str, List[str]]:
    """
    For a given period and vote ID, return a dictionary mapping each
    category ('apruebo', 'rechazo', 'abstencion', 'otro') to the list of
    diputado IDs (as strings) who cast that type of vote. If the
    period's matrix or the vote column cannot be located, empty lists
    are returned.
    """
    categories = {'apruebo': [], 'rechazo': [], 'abstencion': [], 'otro': []}
    df = load_matrix_for_period(period, details_dir)
    if df is None:
        return categories
    if vote_id not in df.columns:
        return categories
    col = df[vote_id]
    # Pandas 2.x removed the `iteritems` method in favour of `items`.
    for idx, raw_vote in col.items():
        if pd.isna(raw_vote):
            continue
        dip_id = str(df.at[idx, 'DiputadoId'])
        cat = map_vote_to_category(str(raw_vote))
        categories.setdefault(cat, []).append(dip_id)
    return categories


def fetch_militancias_for_diputado(dip_id: str) -> List[Tuple[str, str, str]]:
    """
    Fetch the militancia history for a diputado.  Returns a list of
    (start_date, end_date, party_id) tuples.  End dates may be empty; if so,
    '9999-12-31' is used as an open‑ended period.  Results are cached.
    """
    if dip_id in militancia_cache:
        return militancia_cache[dip_id]
    # Utilizar el servicio WSDiputado para obtener el detalle del diputado.
    # La URL de HTTP GET se especifica en la documentación del servicio【821946896054339†L134-L145】.
    url = (
        "https://opendata.camara.cl/camaradiputados/WServices/"
        f"WSDiputado.asmx/retornarDiputado?prmDiputadoId={dip_id}"
    )
    militancias: List[Tuple[str, str, str]] = []
    try:
        # Incluir un User-Agent para evitar bloqueos de algunos servidores
        resp = requests.get(
            url,
            headers={
                "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"
            },
            timeout=15,
            allow_redirects=True,
        )
        if resp.status_code != 200:
            militancia_cache[dip_id] = militancias
            return militancias
    except Exception:
        militancia_cache[dip_id] = militancias
        return militancias
    try:
        root = ET.fromstring(resp.content)
    except ET.ParseError:
        militancia_cache[dip_id] = militancias
        return militancias
    for mil in root.findall('.//{*}Militancia'):
        fi = mil.find('{*}FechaInicio')
        ft = mil.find('{*}FechaTermino')
        partido = mil.find('{*}Partido')
        pid = None
        if partido is not None:
            alias = partido.find('{*}Alias')
            pid_tag = partido.find('{*}Id')
            name_tag = partido.find('{*}Nombre')
            if alias is not None and alias.text:
                pid = alias.text.strip()
            elif pid_tag is not None and pid_tag.text:
                pid = pid_tag.text.strip()
            elif name_tag is not None and name_tag.text:
                pid = name_tag.text.strip()
        start_date = fi.text.strip()[:10] if fi is not None and fi.text else None
        end_date = ft.text.strip()[:10] if ft is not None and ft.text else None
        if start_date and pid:
            militancias.append((start_date, end_date or '9999-12-31', pid))
    militancias.sort(key=lambda x: x[0])
    militancia_cache[dip_id] = militancias
    return militancias


def get_party_for_date(dip_id: str, vote_date: str) -> str:
    """
    Given a diputado ID and a vote date (ISO format), return the party
    affiliation on that date.  If no matching militancia period is
    found, returns 'SIN_PARTIDO'.  If the militancia history is empty,
    returns 'SIN_PARTIDO'.
    """
    militancias = fetch_militancias_for_diputado(dip_id)
    if not militancias:
        return 'SIN_PARTIDO'
    vote_day = vote_date[:10] if vote_date else ''
    for start, end, pid in militancias:
        if start <= vote_day <= end:
            return pid
    return militancias[-1][2] if militancias else 'SIN_PARTIDO'


def is_drug_related(item: Dict[str, Any]) -> bool:
    """
    Determina si una votación está relacionada con drogas / alcohol
    usando solo desc_titulo.
    """
    titulo = item.get("desc_titulo", "") or ""
    norm = normalize_text(titulo)

    for kw in DRUG_KEYWORDS:
        if kw in norm:
            return True
    return False


def fetch_anyhow(url: str, retries: int = 3) -> Optional[bytes]:
    """
    Intenta descargar un documento probando varias combinaciones de dominio y
    protocolo (http/https) y usando cabeceras de navegador para evitar bloqueos.
    """
    session = requests.Session()
    session.headers.update({
        "User-Agent": (
            "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
            "AppleWebKit/537.36 (KHTML, like Gecko) "
            "Chrome/118.0.5993.177 Safari/537.36"
        ),
        "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
        "Accept-Language": "es-CL,es;q=0.9,en;q=0.8",
        "Referer": "https://www.senado.cl/"
    })
    
    alt_urls = [url]
    parsed = urlparse(url)
    alt_scheme = "https" if parsed.scheme == "http" else "http"
    alt_urls.append(urlunparse((alt_scheme, parsed.netloc, parsed.path,
                                parsed.params, parsed.query, parsed.fragment)))
    if "www.senado.cl" in url:
        alt_urls.append(url.replace("www.senado.cl", "tramitacion.senado.cl"))

    for alt in alt_urls:
        for attempt in range(retries):
            try:
                response = session.get(alt, timeout=15, allow_redirects=True, verify=False)
                if response.status_code == 200 and response.content:
                    return response.content
            except requests.RequestException as e:
                logging.debug(f"Error al intentar {alt}: {e}")
    return None


def extract_text_from_docx(content: bytes) -> Optional[str]:
    """
    Extrae texto de archivos .docx usando python-docx.
    """
    if not HAS_DOCX:
        return None
    
    try:
        with tempfile.NamedTemporaryFile(delete=False, suffix=".docx") as tmp:
            tmp.write(content)
            tmp_path = tmp.name
        
        try:
            doc = Document(tmp_path)
            paragraphs = [p.text.strip() for p in doc.paragraphs if p.text.strip()]
            if paragraphs:
                return '\n\n'.join(paragraphs)
        finally:
            try:
                os.unlink(tmp_path)
            except OSError:
                pass
                
    except Exception as e:
        logging.debug(f"Error extrayendo texto DOCX: {e}")
    
    return None


def extract_text_from_ole_doc(content: bytes) -> Optional[str]:
    """
    Extrae texto de archivos .doc antiguos usando Microsoft Word via COM.
    Requiere Windows con Microsoft Word instalado.
    """
    if not HAS_WIN32COM:
        logging.warning("Para extraer .doc antiguos instala pywin32: pip install pywin32")
        return None
    
    try:
        with tempfile.NamedTemporaryFile(delete=False, suffix=".doc") as tmp:
            tmp.write(content)
            tmp_path = tmp.name
        
        try:
            # Usar Word via COM
            word = win32com.client.Dispatch("Word.Application")
            word.Visible = False
            
            doc = word.Documents.Open(tmp_path)
            text = doc.Content.Text
            doc.Close(False)
            word.Quit()
            
            if text and text.strip():
                return text.strip()
                
        except Exception as e:
            logging.debug(f"Error usando Word COM: {e}")
        finally:
            try:
                os.unlink(tmp_path)
            except OSError:
                pass
                
    except Exception as e:
        logging.debug(f"Error extrayendo texto DOC: {e}")
    
    return None


def extract_text_from_pdf(content: bytes) -> Optional[str]:
    """
    Extrae texto de archivos PDF usando PyMuPDF.
    """
    if not HAS_PYMUPDF:
        return None
    
    try:
        doc = fitz.open(stream=content, filetype="pdf")
        text_parts = []
        
        for page in doc:
            text = page.get_text()
            if text.strip():
                text_parts.append(text.strip())
        
        doc.close()
        
        if text_parts:
            return '\n\n'.join(text_parts)
            
    except Exception as e:
        logging.debug(f"Error extrayendo texto PDF: {e}")
    
    return None


def extract_text_from_doc(content: bytes) -> Optional[str]:
    """
    Intenta extraer texto de un documento.
    Detecta tipo real por magic bytes y usa la librería apropiada.
    """
    # Detectar tipo de archivo por magic bytes
    is_pdf = content[:5] == b'%PDF-'
    is_zip = content[:4] == b'PK\x03\x04'  # DOCX es un ZIP
    is_ole = content[:8] == b'\xd0\xcf\x11\xe0\xa1\xb1\x1a\xe1'  # DOC antiguo (OLE)
    
    detected_type = "desconocido"
    text = None
    
    # Si es PDF
    if is_pdf:
        detected_type = "PDF"
        if HAS_PYMUPDF:
            text = extract_text_from_pdf(content)
        else:
            logging.warning("Archivo es PDF pero PyMuPDF no está instalado")
    
    # Si es DOCX (ZIP)
    elif is_zip:
        detected_type = "DOCX"
        if HAS_DOCX:
            text = extract_text_from_docx(content)
        else:
            logging.warning("Archivo es DOCX pero python-docx no está instalado")
    
    # Si es DOC antiguo (OLE)
    elif is_ole:
        detected_type = "DOC"
        if HAS_WIN32COM:
            text = extract_text_from_ole_doc(content)
        else:
            logging.warning("Archivo es DOC antiguo - instala pywin32 y ten Word instalado")
    
    else:
        logging.warning(f"Tipo de archivo no reconocido. Primeros bytes: {content[:20]}")
    
    if text:
        logging.debug(f"Extraído texto de {detected_type}: {len(text)} caracteres")
    else:
        logging.warning(f"No se pudo extraer texto del archivo {detected_type}")
    
    return text


def download_and_extract_text(url: Optional[str]) -> Tuple[Optional[str], Optional[int], bool, str]:
    """
    Descarga el documento y extrae su texto.
    Retorna: (texto_extraido, tamaño_bytes, exito_extraccion, tipo_detectado)
    """
    if not url:
        return None, None, False, "sin_url"

    content = fetch_anyhow(url)
    if not content:
        logging.warning(f"No se pudo descargar documento desde: {url}")
        return None, None, False, "error_descarga"

    doc_size = len(content)
    
    # Detectar tipo
    if content[:5] == b'%PDF-':
        doc_type = "PDF"
    elif content[:4] == b'PK\x03\x04':
        doc_type = "DOCX"
    elif content[:8] == b'\xd0\xcf\x11\xe0\xa1\xb1\x1a\xe1':
        doc_type = "DOC"
    else:
        doc_type = "desconocido"
    
    text = extract_text_from_doc(content)
    
    if text:
        # Limpiar texto
        lines = [line.strip() for line in text.split("\n")]
        lines = [line for line in lines if line]
        text = "\n".join(lines)
        return text, doc_size, True, doc_type
    else:
        logging.warning(f"No se pudo extraer texto del documento: {url}")
        return None, doc_size, False, doc_type


def process_details_file(path: str) -> List[Dict[str, Any]]:
    """
    Lee un archivo de detalles, filtra votaciones sobre drogas
    y devuelve una lista de registros extendidos con texto extraído.
    """
    logging.info(f"Procesando archivo: {path}")
    try:
        with open(path, "r", encoding="utf-8") as f:
            data = json.load(f)
    except Exception as e:
        logging.error(f"Error leyendo {path}: {e}")
        return []

    if not isinstance(data, list):
        logging.warning(f"{path} no contiene una lista de registros, se omite.")
        return []

    results: List[Dict[str, Any]] = []
    base_name = os.path.basename(path)

    # Determine the directory where this details file resides.  This is used
    # to locate the corresponding roll‑call matrix CSV for the period.
    details_dir = os.path.dirname(path)

    for item in data:
        if not isinstance(item, dict):
            continue

        if not is_drug_related(item):
            continue

        url_doc = item.get("desc_link_mensaje_mocion")
        logging.info(
            f"Match drogas: vote_id={item.get('vote_id')} | boletin={item.get('desc_boletin')} ({base_name})"
        )

        doc_texto, doc_size, extraction_ok, doc_type = download_and_extract_text(url_doc)

        nuevo = dict(item)
        nuevo["drug_match_flag"] = True
        nuevo["drug_match_source"] = "desc_titulo"
        nuevo["source_file"] = base_name
        nuevo["doc_url"] = url_doc
        nuevo["doc_size_bytes"] = doc_size
        nuevo["doc_type"] = doc_type
        nuevo["doc_texto"] = doc_texto
        nuevo["doc_extraction_ok"] = extraction_ok
        # -----------------------------------------------------------------
        # Extended fields: obtain vote categories and party aggregation
        # Only proceed if the details file name encodes a period such as
        # 'details__periodo_YYYY_MM.json' and pandas is available.  If
        # either condition fails, we still include empty lists/dict.
        period = None
        if base_name.startswith("details__periodo_") and base_name.endswith(".json"):
            period = base_name[len("details__periodo_"):-len(".json")]
        if period and HAS_PANDAS:
            vid = str(item.get('vote_id', ''))
            categories = get_vote_categories(period, vid, details_dir)
            # Store lists of diputado IDs per category
            nuevo['apruebo_ids'] = categories.get('apruebo', [])
            nuevo['rechazo_ids'] = categories.get('rechazo', [])
            nuevo['abstencion_ids'] = categories.get('abstencion', [])
            nuevo['otro_ids'] = categories.get('otro', [])
            # Aggregate by party
            vote_date = str(item.get('fecha', ''))
            party_counts: Dict[str, Dict[str, int]] = {}
            for cat_name, dip_list in categories.items():
                for dip in dip_list:
                    party = get_party_for_date(dip, vote_date)
                    if party not in party_counts:
                        party_counts[party] = {
                            'apruebo': 0,
                            'rechazo': 0,
                            'abstencion': 0,
                            'otro': 0,
                        }
                    party_counts[party][cat_name] += 1
            nuevo['votos_por_partido'] = party_counts
        else:
            nuevo['apruebo_ids'] = []
            nuevo['rechazo_ids'] = []
            nuevo['abstencion_ids'] = []
            nuevo['otro_ids'] = []
            nuevo['votos_por_partido'] = {}

        results.append(nuevo)

    logging.info(f"{len(results)} registros de drogas en {base_name}")
    return results


def main():
    setup_logging()
    logging.info("=== Filtrar votaciones sobre drogas/alcohol (JSON Senado) ===")
    
    # Verificar dependencias
    if HAS_PYMUPDF:
        logging.info("PyMuPDF disponible para archivos PDF")
    else:
        logging.warning("PyMuPDF NO instalado - pip install pymupdf")
    
    if HAS_DOCX:
        logging.info("python-docx disponible para archivos DOCX")
    else:
        logging.warning("python-docx NO instalado - pip install python-docx")
    
    if HAS_WIN32COM:
        logging.info("pywin32 disponible para archivos DOC antiguos (requiere Word)")
    else:
        logging.warning("pywin32 NO instalado - pip install pywin32 (requiere Word)")

    script_dir = os.path.dirname(os.path.abspath(__file__))
    outputs_dir = os.path.join(script_dir, "..", "outputs")

    if not os.path.exists(outputs_dir):
        outputs_dir = os.path.join(script_dir, "outputs")
    if not os.path.exists(outputs_dir):
        outputs_dir = "outputs"

    pattern = os.path.join(outputs_dir, "details__periodo_*.json")
    files = sorted(glob.glob(pattern))

    if not files:
        logging.error(f"No se encontraron archivos con patrón: {pattern}")
        return

    logging.info(f"Se encontraron {len(files)} archivos de detalles para procesar.")

    all_results: List[Dict[str, Any]] = []
    for path in files:
        all_results.extend(process_details_file(path))

    if not all_results:
        logging.warning("No se encontraron votaciones relacionadas con drogas.")
        return

    output_path = os.path.join(outputs_dir, "details__drogas.json")
    try:
        with open(output_path, "w", encoding="utf-8") as f:
            json.dump(all_results, f, ensure_ascii=False, indent=2)
        logging.info(f"Guardado JSON unificado en: {output_path}")
        logging.info(f"Total registros: {len(all_results)}")
        
        # Estadísticas por tipo
        exitosos = sum(1 for r in all_results if r.get("doc_extraction_ok"))
        logging.info(f"Documentos con texto extraído: {exitosos}/{len(all_results)}")
        
        # Desglose por tipo de documento
        tipos = Counter(r.get("doc_type", "desconocido") for r in all_results)
        tipos_ok = Counter(r.get("doc_type", "desconocido") for r in all_results if r.get("doc_extraction_ok"))
        
        for tipo, total in tipos.items():
            ok = tipos_ok.get(tipo, 0)
            logging.info(f"  {tipo}: {ok}/{total} extraídos")
        
    except Exception as e:
        logging.error(f"Error guardando {output_path}: {e}")


if __name__ == "__main__":
    main()
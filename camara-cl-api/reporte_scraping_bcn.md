# Reporte de Web Scraping: Perfiles Biográficos BCN

Este documento explica la arquitectura y el funcionamiento del scraper desarrollado para extraer información de los parlamentarios chilenos desde el sitio web de la Biblioteca del Congreso Nacional (BCN). Este modelo servirá como base para el nuevo scraper de Convencionales Constituyentes (2021-2022).

## 1. Arquitectura General del Scraper (`fetch_biography_2.py`)

El proceso de recolección de datos consta de las siguientes fases principales:

1. **Obtención de Enlaces (Listing Crawl):** 
   El scraper primero consulta el listado general de parlamentarios para un periodo determinado. Extrae todas las URLs canónicas de los perfiles individuales que coincidan con la búsqueda.
   
2. **Descarga y Parseo de Perfiles (Profile Parsing):** 
   Por cada URL estructurada, se descarga el código HTML y se procesa utilizando `BeautifulSoup`. 

3. **Extracción de Datos Dinámicos (AJAX):**
   Algunos datos (como intervenciones en comisiones) no están en el DOM inicial, sino que se cargan de fondo. El scraper identifica el `id_persona` interno de BCN y hace una petición directa al endpoint REST/AJAX (`getParticipaciones.html`).

4. **Estructura de Salida:**
   Toda la información se consolida en un diccionario estandarizado que luego se exporta a un archivo JSON (como `bcn_diputados.json`).

## 2. Módulos de Extracción (Scraping Específico)

La lógica crítica se encuentra separada en múltiples funciones dentro de la clase principal para aislar la complejidad estructural del HTML local:

* `extract_antecedentes_personales`: Extrae metadatos de la barra lateral (nombres, fechas de nacimiento, profesión, lugar de origen). Contiene lógica para manejar formatos de fecha inconsistentes o solo años.
* `extract_trayectoria_parlamentaria`: Identifica cargos, periodos de inicio y fin, y distrito/circunscripción. Aquí fue crucial añadir tolerancia a fechas en formato texto (ej: "21 de junio 2005").
* `extract_biografia`: Lee el bloque central de texto y parsea los títulos `<h3>` ocultos o clases asociadas a "Familia y juventud", "Estudios y vida laboral", etc.
* `extract_legislaturas`: Analiza el texto para rescatar cuáles fueron las "Comisiones Permanentes" y "Comisiones Especiales". Utiliza Expresiones Regulares (RegEx) para buscar patrones lingüísticos (ej. "integró las comisiones permanentes de...") y separar por punto y coma (`;`).

## 3. Consideraciones para Convencionales Constituyentes

Si bien los Convencionales están en la misma plataforma (BCN), al usar esta estructura base como molde, debes fijarte en lo siguiente:

1. **URL de Búsqueda Inicial:** La URL de búsqueda o los filtros que alimentan la función de "listing" (`get_listing_urls`) deberán cambiar para apuntar específicamente a los convencionales de 2021-2022.
2. **Nombres de Cargos:** El scraper actual asume "Diputado" o "Senador". Para los convencionales, la metadata de "Trayectoria" seguramente dirá "Convencional Constituyente".
3. **Comisiones de la Convención:** Probablemente las comisiones descritas en texto tengan nombres diferentes ("Comisión de Sistema Político", "Comisión de Armonización", etc.), pero las reglas regex actuales (buscar "comisión permanente de", separar por `;`) podrían servir casi sin alteraciones si el editor humano de la BCN mantuvo el mismo estilo de redacción.

## 4. Dependencias Críticas
- `requests`: Para manejar las sesiones HTTP.
- `BeautifulSoup` (bs4): Para navegar el árbol DOM (nodos HTML).
- `json`: Para la serialización de datos.
- `re`: Expresiones regulares para extracción de datos limpios desde textos en prosa.

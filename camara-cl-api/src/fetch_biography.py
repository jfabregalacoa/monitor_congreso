import requests
from bs4 import BeautifulSoup
import json
import time
import re
import logging
from urllib.parse import urljoin
from datetime import datetime
import locale

# Set locale for date parsing if possible, otherwise handle manually
try:
    locale.setlocale(locale.LC_TIME, 'es_CL.UTF-8')
except locale.Error:
    try:
        locale.setlocale(locale.LC_TIME, 'es_ES.UTF-8')
    except locale.Error:
        pass # Fallback to manual parsing

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler("scraper.log"),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger(__name__)

class BCNBiographyScraper:
    BASE_URL = "https://www.bcn.cl/historiapolitica/resenas_parlamentarias/index.html"
    WIKI_BASE_URL = "https://www.bcn.cl/historiapolitica/resenas_parlamentarias/wiki/"
    AJAX_ENDPOINT = "https://www.bcn.cl/historiapolitica/resenas_parlamentarias/getParticipaciones.html"

    def __init__(self, output_file="bcn_diputados.json", delay=1.0):
        self.session = requests.Session()
        self.session.headers.update({
            'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.114 Safari/537.36'
        })
        self.output_file = output_file
        self.delay = delay
        self.data = []

    def _sleep(self):
        time.sleep(self.delay)

    def parse_spanish_date(self, date_str):
        """
        Parses dates like '9 de Septiembre de 2025' or '1977-06-09'.
        Returns ISO format YYYY-MM-DD.
        """
        if not date_str:
            return None
        
        date_str = date_str.strip()
        
        # Try ISO format first
        try:
            return datetime.strptime(date_str, "%Y-%m-%d").strftime("%Y-%m-%d")
        except ValueError:
            pass

        # Manual mapping for Spanish months to handle case sensitivity and locale issues
        months = {
            'enero': '01', 'febrero': '02', 'marzo': '03', 'abril': '04',
            'mayo': '05', 'junio': '06', 'julio': '07', 'agosto': '08',
            'septiembre': '09', 'octubre': '10', 'noviembre': '11', 'diciembre': '12'
        }
        
        # Regex for "9 de Septiembre de 2025"
        match = re.search(r'(\d{1,2})\s+de\s+([a-zA-Z]+)\s+de\s+(\d{4})', date_str, re.IGNORECASE)
        if match:
            day, month_name, year = match.groups()
            month = months.get(month_name.lower())
            if month:
                return f"{year}-{month}-{day.zfill(2)}"
        
        return None

    def get_listing_urls(self, period="1990-2018"):
        """
        Iterates through listing pages for a given period.
        Yields URLs to person detail pages.
        """
        page = 1
        while True:
            logger.info(f"Fetching listing page {page} for period {period}...")
            params = {
                'categ': 'por_periodo',
                'periodo': period,
                'pagina': str(page)
            }
            try:
                response = self.session.get(self.BASE_URL, params=params)
                response.raise_for_status()
            except requests.RequestException as e:
                logger.error(f"Error fetching listing page {page}: {e}")
                break

            soup = BeautifulSoup(response.content, 'html.parser')
            
            # Identify the list container. Based on BCN structure, usually a div with class 'row' or specific ID
            # Looking for links to /wiki/
            person_links = soup.select('a[href*="wiki/"]')
            
            valid_links_found = 0
            for link in person_links:
                href = link.get('href')
                if 'wiki/' in href and 'index.html' not in href:
                    full_url = urljoin(self.BASE_URL, href)
                    yield full_url
                    valid_links_found += 1
            
            if valid_links_found == 0:
                logger.info("No more person links found. Ending listing crawl.")
                break
            
            page += 1
            self._sleep()

    def fetch_intervenciones_en_comision(self, id_persona):
        """
        Fetches 'Intervención en Comision' data via AJAX.
        """
        if not id_persona:
            return []

        params = {
            'id': '949', # Code for Intervención en Comision
            'id_persona': str(id_persona)
        }
        
        logger.info(f"Fetching intervenciones for id_persona {id_persona}...")
        try:
            response = self.session.get(self.AJAX_ENDPOINT, params=params)
            response.raise_for_status()
        except requests.RequestException as e:
            logger.error(f"Error fetching intervenciones: {e}")
            return []

        soup = BeautifulSoup(response.content, 'html.parser')
        registros = []
        
        # Parse the table
        rows = soup.select('tr')
        for row in rows:
            cols = row.find_all('td')
            if len(cols) >= 2:
                fecha_raw = cols[0].get_text(strip=True)
                titulo_cell = cols[1]
                titulo_text = titulo_cell.get_text(strip=True)
                
                # Check for link in title
                link = titulo_cell.find('a')
                url_detalle = link.get('href') if link else None
                if url_detalle and not url_detalle.startswith('http'):
                    url_detalle = urljoin(self.AJAX_ENDPOINT, url_detalle)

                fecha_iso = self.parse_spanish_date(fecha_raw)

                registros.append({
                    "fecha_texto": fecha_raw,
                    "fecha_iso": fecha_iso,
                    "titulo": titulo_text,
                    "url_detalle": url_detalle
                })
        
        return registros

    def extract_infobox_data(self, soup):
        """
        Extracts data from the side infobox (Datos Biográficos).
        """
        data = {
            "nombre_completo": None,
            "nombres": None,
            "apellido_paterno": None,
            "apellido_materno": None,
            "fecha_nacimiento": None,
            "fecha_nacimiento_texto": None,
            "lugar_nacimiento": None,
            "nacionalidad": None,
            "grado_academico": None,
            "profesion": None,
            "partido_principal": None,
            "imagen_ficha_url": None,
            "enlaces_externos": {}
        }

        # Image
        img = soup.select_one('.foto-parlamentario img')
        if img:
            data['imagen_ficha_url'] = urljoin(self.BASE_URL, img.get('src'))

        # Name
        # Try to extract from page title first as it's reliable
        # Format: "Reseña Biográfica Name Name - ..."
        if soup.title:
            title_text = soup.title.get_text(strip=True)
            match = re.search(r"Reseña Biográfica (.*?) -", title_text)
            if match:
                data['nombre_completo'] = match.group(1).strip()
        
        # Fallback to h1 if title extraction failed
        if not data['nombre_completo']:
            h1 = soup.find('h1')
            if h1:
                text = h1.get_text(strip=True)
                # Avoid generic headers
                if "Reseñas" not in text and "Trayectoria" not in text:
                    data['nombre_completo'] = text

        # Helper to parse name parts
        def parse_name(full_name):
            if not full_name: return
            parts = full_name.split()
            if len(parts) >= 3:
                data['apellido_materno'] = parts[-1]
                data['apellido_paterno'] = parts[-2]
                data['nombres'] = " ".join(parts[:-2])
            elif len(parts) == 2:
                data['apellido_paterno'] = parts[-1]
                data['nombres'] = parts[0]

        if data['nombre_completo']:
            parse_name(data['nombre_completo'])

        # Strategy: Look for tables that might contain the info
        # The infobox often has rows like <tr><td>Nombre:</td><td>Value</td></tr>
        for row in soup.find_all('tr'):
            cells = row.find_all(['td', 'th'])
            if len(cells) >= 2:
                label = cells[0].get_text(strip=True).rstrip(':')
                value = cells[1].get_text(strip=True)
                
                # Only update if not already found or if it looks valid
                if 'Nombre' in label and not data['nombre_completo']:
                    data['nombre_completo'] = value
                    parse_name(value)
                elif 'Nacimiento' in label:
                    data['fecha_nacimiento_texto'] = value
                    # Try to extract place if possible (often "Date Place")
                    # Regex for date
                    data['fecha_nacimiento'] = self.parse_spanish_date(value)
                    # Heuristic for place: text after date?
                    # "9 de Junio de 1977 Santiago"
                    date_match = re.search(r'(\d{1,2}\s+de\s+[a-zA-Z]+\s+de\s+\d{4})', value, re.IGNORECASE)
                    if date_match:
                        date_str = date_match.group(1)
                        place = value.replace(date_str, '').strip()
                        if place:
                            data['lugar_nacimiento'] = place
                elif 'Profesión' in label:
                    data['profesion'] = value
                elif 'Grado' in label:
                    data['grado_academico'] = value
                elif 'Partido' in label:
                    data['partido_principal'] = value

        # Fallback: Text search if table parsing didn't fill everything
        if not data['fecha_nacimiento']:
            birth_text = soup.find(string=re.compile(r"Nació en"))
            if birth_text:
                match = re.search(r"Nació en (.*?), el (.*?)(?:\.|$)", birth_text)
                if match:
                    data['lugar_nacimiento'] = match.group(1).strip()
                    data['fecha_nacimiento_texto'] = match.group(2).strip()
                    data['fecha_nacimiento'] = self.parse_spanish_date(data['fecha_nacimiento_texto'])

        # External links
        for link in soup.find_all('a', href=True):
            href = link['href']
            if 'senado.cl' in href and 'ficha' in href:
                data['enlaces_externos']['ficha_senado'] = href
            elif 'camara.cl' in href and 'diputado' in href:
                data['enlaces_externos']['ficha_camara'] = href
            elif 'wikipedia.org' in href:
                data['enlaces_externos']['wikipedia'] = href

        return data

    def extract_id_persona(self, soup):
        """
        Extracts the internal ID used for AJAX calls.
        """
        # Method 1: Look for links with idPersona parameter
        # Example: .../descargar-participaciones-xls?idPersona=4030...
        for link in soup.find_all('a', href=True):
            href = link['href']
            match = re.search(r'idPersona=(\d+)', href)
            if match:
                return match.group(1)

        # Method 2: Look for hidden input
        hidden_input = soup.find('input', {'name': 'id_persona'})
        if hidden_input:
            return hidden_input.get('value')
        
        # Method 3: Look for JS variable
        scripts = soup.find_all('script')
        for script in scripts:
            if script.string:
                match = re.search(r'id_persona\s*=\s*["\']?(\d+)["\']?', script.string)
                if match:
                    return match.group(1)
        
        # Method 4: Search in the whole HTML text for "id_persona="
        match = re.search(r'id_persona=(\d+)', str(soup))
        if match:
            return match.group(1)

        return None

    def parse_person_page(self, url):
        """
        Parses a single person's detail page.
        """
        logger.info(f"Parsing person page: {url}")
        try:
            response = self.session.get(url)
            response.raise_for_status()
        except requests.RequestException as e:
            logger.error(f"Error fetching person page {url}: {e}")
            return None

        soup = BeautifulSoup(response.content, 'html.parser')
        
        # Extract ID from URL or content
        id_bcn = url.split('/')[-1]
        id_persona = self.extract_id_persona(soup)
        
        if not id_persona:
            logger.warning(f"Could not find id_persona for {url}")

        # Metadata
        metadatos = {
            "titulo_pagina": soup.title.string if soup.title else None,
            "descripcion": None,
            "anio_version": 2020
        }
        meta_desc = soup.find('meta', attrs={'name': 'DC.description'})
        if meta_desc:
            metadatos['descripcion'] = meta_desc.get('content')

        # Antecedentes Personales
        antecedentes = self.extract_infobox_data(soup)
        
        # Biografia Sections
        biografia = {
            "familia_y_juventud": None,
            "estudios_y_vida_laboral": None,
            "trayectoria_politica_y_publica": None,
            "reconocimientos": []
        }
        
        def get_section_text(header_text):
            # Find header containing the text (case insensitive)
            header = soup.find(lambda tag: tag.name in ['h2', 'h3', 'h4'] and header_text.lower() in tag.get_text().lower())
            if header:
                content = []
                # Iterate siblings until next header
                for sibling in header.find_next_siblings():
                    if sibling.name in ['h2', 'h3', 'h4']:
                        break
                    
                    text = ""
                    if sibling.name == 'p':
                        text = sibling.get_text(strip=True)
                    elif sibling.name == 'ul':
                        for li in sibling.find_all('li'):
                            content.append(f"- {li.get_text(strip=True)}")
                        continue
                    elif sibling.name is None: # Text node
                        text = str(sibling).strip()
                    
                    if text:
                        content.append(text)
                return "\n\n".join(content)
            return None

        biografia['familia_y_juventud'] = get_section_text("Familia y juventud")
        biografia['estudios_y_vida_laboral'] = get_section_text("Estudios y vida laboral")
        biografia['trayectoria_politica_y_publica'] = get_section_text("Trayectoria política y pública")
        
        # Labor Parlamentaria - Intervenciones
        intervenciones = []
        if id_persona:
            intervenciones = self.fetch_intervenciones_en_comision(id_persona)
            self._sleep()

        # Construct final object
        person_data = {
            "id_bcn": id_bcn,
            "id_persona": int(id_persona) if id_persona else None,
            "url_canonica": url,
            "metadatos": metadatos,
            "antecedentes_personales": antecedentes,
            "trayectoria_parlamentaria": [], # Placeholder
            "cargos_publicos": [], # Placeholder
            "biografia": biografia,
            "legislaturas": [], # Placeholder
            "labor_parlamentaria": {
                "intervencion_en_comision": {
                    "endpoint": f"getParticipaciones.html?id=949&id_persona={id_persona}" if id_persona else None,
                    "registros": intervenciones
                }
            },
            "notas": []
        }
        
        return person_data

    def run(self, period="1990-2018", limit=None):
        """
        Main execution method.
        """
        logger.info(f"Starting scrape for period {period}")
        
        urls = list(self.get_listing_urls(period))
        logger.info(f"Found {len(urls)} profiles to scrape.")
        
        if limit:
            urls = urls[:limit]
            logger.info(f"Limiting to first {limit} profiles.")
        
        results = []
        for i, url in enumerate(urls):
            logger.info(f"Processing {i+1}/{len(urls)}: {url}")
            person_data = self.parse_person_page(url)
            if person_data:
                results.append(person_data)
            self._sleep()
            
            if (i + 1) % 10 == 0:
                self.save_json(results)

        self.save_json(results)
        logger.info("Scraping completed.")

    def save_json(self, data):
        with open(self.output_file, 'w', encoding='utf-8') as f:
            json.dump(data, f, ensure_ascii=False, indent=2)
        logger.info(f"Saved {len(data)} records to {self.output_file}")

if __name__ == "__main__":
    scraper = BCNBiographyScraper()
    # Use a known valid period
    scraper.run(period="1990-2018")

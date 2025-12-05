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
        logging.FileHandler("scraper_v2.log"),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger(__name__)

class BCNBiographyScraperV2:
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

        # Manual mapping for Spanish months
        months = {
            'enero': '01', 'febrero': '02', 'marzo': '03', 'abril': '04',
            'mayo': '05', 'junio': '06', 'julio': '07', 'agosto': '08',
            'septiembre': '09', 'octubre': '10', 'noviembre': '11', 'diciembre': '12'
        }
        
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
        
        rows = soup.select('tr')
        for row in rows:
            cols = row.find_all('td')
            if len(cols) >= 2:
                fecha_raw = cols[0].get_text(strip=True)
                titulo_cell = cols[1]
                titulo_text = titulo_cell.get_text(strip=True)
                
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

    def extract_id_persona(self, soup):
        """
        Extracts the internal ID used for AJAX calls.
        """
        # Method 1: Look for links with idPersona parameter
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

    def extract_trayectoria_parlamentaria(self, soup):
        """
        Extracts parliamentary career from the side table.
        Looks for rows with rel="bcnbio:hasParliamentaryAppointment".
        """
        trayectoria = []
        
        # Find all rows that represent a parliamentary appointment
        rows = soup.find_all('tr', attrs={'rel': 'bcnbio:hasParliamentaryAppointment'})
        
        for row in rows:
            item = {
                "cargo": None,
                "anio_inicio": None,
                "anio_termino": None,
                "distrito_o_circunscripcion": None,
                "partido": None,
                "precedido_por": None,
                "sucedido_por": None
            }
            
            # The main cell usually has class "trayectoria_align" or similar
            cell = row.find('td')
            if not cell:
                continue
                
            # Cargo and Years
            # Usually in a div with bold text: "Diputado 2010-2014"
            # Or structured with spans
            
            # Try to find the cargo text (first bold div or text node)
            # The structure provided:
            # <div style="font-weight:bold; font-size:12px;">Diputado <span property="time:hasBeginning">2010</span>...</div>
            
            # Extract years
            start_span = cell.find('span', property='time:hasBeginning')
            end_span = cell.find('span', property='time:hasEnd')
            
            if start_span:
                item['anio_inicio'] = int(start_span.get_text(strip=True))
            if end_span:
                item['anio_termino'] = int(end_span.get_text(strip=True))
                
            # Extract Cargo
            # It's often the text before the years in that same div
            # Let's look for the div containing the years
            if start_span and start_span.parent:
                parent_text = start_span.parent.get_text(strip=True)
                # Remove years to get cargo
                # "Diputado 2010-2014" -> "Diputado"
                # Simple heuristic: take the first word or everything before the first digit
                match = re.match(r'^([^\d]+)', parent_text)
                if match:
                    item['cargo'] = match.group(1).strip()
            
            # District / Constituency
            place_span = cell.find('span', property='bcnbio:representingPlaceNamed')
            if place_span:
                item['distrito_o_circunscripcion'] = place_span.get_text(strip=True)
            
            # Party
            party_span = cell.find('span', rel='bcnbio:hasPoliticalParty')
            if party_span:
                party_link = party_span.find('a')
                if party_link:
                    item['partido'] = party_link.get_text(strip=True)
                else:
                    item['partido'] = party_span.get_text(strip=True)
            
            # Predecessor / Successor
            # Look for divs containing "Precedido por:" or "Sucedido por:"
            divs = cell.find_all('div')
            for div in divs:
                text = div.get_text(strip=True)
                if "Precedido por:" in text:
                    link = div.find('a')
                    if link:
                        item['precedido_por'] = link.get_text(strip=True)
                if "Sucedido por:" in text:
                    link = div.find('a')
                    if link:
                        item['sucedido_por'] = link.get_text(strip=True)
            
            trayectoria.append(item)
            
        return trayectoria

    def extract_antecedentes_personales(self, soup):
        """
        Extracts personal details from the side table and image.
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
        # Try multiple selectors for the image
        img = soup.select_one('.foto-parlamentario img')
        if not img:
            img = soup.select_one('.foto-ficha img') # Alternative class
        
        if img:
            src = img.get('src')
            if src:
                data['imagen_ficha_url'] = urljoin(self.BASE_URL, src)

        # Name from Title
        if soup.title:
            title_text = soup.title.get_text(strip=True)
            match = re.search(r"Reseña Biográfica (.*?) -", title_text)
            if match:
                data['nombre_completo'] = match.group(1).strip()
        
        if not data['nombre_completo']:
            h1 = soup.find('h1')
            if h1:
                text = h1.get_text(strip=True)
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

        # Find the "Antecedentes Personales" header to locate the correct table section
        # We iterate through all rows in the document to find key-value pairs
        # This is more robust than finding a specific table index
        
        for row in soup.find_all('tr'):
            cells = row.find_all(['td', 'th'])
            if len(cells) >= 2:
                # Clean label: remove colons, strip whitespace
                label = cells[0].get_text(strip=True).rstrip(':').lower()
                value = cells[1].get_text(strip=True)
                
                if 'nombre' in label and not data['nombre_completo']:
                    data['nombre_completo'] = value
                    parse_name(value)
                elif 'nacimiento' in label:
                    data['fecha_nacimiento_texto'] = value
                    data['fecha_nacimiento'] = self.parse_spanish_date(value)
                    # Extract place
                    date_match = re.search(r'(\d{1,2}\s+de\s+[a-zA-Z]+\s+de\s+\d{4})', value, re.IGNORECASE)
                    if date_match:
                        date_str = date_match.group(1)
                        place = value.replace(date_str, '').strip()
                        if place:
                            data['lugar_nacimiento'] = place
                elif 'profesión' in label:
                    data['profesion'] = value
                elif 'grado' in label or 'académico' in label:
                    data['grado_academico'] = value
                elif 'nacionalidad' in label:
                    data['nacionalidad'] = value
                elif 'partido' in label or 'militancia' in label:
                    data['partido_principal'] = value
                elif 'estado civil' in label:
                    # Optional field, but good to have if needed
                    pass

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

    def extract_biografia(self, soup):
        """
        Extracts biography sections and intro.
        """
        biografia = {
            "intro": None,
            "familia_y_juventud": None,
            "estudios_y_vida_laboral": None,
            "trayectoria_politica_y_publica": None,
            "reconocimientos": []
        }

        # Intro
        intro_div = soup.find('div', class_='intro_wiki')
        if intro_div:
            biografia['intro'] = intro_div.get_text(strip=True)

        # Main sections
        # Look for h3 "Reseña biográfica"
        resena_header = soup.find('h3', string=re.compile(r'Reseña biográfica', re.IGNORECASE))
        
        if resena_header:
            # Iterate over siblings to find div.box_contenidos
            # Or just find all div.box_contenidos inside the parent container
            # Assuming structure: h3 -> div.box_contenidos -> h4, div
            
            # Let's look for all box_contenidos that follow the header
            # Or better, look for h4s inside the container
            
            container = resena_header.parent
            if container:
                boxes = container.find_all('div', class_='box_contenidos')
                for box in boxes:
                    h4 = box.find('h4')
                    if not h4:
                        continue
                    
                    title = h4.get_text(strip=True).lower()
                    
                    # Get content div (usually next sibling of h4 or inside box)
                    # The structure is usually h4 then a div with paragraphs
                    content_div = box.find('div')
                    if not content_div:
                        # Maybe paragraphs are direct children?
                        content_div = box
                    
                    # Extract text from paragraphs, removing footnotes
                    paragraphs = []
                    for p in content_div.find_all('p'):
                        # Remove sup tags (footnotes)
                        for sup in p.find_all('sup'):
                            sup.decompose()
                        text = p.get_text(strip=True)
                        if text:
                            paragraphs.append(text)
                    
                    full_text = "\n\n".join(paragraphs)
                    
                    if 'familia' in title and 'juventud' in title:
                        biografia['familia_y_juventud'] = full_text
                    elif 'estudios' in title and 'laboral' in title:
                        biografia['estudios_y_vida_laboral'] = full_text
                    elif 'trayectoria' in title and 'política' in title:
                        biografia['trayectoria_politica_y_publica'] = full_text
                    elif 'reconocimientos' in title:
                        # Split by newlines or keep as text?
                        # User asked for array in schema, but text in prompt description
                        # Let's try to split if it looks like a list
                        if full_text:
                            biografia['reconocimientos'].append(full_text)

        return biografia

    def extract_legislaturas(self, soup):
        """
        Extracts legislative periods and commissions.
        """
        legislaturas = []
        
        container = soup.find('div', id='parlamentario_hemiciclo')
        if not container:
            return legislaturas
            
        # Iterate over direct children divs
        # Each div usually represents a period
        for div in container.find_all('div', recursive=False):
            h4 = div.find('h4')
            if not h4:
                continue
                
            periodo_text = h4.get_text(strip=True).replace('Legislatura', '').strip()
            
            # Get all paragraphs
            paragraphs = []
            for p in div.find_all('p'):
                text = p.get_text(strip=True)
                if text:
                    paragraphs.append(text)
            
            full_text = "\n\n".join(paragraphs)
            
            # Extract commissions
            comisiones_permanentes = []
            comisiones_especiales = []
            
            # Heuristic extraction
            # "Integró las comisiones permanentes de X, Y y Z."
            # "Integrante de las comisiones permanentes de ..."
            
            # Normalize text for searching
            text_lower = full_text.lower()
            
            # Regex for permanent commissions
            # Look for "comisiones permanentes de" followed by content until a period
            # Improved regex to capture until the end of the sentence
            # Also handling singular "la comisión permanente de" and "continuó en"
            perm_match = re.search(r'(?:integró|integrante de|participó en|continuó en)\s+(?:las?|la)\s+comisi(?:ones|ón)\s+permanente(?:s)?\s+de\s+(.*?)(?:\.|$)', full_text, re.IGNORECASE | re.DOTALL)
            if perm_match:
                comm_text = perm_match.group(1)
                
                # Split by semicolons as requested by user to avoid splitting complex commission names
                parts = comm_text.split(';')
                
                cleaned_parts = []
                for p in parts:
                    p = p.strip()
                    # Remove leading 'y ' if present (e.g. "; y Conducta Parlamentaria")
                    if p.lower().startswith('y '):
                        p = p[2:].strip()
                    
                    # Filter out long sentences that might have been captured by mistake
                    if p and len(p) < 150:
                        cleaned_parts.append(p)
                
                comisiones_permanentes = cleaned_parts

            # Regex for special commissions
            # "Comisión Especial de ..." or "Comisiones Especiales de ..."
            # Capture the full name of the commission
            # Heuristic: "Comisión Especial X" until punctuation or connector
            special_matches = re.findall(r'(Comisión Especial [^,.;]+)', full_text)
            if special_matches:
                comisiones_especiales = [m.strip() for m in special_matches]

            legislaturas.append({
                "periodo": periodo_text,
                "descripcion": paragraphs[0] if paragraphs else None,
                "comisiones_permanentes": comisiones_permanentes,
                "comisiones_especiales": comisiones_especiales,
                "texto_crudo": full_text
            })
            
        return legislaturas

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

        # Extract all sections
        antecedentes = self.extract_antecedentes_personales(soup)
        trayectoria = self.extract_trayectoria_parlamentaria(soup)
        biografia = self.extract_biografia(soup)
        legislaturas = self.extract_legislaturas(soup)
        
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
            "trayectoria_parlamentaria": trayectoria,
            "cargos_publicos": [], # Not implemented yet
            "biografia": biografia,
            "legislaturas": legislaturas,
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
    scraper = BCNBiographyScraperV2()
    # Run for the requested period
    scraper.run(period="1990-2018")

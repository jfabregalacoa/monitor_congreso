import pandas as pd
import logging
from tqdm import tqdm
from typing import List, Dict, Any, Optional
import os
import sys
import urllib.parse

# Add src to path
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
from src.utils import setup_logging, fetch_content, parse_xml, parse_html, clean_text

# Caches
boletin_to_project_id: Dict[str, str] = {}
project_urgencies: Dict[str, List[Dict[str, Any]]] = {}
vote_to_boletin: Dict[str, str] = {}

def fetch_boletin_for_vote(vote_id: str) -> str:
    """
    Quickly fetches just the bulletin for a vote ID.
    """
    if vote_id in vote_to_boletin:
        return vote_to_boletin[vote_id]
        
    url = f"https://opendata.camara.cl/camaradiputados/pages/legislativo/retornarVotacionDetalle.aspx?prmID={vote_id}"
    content = fetch_content(url)
    if not content:
        return ""

    root = parse_xml(content)
    if root is None:
        return ""
        
    # Find Boletin
    found = root.find(".//{*}Boletin")
    if found is None:
        found = root.find("Boletin")
    
    boletin = clean_text(found.text) if found is not None else ""
    if boletin:
        vote_to_boletin[vote_id] = boletin
    return boletin

def fetch_proyecto_id_from_tramitacion(boletin: str) -> str:
    """
    Scrapes the tramitacion page to find the internal project ID (prmID) for urgencies.
    """
    if not boletin:
        return ""
    if boletin in boletin_to_project_id:
        return boletin_to_project_id[boletin]

    url = f"https://www.camara.cl/legislacion/ProyectosDeLey/tramitacion.aspx?prmBoletin={boletin}"
    content = fetch_content(url)
    if not content:
        return ""

    soup = parse_html(content)
    if not soup:
        return ""

    # Look for the link to urgencias.aspx
    # <a href="urgencias.aspx?prmID=XXXX&prmBOLETIN=...">
    
    link = soup.find('a', href=lambda x: x and 'urgencias.aspx' in x)
    if link:
        href = link.get('href')
        parsed = urllib.parse.urlparse(href)
        qs = urllib.parse.parse_qs(parsed.query)
        prmID = qs.get('prmID', [''])[0]
        
        if prmID:
            boletin_to_project_id[boletin] = prmID
            return prmID
            
    return ""

def fetch_urgencias(project_id: str, boletin: str) -> List[Dict[str, Any]]:
    """
    Scrapes the urgencies table.
    """
    if not project_id:
        return []
        
    cache_key = f"{project_id}_{boletin}"
    if cache_key in project_urgencies:
        return project_urgencies[cache_key]

    url = f"https://www.camara.cl/legislacion/ProyectosDeLey/urgencias.aspx?prmID={project_id}&prmBOLETIN={boletin}"
    content = fetch_content(url)
    if not content:
        return []

    soup = parse_html(content)
    if not soup:
        return []

    urgencies = []
    # Find the table "Urgencias"
    # Usually a table with specific headers or class.
    # We'll look for a table that contains "Fecha Inicio" or "Tipo de urgencia"
    
    tables = soup.find_all('table')
    target_table = None
    
    for table in tables:
        if "Fecha Inicio" in table.get_text() or "Tipo de urgencia" in table.get_text():
            target_table = table
            break
            
    if target_table:
        rows = target_table.find_all('tr')
        # Assuming first row is header
        for row in rows[1:]:
            cols = row.find_all('td')
            if len(cols) < 5: # Expecting at least 5-6 columns
                continue
                
            # Mapping based on typical order:
            # Fecha Inicio | Fecha término | Tipo | N° Oficio | N° Mensaje ingreso | N° Mensaje retiro
            
            u_data = {
                'urgencia_fecha_inicio': clean_text(cols[0].get_text()),
                'urgencia_fecha_termino': clean_text(cols[1].get_text()),
                'urgencia_tipo': clean_text(cols[2].get_text()),
                'urgencia_oficio': clean_text(cols[3].get_text()),
                'urgencia_mensaje_ing': clean_text(cols[4].get_text()),
                'urgencia_mensaje_ret': clean_text(cols[5].get_text()) if len(cols) > 5 else ""
            }
            urgencies.append(u_data)

    project_urgencies[cache_key] = urgencies
    return urgencies

def main():
    setup_logging()
    logging.info("Starting Script 2: Fetch Urgencias")

    # Define paths relative to the script location
    script_dir = os.path.dirname(os.path.abspath(__file__))
    project_root = os.path.dirname(os.path.dirname(script_dir))
    
    input_path = os.path.join(project_root, 'Harvard Dataverse/Roll calls/matriz__periodo_2022_26.csv')
    output_path = os.path.join(script_dir, '../outputs/votaciones_urgencias_2022_26.csv')

    if not os.path.exists(input_path):
        logging.error(f"Input file not found: {input_path}")
        return

    try:
        df = pd.read_csv(input_path)
    except Exception as e:
        logging.error(f"Error reading CSV: {e}")
        return

    vote_ids = [col for col in df.columns if col.isdigit()]
    logging.info(f"Found {len(vote_ids)} vote IDs to process.")

    results = []

    # Optimization: First find unique bulletins for all votes to minimize requests
    # But we need to map vote -> boletin first.
    # We can do it lazily in the loop.

    for vote_id in tqdm(vote_ids, desc="Processing Urgencies"):
        # 1. Get Boletin
        boletin = fetch_boletin_for_vote(vote_id)
        if not boletin:
            continue
            
        # 2. Get Project ID
        project_id = fetch_proyecto_id_from_tramitacion(boletin)
        if not project_id:
            logging.warning(f"Could not find project ID for boletin {boletin}")
            continue
            
        # 3. Get Urgencies
        urgency_list = fetch_urgencias(project_id, boletin)
        
        # 4. Expand rows (one vote might have multiple urgencies associated with its project)
        # The user wants a CSV with vote_id and urgency details.
        # If a project has 5 urgencies, do we repeat the vote_id 5 times?
        # Usually yes, to have a flat table.
        
        if urgency_list:
            for urg in urgency_list:
                row = {
                    'vote_id': vote_id,
                    'boletin': boletin,
                    'proyecto_id': project_id,
                    **urg
                }
                results.append(row)
        else:
            # No urgencies found, but we still record the vote/project link?
            # Or skip? User asked for "votaciones_urgencias", implying the join.
            # I'll add a row with empty urgency fields to preserve the vote info.
            row = {
                'vote_id': vote_id,
                'boletin': boletin,
                'proyecto_id': project_id,
                'urgencia_fecha_inicio': "",
                'urgencia_fecha_termino': "",
                'urgencia_tipo': "",
                'urgencia_oficio': "",
                'urgencia_mensaje_ing': "",
                'urgencia_mensaje_ret': ""
            }
            results.append(row)

    if results:
        out_df = pd.DataFrame(results)
        out_df.to_csv(output_path, index=False)
        logging.info(f"Saved urgencies to {output_path}")
    else:
        logging.warning("No results to save.")

if __name__ == "__main__":
    main()

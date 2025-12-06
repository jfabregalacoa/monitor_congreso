import pandas as pd
import logging
import json
import os
import sys
import glob
import re
from tqdm import tqdm
from typing import List, Dict, Any, Optional

# Add src to path to import utils
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
from src.utils import setup_logging, fetch_content, parse_xml, clean_text

# Cache to avoid re-fetching the same bulletin info across different votes/files
bulletin_cache: Dict[str, Dict[str, Any]] = {}

def fetch_vote_details(vote_id: str) -> Optional[Dict[str, Any]]:
    """
    Fetches details for a specific vote ID from the Camara XML API.
    Uses the Web Service URL (WSLegislativo.asmx) which returns valid XML.
    """
    url = f"https://opendata.camara.cl/camaradiputados/WServices/WSLegislativo.asmx/retornarVotacionDetalle?prmVotacionId={vote_id}"
    content = fetch_content(url)
    if not content:
        return None

    root = parse_xml(content)
    if root is None:
        return None

    # Helper to find text safely, ignoring namespaces
    def get_text(element, tag):
        if element is None:
            return ""
        # Search for tag in children (handling namespaces)
        # Try direct child first with namespace wildcard
        found = element.find(f"{{*}}{tag}")
        if found is None:
            # Try recursive
            found = element.find(f".//{{*}}{tag}")
        if found is None:
            # Try without namespace (if parser stripped it or it's absent)
            found = element.find(tag)
        return clean_text(found.text) if found is not None else ""

    descripcion_full = get_text(root, 'Descripcion')
    
    data = {
        'vote_id': get_text(root, 'Id'),
        'boletin_full': descripcion_full,
        'fecha': get_text(root, 'Fecha'),
        'quorum': get_text(root, 'Quorum'),
        'resultado': get_text(root, 'Resultado'),
        'tipo': get_text(root, 'Tipo')
    }
    
    return data

def parse_boletin_number(boletin_str: str) -> str:
    """
    Extracts the bulletin number from the description string.
    Example: "Boletín N° 6639-25" -> "6639"
    """
    if not boletin_str:
        return ""
    
    # Usually format is "Boletín N° XXXXX-YY"
    # We want XXXXX.
    match = re.search(r'(\d+)-(\d+)', boletin_str)
    if match:
        return match.group(1) # Return the first part (e.g., 6639)
    
    return ""

def fetch_senado_project_details(boletin_number: str) -> Dict[str, Any]:
    """
    Fetches project details from Senado Tramitacion API.
    Source 2: https://tramitacion.senado.cl/wspublico/tramitacion.php?boletin={NUMBER}
    """
    if not boletin_number:
        return {}
        
    if boletin_number in bulletin_cache:
        return bulletin_cache[boletin_number]

    url = f"https://tramitacion.senado.cl/wspublico/tramitacion.php?boletin={boletin_number}"
    content = fetch_content(url)
    if not content:
        return {}

    root = parse_xml(content)
    if root is None:
        return {}

    data = {}
    
    # Extract everything from <descripcion>
    desc_node = root.find(".//descripcion")
    if desc_node is not None:
        for child in desc_node:
            tag = child.tag
            # Remove namespace if present in tag name
            if '}' in tag:
                tag = tag.split('}', 1)[1]
            data[f"desc_{tag}"] = clean_text(child.text)

    # Extract everything from <materias>
    materias_node = root.find(".//materias")
    materias_list = []
    if materias_node is not None:
        for materia in materias_node.findall(".//materia"):
            desc = materia.find("DESCRIPCION")
            if desc is not None:
                materias_list.append(clean_text(desc.text))
    
    data['materias'] = " | ".join(materias_list)

    bulletin_cache[boletin_number] = data
    return data

def process_file(input_path: str, output_path: str):
    logging.info(f"Processing file: {input_path}")
    
    try:
        df = pd.read_csv(input_path)
    except Exception as e:
        logging.error(f"Error reading CSV {input_path}: {e}")
        return

    # Extract vote IDs (numeric columns)
    vote_ids = [col for col in df.columns if col.isdigit()]
    logging.info(f"Found {len(vote_ids)} vote IDs in {os.path.basename(input_path)}")

    results = []

    for vote_id in tqdm(vote_ids, desc=f"Votes in {os.path.basename(input_path)}"):
        # Step 1: Fetch Vote Details
        vote_data = fetch_vote_details(vote_id)
        if not vote_data:
            logging.warning(f"Could not fetch details for vote_id {vote_id}")
            results.append({'vote_id': vote_id, 'error': 'Fetch failed'})
            continue
            
        # Step 2: Parse Boletin Number
        boletin_full = vote_data.get('boletin_full', '')
        boletin_number = parse_boletin_number(boletin_full)
        
        # Step 3: Fetch Project Details
        project_data = {}
        if boletin_number:
            project_data = fetch_senado_project_details(boletin_number)
        
        # Combine data
        row = {**vote_data, 'boletin_numero': boletin_number, **project_data}
        results.append(row)

    # Save results as JSON
    try:
        with open(output_path, 'w', encoding='utf-8') as f:
            json.dump(results, f, ensure_ascii=False, indent=4)
        logging.info(f"Saved details to {output_path}")
    except Exception as e:
        logging.error(f"Error saving JSON {output_path}: {e}")

def main():
    setup_logging()
    logging.info("Starting Script: Fetch Roll Calls Details (Periods 2022_26 onwards)")

    # Define paths
    script_dir = os.path.dirname(os.path.abspath(__file__))
    project_root = os.path.dirname(os.path.dirname(script_dir))
    
    input_dir = os.path.join(project_root, 'Harvard Dataverse/Roll calls')
    output_dir = os.path.join(script_dir, '../outputs')
    
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)

    # Find all matching CSV files
    pattern = os.path.join(input_dir, 'matriz__periodo_*.csv')
    all_files = glob.glob(pattern)
    
    # Filter to only include periods from 2022_26 onwards
    files = []
    for f in all_files:
        filename = os.path.basename(f)
        # Extract year and period from filename (e.g., matriz__periodo_2022_26.csv)
        match = re.search(r'periodo_(\d{4})_(\d+)', filename)
        if match:
            year = int(match.group(1))
            period = int(match.group(2))
            # Include if year > 2022, or if year == 2022 and period >= 26
            if year > 2022 or (year == 2022 and period >= 26):
                files.append(f)
    
    files.sort()  # Process in chronological order
    
    if not files:
        logging.error(f"No files found matching pattern: {pattern} for periods >= 2022_26")
        return
        
    logging.info(f"Found {len(files)} files to process (2022_26 onwards).")

    for input_path in files:
        filename = os.path.basename(input_path)
        # Create output filename: details_periodo_20XX_XX.json
        # Input: matriz__periodo_2002_06.csv
        # Output: details_periodo_2002_06.json
        output_filename = filename.replace('matriz__', 'details__').replace('.csv', '.json')
        output_path = os.path.join(output_dir, output_filename)
        
        process_file(input_path, output_path)

if __name__ == "__main__":
    main()
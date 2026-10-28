import pandas as pd
import logging
from tqdm import tqdm
from typing import List, Dict, Any, Optional
import os
import sys
import lxml.etree as ET

# Add src to path to import utils
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
from src.utils import setup_logging, fetch_content, parse_xml, clean_text

# Cache to avoid re-fetching the same bulletin info
bulletin_cache: Dict[str, Dict[str, Any]] = {}

def fetch_vote_details(vote_id: str) -> Optional[Dict[str, Any]]:
    """
    Fetches details for a specific vote ID from the Camara XML API.
    Source 1: https://opendata.camara.cl/camaradiputados/pages/legislativo/retornarVotacionDetalle.aspx
    """
    url = f"https://opendata.camara.cl/camaradiputados/pages/legislativo/retornarVotacionDetalle.aspx?prmVotacionId={vote_id}"
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
        found = element.find(f".//{{*}}{tag}")
        if found is None:
            found = element.find(tag)
        return clean_text(found.text) if found is not None else ""

    # Extract fields requested: ID - boletin - fecha - quorum - resultado - tipo
    # Note: 'Descripcion' in XML usually contains the Boletin string like "Boletín N° 6639-25"
    
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
    # Strategy: Find the part that looks like a bulletin number (digits-digits)
    # Or just look for "Boletín N° " and take the rest, then split by '-'
    
    import re
    # Look for pattern like "Boletín N° 6639-25" or just "6639-25"
    # Sometimes it might be just text.
    
    # Try to find the pattern "digits-digits"
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
    # Structure: <proyectos><proyecto><descripcion>...</descripcion>...</proyecto></proyectos>
    
    # Find <descripcion>
    # We use xpath or find to locate it deep in the tree
    # Namespaces might be absent here based on the user example
    
    # Try to find 'descripcion' anywhere
    desc_node = root.find(".//descripcion")
    if desc_node is not None:
        for child in desc_node:
            tag = child.tag
            # Remove namespace if present in tag name (though lxml usually handles it)
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

def main():
    setup_logging()
    logging.info("Starting Script: Fetch Details and Urgencies (Combined)")

    # Define paths
    script_dir = os.path.dirname(os.path.abspath(__file__))
    project_root = os.path.dirname(os.path.dirname(script_dir))
    
    input_path = os.path.join(project_root, 'Harvard Dataverse/Roll calls/matriz__periodo_2022_26.csv')
    output_path = os.path.join(script_dir, '../outputs/votaciones_detalle_senado_2022_26.csv')

    if not os.path.exists(input_path):
        logging.error(f"Input file not found: {input_path}")
        return

    try:
        df = pd.read_csv(input_path)
    except Exception as e:
        logging.error(f"Error reading CSV: {e}")
        return

    # Extract vote IDs (numeric columns)
    vote_ids = [col for col in df.columns if col.isdigit()]
    logging.info(f"Found {len(vote_ids)} vote IDs to process.")

    results = []

    for vote_id in tqdm(vote_ids, desc="Processing Votes"):
        # Step 1: Fetch Vote Details (Source 1)
        vote_data = fetch_vote_details(vote_id)
        if not vote_data:
            logging.warning(f"Could not fetch details for vote_id {vote_id}")
            # Add a placeholder row to keep track? Or skip?
            # Better to skip or add partial data. I'll add partial.
            results.append({'vote_id': vote_id, 'error': 'Fetch failed'})
            continue
            
        # Step 2: Parse Boletin Number
        boletin_full = vote_data.get('boletin_full', '')
        boletin_number = parse_boletin_number(boletin_full)
        
        # Step 3: Fetch Project Details (Source 2)
        project_data = {}
        if boletin_number:
            project_data = fetch_senado_project_details(boletin_number)
        
        # Combine data
        row = {**vote_data, 'boletin_numero': boletin_number, **project_data}
        results.append(row)

    # Save results
    if results:
        out_df = pd.DataFrame(results)
        out_df.to_csv(output_path, index=False)
        logging.info(f"Saved combined details to {output_path}")
    else:
        logging.warning("No results to save.")

if __name__ == "__main__":
    main()

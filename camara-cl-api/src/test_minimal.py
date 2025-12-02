import sys
import os
import logging
import lxml.etree as ET

# Add src to path to import utils
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
from src.utils import setup_logging, fetch_content, parse_xml, clean_text
from src.fetch_details_and_urgencies import fetch_vote_details, parse_boletin_number, fetch_senado_project_details

# Setup logging to console
logging.basicConfig(level=logging.INFO, handlers=[logging.StreamHandler()])

def debug_xml_structure(vote_id):
    # Corrected URL to point to the Web Service, not the page
    url = f"https://opendata.camara.cl/camaradiputados/WServices/WSLegislativo.asmx/retornarVotacionDetalle?prmVotacionId={vote_id}"
    print(f"Fetching URL: {url}")
    content = fetch_content(url)
    if not content:
        print("No content fetched.")
        return

    print(f"Content length: {len(content)}")
    # Print first 500 chars
    print(f"First 500 chars: {content[:500]}")

    root = parse_xml(content)
    if root is None:
        print("Failed to parse XML.")
        return

    print(f"Root tag: {root.tag}")
    print("Children tags:")
    for child in root:
        print(f" - {child.tag}: {child.text}")

def fetch_vote_details_corrected(vote_id: str):
    """
    Fetches details for a specific vote ID from the Camara XML API.
    Corrected URL to point to the Web Service.
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

def test_single_vote(vote_id):
    print(f"--- Testing Vote ID: {vote_id} ---")
    
    # Debug XML first
    debug_xml_structure(vote_id)
    
    # 1. Fetch Vote Details
    print("\nFetching vote details using CORRECTED function...")
    vote_data = fetch_vote_details_corrected(vote_id)
    print(f"Vote Data: {vote_data}")
    
    if not vote_data:
        print("Failed to fetch vote data.")
        return

    # 2. Parse Boletin
    boletin_full = vote_data.get('boletin_full', '')
    boletin_number = parse_boletin_number(boletin_full)
    print(f"Boletin Full: '{boletin_full}' -> Parsed Number: '{boletin_number}'")

    # 3. Fetch Project Details
    if boletin_number:
        print(f"Fetching project details for boletin {boletin_number}...")
        project_data = fetch_senado_project_details(boletin_number)
        print(f"Project Data: {project_data}")
    else:
        print("No boletin number found, skipping project details.")

if __name__ == "__main__":
    # Test with the ID from the example
    test_single_vote('32713')

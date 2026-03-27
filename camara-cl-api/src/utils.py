import requests
import time
import logging
from bs4 import BeautifulSoup
import lxml.etree as ET
from typing import Optional, Union, Dict, Any

# Configure logging
def setup_logging(log_file: str = None):
    if log_file is None:
        # Default to outputs/logs.txt relative to the project root
        import os
        current_file = os.path.abspath(__file__)
        script_dir = os.path.dirname(current_file)
        log_file = os.path.join(script_dir, '../outputs/logs.txt')
    
    # Ensure output directory exists
    os.makedirs(os.path.dirname(log_file), exist_ok=True)
    
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(levelname)s - %(message)s',
        handlers=[
            logging.FileHandler(log_file),
            logging.StreamHandler()
        ]
    )

def fetch_content(url: str, retries: int = 3, sleep_time: float = 0.5) -> Optional[Union[bytes, str]]:
    """
    Fetches content from a URL with retries and sleep.
    """
    for attempt in range(retries):
        try:
            time.sleep(sleep_time)
            response = requests.get(url, timeout=10)
            response.raise_for_status()
            return response.content
        except requests.RequestException as e:
            logging.warning(f"Attempt {attempt + 1} failed for {url}: {e}")
            time.sleep(sleep_time * 2)
    
    logging.error(f"Failed to fetch {url} after {retries} attempts.")
    return None

def parse_xml(content: bytes) -> Optional[ET.Element]:
    """
    Parses XML content using lxml.
    """
    try:
        # Replace &nbsp; with space as it's not defined in standard XML
        cleaned_content = content.replace(b'&nbsp;', b' ')
        # Use recover=True to handle broken XML better
        parser = ET.XMLParser(recover=True)
        return ET.fromstring(cleaned_content, parser=parser)
    except ET.XMLSyntaxError as e:
        logging.error(f"XML parsing error: {e}")
        return None

def parse_html(content: bytes) -> Optional[BeautifulSoup]:
    """
    Parses HTML content using BeautifulSoup.
    """
    try:
        return BeautifulSoup(content, 'lxml')
    except Exception as e:
        logging.error(f"HTML parsing error: {e}")
        return None

def clean_text(text: Optional[str]) -> str:
    """
    Cleans whitespace from text.
    """
    if text:
        return " ".join(text.split())
    return ""

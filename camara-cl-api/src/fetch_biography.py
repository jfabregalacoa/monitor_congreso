"""
Script para realizar webscraping de biografías de diputados desde BCN.
Fuente: https://www.bcn.cl/historiapolitica/resenas_parlamentarias/

Este script extrae información biográfica de diputados chilenos para complementar
el dataset de votaciones del período 2002-2026.
"""

import requests
from bs4 import BeautifulSoup
import json
import time
from typing import Dict, List
import logging

# Configurar logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


class BiographyScraper:
    """Clase para realizar webscraping de biografías de diputados desde BCN."""
    
    BASE_URL = "https://www.bcn.cl/historiapolitica/resenas_parlamentarias/index.html"
    
    def __init__(self):
        self.session = requests.Session()
        self.session.headers.update({
            'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36'
        })
    
    def fetch_page(self, periodo: str = "1990-2018", pagina: int = 7) -> str:
        """
        Obtiene el HTML de una página específica.
        
        Args:
            periodo: Período parlamentario (ej: "1990-2018")
            pagina: Número de página
            
        Returns:
            Contenido HTML de la página
        """
        params = {
            'categ': 'por_periodo',
            'periodo': periodo,
            'pagina': str(pagina)
        }
        
        try:
            response = self.session.get(self.BASE_URL, params=params, timeout=10)
            response.raise_for_status()
            logger.info(f"Página obtenida exitosamente: período={periodo}, página={pagina}")
            return response.text
        except requests.RequestException as e:
            logger.error(f"Error al obtener página: {e}")
            raise
    
    def parse_deputy_list(self, html: str) -> List[Dict]:
        """
        Parsea el HTML para extraer información de diputados.
        
        Args:
            html: Contenido HTML de la página
            
        Returns:
            Lista de diccionarios con información de diputados
        """
        soup = BeautifulSoup(html, 'html.parser')
        deputies = []
        
        # TODO: Implementar el parsing específico según la estructura del sitio
        # Esta es una estructura de ejemplo que debe ajustarse al HTML real
        
        logger.info(f"Se encontraron {len(deputies)} diputados en la página")
        return deputies
    
    def extract_biography_details(self, deputy_url: str) -> Dict:
        """
        Extrae detalles biográficos de la página individual de un diputado.
        
        Args:
            deputy_url: URL de la página del diputado
            
        Returns:
            Diccionario con información biográfica detallada
        """
        # TODO: Implementar extracción de detalles biográficos
        biography = {
            'nombre': '',
            'periodo': '',
            'partido': '',
            'experiencia_politica': [],
            'experiencia_profesional': [],
            'educacion': []
        }
        
        return biography
    
    def save_to_json(self, data: List[Dict], filename: str):
        """
        Guarda los datos en formato JSON.
        
        Args:
            data: Lista de diccionarios con información de diputados
            filename: Nombre del archivo de salida
        """
        output_path = f"../outputs/{filename}"
        with open(output_path, 'w', encoding='utf-8') as f:
            json.dump(data, f, ensure_ascii=False, indent=2)
        logger.info(f"Datos guardados en {output_path}")


def main():
    """Función principal para ejecutar el webscraping."""
    scraper = BiographyScraper()
    
    # Períodos a scrapear (ajustar según necesidades)
    periodos = ["1990-2018", "2018-2026"]
    
    all_deputies = []
    
    for periodo in periodos:
        logger.info(f"Procesando período: {periodo}")
        # TODO: Implementar lógica para iterar sobre todas las páginas
        # TODO: Implementar rate limiting para no sobrecargar el servidor
        time.sleep(1)  # Pausa entre requests
    
    # Guardar resultados
    scraper.save_to_json(all_deputies, "biografias_diputados.json")
    logger.info("Webscraping completado")


if __name__ == "__main__":
    main()

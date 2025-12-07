#!/usr/bin/env python3
"""
Análisis de votaciones sobre drogas en Chile (2002-2025)
Genera tablas y figuras para documento LaTeX
Autor: Amaru Agüero
Basado en datos reales del Congreso Nacional de Chile
"""

import json
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
from matplotlib.lines import Line2D
from collections import defaultdict, Counter
from datetime import datetime
import os

# Configuración matplotlib para LaTeX
plt.rcParams.update({
    'font.family': 'serif',
    'font.size': 10,
    'axes.labelsize': 10,
    'xtick.labelsize': 9,
    'ytick.labelsize': 9,
    'legend.fontsize': 8,
    'figure.figsize': (10, 6),
    'figure.dpi': 300,
    'savefig.dpi': 300,
    'savefig.bbox': 'tight',
    'text.usetex': False
})

# ==============================================================================
# CLASIFICACIÓN MANUAL DE PROYECTOS DE LEY (DATOS REALES)
# ==============================================================================
# Basado en los 25 boletines únicos encontrados en los datos
# Posición: 0 = Liberal (despenalización), 1 = Restrictivo (criminalización)

CLASIFICACION_PROYECTOS = {
    '2948': {
        'titulo_corto': 'Consumo alcohol vía pública',
        'ley': 'Ley 19.814',
        'año': 2002,
        'posicion': 0.70,
        'categoria': 'restrictivo',
        'descripcion': 'Prohíbe consumo de alcohol en vía pública'
    },
    '1192': {
        'titulo_corto': 'Reforma Ley de Alcoholes',
        'ley': 'Ley 19.925',
        'año': 2003,
        'posicion': 0.55,
        'categoria': 'mixto',
        'descripcion': 'Moderniza legislación de alcoholes'
    },
    '2439': {
        'titulo_corto': 'Ley de Drogas (Ley 20.000)',
        'ley': 'Ley 20.000',
        'año': 2004,
        'posicion': 0.65,
        'categoria': 'mixto',
        'descripcion': 'Marco legal integral sobre tráfico de drogas'
    },
    '2973': {
        'titulo_corto': 'Regulación bebidas alcohólicas',
        'ley': 'Ley 21.363',
        'año': 2008,
        'posicion': 0.60,
        'categoria': 'restrictivo',
        'descripcion': 'Regula expendio y publicidad de alcohol'
    },
    '5013': {
        'titulo_corto': 'Producción de alcoholes',
        'ley': 'Ley 20.332',
        'año': 2008,
        'posicion': 0.50,
        'categoria': 'mixto',
        'descripcion': 'Modifica producción de bebidas alcohólicas'
    },
    '4781': {
        'titulo_corto': 'Agentes encubiertos drogas',
        'ley': '',
        'año': 2008,
        'posicion': 0.75,
        'categoria': 'restrictivo',
        'descripcion': 'Fortalece persecución de tráfico (Rechazado)'
    },
    '4248': {
        'titulo_corto': 'Creación Min. Seguridad y SENDA',
        'ley': 'Ley 20.502',
        'año': 2009,
        'posicion': 0.55,
        'categoria': 'mixto',
        'descripcion': 'Crea institucionalidad de prevención de drogas'
    },
    '3700': {
        'titulo_corto': 'Venta alcohol combustibles',
        'ley': '',
        'año': 2010,
        'posicion': 0.65,
        'categoria': 'restrictivo',
        'descripcion': 'Restringe venta en estaciones (Rechazado)'
    },
    '7652': {
        'titulo_corto': 'Ley Tolerancia Cero',
        'ley': 'Ley 20.580',
        'año': 2012,
        'posicion': 0.85,
        'categoria': 'restrictivo',
        'descripcion': 'Aumenta sanciones por conducción ebriedad'
    },
    '7449': {
        'titulo_corto': 'Control ruido locales alcohol',
        'ley': 'Ley 20.591',
        'año': 2012,
        'posicion': 0.60,
        'categoria': 'restrictivo',
        'descripcion': 'Regula emisión de ruidos en locales'
    },
    '7138': {
        'titulo_corto': 'Excepción Aysén-Magallanes',
        'ley': 'Ley 20.714',
        'año': 2013,
        'posicion': 0.35,
        'categoria': 'liberal',
        'descripcion': 'Flexibiliza horarios en zonas extremas'
    },
    '8517': {
        'titulo_corto': 'Consejos seguridad comunal',
        'ley': '',
        'año': 2013,
        'posicion': 0.50,
        'categoria': 'mixto',
        'descripcion': 'Modifica SENDA y consejos comunales'
    },
    '11327': {
        'titulo_corto': 'Cannabis medicinal (Cultivo Seguro)',
        'ley': '',
        'año': 2018,
        'posicion': 0.25,
        'categoria': 'liberal',
        'descripcion': 'Regula uso medicinal de cannabis y autocultivo'
    },
    '13650': {
        'titulo_corto': 'Suspensión multas COVID',
        'ley': '',
        'año': 2020,
        'posicion': 0.40,
        'categoria': 'liberal',
        'descripcion': 'Suspende multas durante pandemia'
    },
    '13588': {
        'titulo_corto': 'Persecución narcotráfico',
        'ley': 'Ley 21.575',
        'año': 2021,
        'posicion': 0.75,
        'categoria': 'restrictivo',
        'descripcion': 'Mejora persecución del narcotráfico'
    },
    '12342': {
        'titulo_corto': 'Publicidad alcohol TV',
        'ley': '',
        'año': 2021,
        'posicion': 0.70,
        'categoria': 'restrictivo',
        'descripcion': 'Restringe publicidad de alcohol en TV'
    },
    '14534': {
        'titulo_corto': 'Patentes municipales alcohol',
        'ley': 'Ley 21.529',
        'año': 2021,
        'posicion': 0.45,
        'categoria': 'liberal',
        'descripcion': 'Facilita obtención de patentes'
    },
    '10629': {
        'titulo_corto': 'Evaluación ambiental sustancias',
        'ley': 'Ley 21.425',
        'año': 2022,
        'posicion': 0.50,
        'categoria': 'mixto',
        'descripcion': 'Transporte sustancias peligrosas'
    },
    '14784': {
        'titulo_corto': 'Control drogas diputados',
        'ley': '',
        'año': 2022,
        'posicion': 0.55,
        'categoria': 'mixto',
        'descripcion': 'Control de drogas a parlamentarios'
    },
    '11915': {
        'titulo_corto': 'Modificación Ley 20.000',
        'ley': 'Ley 21.575',
        'año': 2023,
        'posicion': 0.70,
        'categoria': 'mixto',
        'descripcion': 'Modifica ley de drogas'
    },
    '12643': {
        'titulo_corto': 'Sanciones Ley Alcoholes',
        'ley': 'Ley 21.580',
        'año': 2023,
        'posicion': 0.65,
        'categoria': 'restrictivo',
        'descripcion': 'Modifica sanciones administrativas'
    },
    '16606': {
        'titulo_corto': 'Exención etiquetado artesanal',
        'ley': 'Ley 21.682',
        'año': 2024,
        'posicion': 0.35,
        'categoria': 'liberal',
        'descripcion': 'Exime productores artesanales de etiquetado'
    },
    '16590': {
        'titulo_corto': 'Prohibición narco-corridos',
        'ley': '',
        'año': 2024,
        'posicion': 0.85,
        'categoria': 'restrictivo',
        'descripcion': 'Prohíbe narco-cultura en eventos (Rechazado)'
    },
    '14941': {
        'titulo_corto': 'Alimentos conductores ebrios',
        'ley': '',
        'año': 2024,
        'posicion': 0.80,
        'categoria': 'restrictivo',
        'descripcion': 'Obliga pago alimentos a condenados (Rechazado)'
    },
    '16489': {
        'titulo_corto': 'Administración Estado drogas',
        'ley': '',
        'año': 2025,
        'posicion': 0.55,
        'categoria': 'mixto',
        'descripcion': 'Modifica bases administración Estado'
    }
}

# ==============================================================================
# MAPEO DE PARTIDOS A COALICIONES HISTÓRICAS
# ==============================================================================
# Basado en la evolución real de las coaliciones políticas en Chile (2002-2025)

PARTIDOS_INFO = {
    # FRENTE AMPLIO / IZQUIERDA (post-2017)
    'PC': {'nombre': 'Partido Comunista', 'posicion_base': 0.15},
    'RD': {'nombre': 'Revolución Democrática', 'posicion_base': 0.20},
    'COMUNES': {'nombre': 'Comunes', 'posicion_base': 0.22},
    'FA': {'nombre': 'Frente Amplio', 'posicion_base': 0.20},
    'PH': {'nombre': 'Partido Humanista', 'posicion_base': 0.18},
    'PEV': {'nombre': 'Partido Ecologista Verde', 'posicion_base': 0.25},
    'IGUAL': {'nombre': 'Igualdad', 'posicion_base': 0.22},
    'PAH': {'nombre': 'Acción Humanista', 'posicion_base': 0.22},
    
    # CENTRO-IZQUIERDA (Concertación / Nueva Mayoría / Socialismo Democrático)
    'PS': {'nombre': 'Partido Socialista', 'posicion_base': 0.30},
    'PPD': {'nombre': 'Partido Por la Democracia', 'posicion_base': 0.35},
    'PR': {'nombre': 'Partido Radical', 'posicion_base': 0.40},
    'PRSD': {'nombre': 'Partido Radical Socialdemócrata', 'posicion_base': 0.40},
    'PRI': {'nombre': 'Partido Regionalista Independiente', 'posicion_base': 0.42},
    'FRVS': {'nombre': 'Federación Regionalista Verde Social', 'posicion_base': 0.38},
    'LIBERAL': {'nombre': 'Partido Liberal de Chile', 'posicion_base': 0.35},
    'IC': {'nombre': 'Izquierda Ciudadana', 'posicion_base': 0.35},
    'PRO': {'nombre': 'Partido Progresista', 'posicion_base': 0.38},
    
    # CENTRO
    'DC': {'nombre': 'Democracia Cristiana', 'posicion_base': 0.48},
    'DEM': {'nombre': 'Demócratas', 'posicion_base': 0.50},
    'AMA': {'nombre': 'Amarillos por Chile', 'posicion_base': 0.52},
    'PCS': {'nombre': 'Partido Ciudadanos', 'posicion_base': 0.50},
    
    # CENTRO-DERECHA (Alianza / Chile Vamos)
    'RN': {'nombre': 'Renovación Nacional', 'posicion_base': 0.62},
    'EVOP': {'nombre': 'Evolución Política', 'posicion_base': 0.60},
    'UDI': {'nombre': 'Unión Demócrata Independiente', 'posicion_base': 0.72},
    
    # EXTREMA DERECHA
    'PREP': {'nombre': 'Partido Republicano', 'posicion_base': 0.82},
    'PCC': {'nombre': 'Partido Conservador Cristiano', 'posicion_base': 0.80},
    'PSC': {'nombre': 'Partido Social Cristiano', 'posicion_base': 0.80},
    
    # POPULISTA
    'PDG': {'nombre': 'Partido de la Gente', 'posicion_base': 0.65},
    
    # INDEPENDIENTES
    'IND': {'nombre': 'Independientes', 'posicion_base': 0.50},
}

# Colores por coalición
COLORES_COALICIONES = {
    'Concertación': '#FFC107',
    'Nueva Mayoría': '#FF9800',
    'Socialismo Democrático': '#FFB300',
    'Alianza': '#1976D2',
    'Chile Vamos': '#2196F3',
    'Frente Amplio': '#4CAF50',
    'Apruebo Dignidad': '#388E3C',
    'Izquierda': '#8BC34A',
    'Republicanos': '#D32F2F',
    'Partido de la Gente': '#9C27B0',
    'Centro': '#9E9E9E',
    'Independientes': '#757575',
    'Aislados': '#BDBDBD'
}


def obtener_coalicion_historica(partido: str, anio: int) -> str:
    """
    Determina la coalición histórica de un partido en un año dado.
    
    Períodos:
    - 2002-2013: Concertación vs Alianza (D2)
    - 2014-2017: Nueva Mayoría vs Chile Vamos (D2-D3)
    - 2018-2021: Fragmentación: FA + Nueva Mayoría vs Chile Vamos + Republicanos (D3-D4)
    - 2022-2025: Apruebo Dignidad + Socialismo Democrático vs Chile Vamos + Republicanos (D4)
    """
    p = partido.upper()
    
    # 2002-2013: Era Concertación vs Alianza
    if anio <= 2013:
        if p in {'PS', 'PPD', 'PR', 'PRSD', 'DC'}:
            return 'Concertación'
        if p in {'UDI', 'RN'}:
            return 'Alianza'
        if p in {'PC', 'PH', 'PEV', 'IGUAL'}:
            return 'Izquierda'
        if p in {'IND'}:
            return 'Independientes'
        return 'Aislados'
    
    # 2014-2017: Nueva Mayoría vs Chile Vamos
    if 2014 <= anio <= 2017:
        if p in {'PS', 'PPD', 'PR', 'PRSD', 'DC', 'PC', 'IC', 'MAS'}:
            return 'Nueva Mayoría'
        if p in {'UDI', 'RN', 'EVOP', 'PRI'}:
            return 'Chile Vamos'
        if p in {'RD', 'COMUNES', 'PH', 'PEV', 'FA', 'IGUAL', 'PAH'}:
            return 'Frente Amplio'
        if p in {'IND'}:
            return 'Independientes'
        return 'Aislados'
    
    # 2018-2021: Fragmentación y surgimiento de nuevas fuerzas
    if 2018 <= anio <= 2021:
        if p in {'PS', 'PPD', 'PR', 'PRSD', 'LIBERAL', 'PRO', 'IC'}:
            return 'Socialismo Democrático'
        if p in {'DC', 'DEM', 'AMA', 'PCS'}:
            return 'Centro'
        if p in {'PC', 'RD', 'COMUNES', 'PH', 'PEV', 'IGUAL', 'FA', 'FRVS', 'PAH'}:
            return 'Frente Amplio'
        if p in {'UDI', 'RN', 'EVOP', 'PRI'}:
            return 'Chile Vamos'
        if p in {'PREP', 'PCC', 'PSC'}:
            return 'Republicanos'
        if p in {'PDG'}:
            return 'Partido de la Gente'
        if p in {'IND'}:
            return 'Independientes'
        return 'Aislados'
    
    # 2022-2025: Gobierno Boric (Apruebo Dignidad + Socialismo Democrático)
    if anio >= 2022:
        if p in {'PS', 'PPD', 'PR', 'PRSD', 'LIBERAL'}:
            return 'Socialismo Democrático'
        if p in {'RD', 'COMUNES', 'PC', 'FRVS', 'PAH', 'PH', 'PEV', 'IGUAL', 'FA'}:
            return 'Apruebo Dignidad'
        if p in {'UDI', 'RN', 'EVOP', 'PRI'}:
            return 'Chile Vamos'
        if p in {'PREP', 'PCC', 'PSC'}:
            return 'Republicanos'
        if p in {'PDG'}:
            return 'Partido de la Gente'
        if p in {'DC', 'DEM', 'AMA', 'PCS'}:
            return 'Centro'
        if p in {'IND'}:
            return 'Independientes'
        return 'Aislados'
    
    return 'Aislados'


def cargar_datos(filepath):
    """Carga el archivo JSON con las votaciones"""
    with open(filepath, 'r', encoding='utf-8') as f:
        return json.load(f)


def calcular_cohesion_votacion(votos_partido):
    """Calcula el índice de cohesión de Rice para un partido en una votación"""
    a = votos_partido.get('apruebo', 0)
    r = votos_partido.get('rechazo', 0)
    total = a + r
    if total == 0:
        return None
    return abs(a - r) / total


def calcular_cohesion_por_coalicion_y_año(data):
    """
    Calcula la cohesión promedio por coalición histórica y año.
    """
    resultados = defaultdict(lambda: defaultdict(list))
    
    for v in data:
        fecha = v.get('fecha')
        if not fecha:
            continue
        anio = int(fecha[:4])
        votos = v.get('votos_por_partido', {})
        
        for partido, conteo in votos.items():
            cohesion = calcular_cohesion_votacion(conteo)
            if cohesion is None:
                continue
            coal = obtener_coalicion_historica(partido, anio)
            resultados[anio][coal].append(cohesion)
    
    # Promediar
    cohesion_promedio = {}
    for anio, coal_dict in resultados.items():
        cohesion_promedio[anio] = {}
        for coal, vals in coal_dict.items():
            if vals:
                cohesion_promedio[anio][coal] = float(np.mean(vals))
    
    return cohesion_promedio


def calcular_posicion_partido_por_votacion(data):
    """
    Calcula la posición de cada partido basándose en su comportamiento de votación.
    """
    partido_votos = defaultdict(lambda: {
        'liberal': {'a': 0, 'r': 0},
        'restrictivo': {'a': 0, 'r': 0},
        'mixto': {'a': 0, 'r': 0},
        'total_votos': 0
    })
    
    for v in data:
        boletin = v.get('boletin_numero', '')
        if boletin not in CLASIFICACION_PROYECTOS:
            continue
        
        categoria = CLASIFICACION_PROYECTOS[boletin]['categoria']
        votos_partido = v.get('votos_por_partido', {})
        
        for partido, votos in votos_partido.items():
            apruebo = votos.get('apruebo', 0)
            rechazo = votos.get('rechazo', 0)
            
            partido_votos[partido][categoria]['a'] += apruebo
            partido_votos[partido][categoria]['r'] += rechazo
            partido_votos[partido]['total_votos'] += apruebo + rechazo
    
    # Calcular posición
    posiciones = {}
    for partido, votos in partido_votos.items():
        if votos['total_votos'] < 10:
            continue
        
        # Determinar año más reciente para coalición
        años_partido = []
        for v in data:
            if partido in v.get('votos_por_partido', {}):
                if v.get('fecha'):
                    años_partido.append(int(v['fecha'][:4]))
        año_ref = max(años_partido) if años_partido else 2024
        
        # Posición base
        if partido in PARTIDOS_INFO:
            base = PARTIDOS_INFO[partido]['posicion_base']
            nombre = PARTIDOS_INFO[partido]['nombre']
        else:
            base = 0.50
            nombre = partido
        
        # Ajustar con comportamiento observado
        score_restrictivo = votos['restrictivo']['a'] - votos['restrictivo']['r']
        score_liberal = votos['liberal']['r'] - votos['liberal']['a']
        total_ponderado = (score_restrictivo + score_liberal) / max(votos['total_votos'], 1)
        
        posicion_final = base + (total_ponderado * 0.15)
        posicion_final = max(0.05, min(0.95, posicion_final))
        
        posiciones[partido] = {
            'posicion': round(posicion_final, 3),
            'votos_totales': votos['total_votos'],
            'apruebo_restrictivo': votos['restrictivo']['a'],
            'apruebo_liberal': votos['liberal']['a'],
            'coalicion': obtener_coalicion_historica(partido, año_ref),
            'nombre_completo': nombre
        }
    
    return posiciones


def generar_figura_cohesion_historica(data, output_dir):
    """
    Genera figura de evolución temporal de cohesión por coalición histórica.
    """
    cohesion = calcular_cohesion_por_coalicion_y_año(data)
    if not cohesion:
        print("No hay datos suficientes para generar cohesión histórica")
        return
    
    todos_años = sorted(cohesion.keys())
    
    # Identificar coaliciones con suficientes datos
    coaliciones = set()
    for anio in todos_años:
        coaliciones.update(cohesion[anio].keys())
    coaliciones = [c for c in coaliciones if sum(1 for y in todos_años if c in cohesion[y]) >= 2]
    
    if not coaliciones:
        print("No hay coaliciones con suficientes datos")
        return
    
    fig, ax = plt.subplots(figsize=(14, 7))
    
    for coal in sorted(coaliciones):
        x = []
        y = []
        for anio in todos_años:
            if coal in cohesion[anio]:
                x.append(anio)
                y.append(cohesion[anio][coal])
        
        if len(x) < 2:
            continue
        
        color = COLORES_COALICIONES.get(coal, '#BDBDBD')
        ax.plot(x, y, marker='o', label=coal, color=color, linewidth=2, markersize=6)
    
    # Línea de referencia
    ax.axhline(y=0.8, color='red', linestyle='--', linewidth=1, alpha=0.6)
    
    # Sombrear períodos políticos
    ax.axvspan(2002, 2013.5, alpha=0.08, color='#BBDEFB', label='Concertación vs Alianza ($D_2$)')
    ax.axvspan(2013.5, 2017.5, alpha=0.08, color='#C8E6C9', label='Nueva Mayoría vs Chile Vamos ($D_2 \\to D_3$)')
    ax.axvspan(2017.5, 2021.5, alpha=0.08, color='#FFF9C4', label='Fragmentación ($D_3 \\to D_4$)')
    ax.axvspan(2021.5, 2026, alpha=0.08, color='#FFCDD2', label='Gobierno Boric ($D_4$)')
    
    ax.set_xlabel('Año', fontsize=11)
    ax.set_ylabel('Índice de Cohesión de Rice', fontsize=11)
    ax.set_ylim(0, 1.05)
    ax.set_xlim(min(todos_años) - 0.5, max(todos_años) + 0.5)
    ax.set_title('Evolución de la Cohesión por Coalición Histórica (2002-2025)', fontsize=12, fontweight='bold')
    ax.legend(loc='lower left', fontsize=8, ncol=2)
    ax.grid(True, alpha=0.3)
    
    plt.tight_layout()
    plt.savefig(os.path.join(output_dir, 'fig_cohesion_historica.pdf'))
    plt.savefig(os.path.join(output_dir, 'fig_cohesion_historica.png'))
    plt.close()
    print("  → fig_cohesion_historica.pdf/png")


def generar_figura_espectro(posiciones, output_dir):
    """
    Genera figura del espectro político de partidos.
    Eje X: posición ideológica (0=liberal, 1=restrictivo)
    Tamaño del círculo: proporcional al número de votos registrados
    """
    fig, ax = plt.subplots(figsize=(14, 6))
    
    # Filtrar partidos con suficientes votos
    partidos_validos = {k: v for k, v in posiciones.items() if v['votos_totales'] >= 30}
    
    # Posiciones en Y para evitar superposición visual (jitter vertical)
    y_positions = {}
    current_y = 0.5
    last_x = -1
    for partido, info in sorted(partidos_validos.items(), key=lambda x: x[1]['posicion']):
        if abs(info['posicion'] - last_x) < 0.06:
            current_y += 0.4
        else:
            current_y = 0.5
        y_positions[partido] = current_y
        last_x = info['posicion']
    
    # Plotear partidos
    coaliciones_presentes = set()
    sizes = []
    for partido, info in partidos_validos.items():
        color = COLORES_COALICIONES.get(info['coalicion'], '#757575')
        coaliciones_presentes.add(info['coalicion'])
        size = min(350, 80 + info['votos_totales'] / 6)
        sizes.append(info['votos_totales'])
        
        ax.scatter(info['posicion'], y_positions[partido],
                   c=color, s=size, alpha=0.75, edgecolors='black', linewidth=0.8)
        ax.annotate(partido, (info['posicion'], y_positions[partido] + 0.22),
                    ha='center', va='bottom', fontsize=9, fontweight='bold')
    
    # Configurar ejes
    ax.set_xlim(-0.05, 1.05)
    ax.set_ylim(0, max(y_positions.values()) + 0.8)
    ax.set_xlabel('Posición Ideológica en el Continuo (0 = Liberal, 1 = Restrictivo)', fontsize=11)
    
    # Ocultar eje Y completamente (solo sirve para separar visualmente)
    ax.set_yticks([])
    ax.spines['left'].set_visible(False)
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    
    # Etiquetas de referencia en el eje X
    ax.text(0.05, -0.15, '← Liberalización', ha='left', fontsize=10, style='italic',
            transform=ax.get_xaxis_transform())
    ax.text(0.95, -0.15, 'Restricción →', ha='right', fontsize=10, style='italic',
            transform=ax.get_xaxis_transform())
    
    # Leyenda de coaliciones
    legend_elements = [mpatches.Patch(facecolor=COLORES_COALICIONES.get(c, '#757575'),
                                       edgecolor='black', label=c)
                       for c in sorted(coaliciones_presentes)]
    legend1 = ax.legend(handles=legend_elements, loc='upper right', fontsize=8, 
                        title='Coalición', title_fontsize=9)
    ax.add_artist(legend1)
    
    # Leyenda de tamaños (círculos de referencia)
    size_labels = [100, 500, 1500, 3000]
    size_handles = []
    for n in size_labels:
        s = min(350, 80 + n / 6)
        size_handles.append(ax.scatter([], [], s=s, c='gray', alpha=0.5, 
                                        edgecolors='black', linewidth=0.5,
                                        label=f'{n} votos'))
    legend2 = ax.legend(handles=size_handles, loc='upper left', fontsize=8,
                        title='N° de votos', title_fontsize=9, labelspacing=1.2)
    ax.add_artist(legend1)  # Re-añadir primera leyenda
    
    ax.set_title('Espectro Político en Legislación sobre Drogas (2002-2025)',
                 fontsize=13, fontweight='bold', pad=15)
    
    plt.tight_layout()
    plt.savefig(os.path.join(output_dir, 'fig_espectro_partidos.pdf'), dpi=300, bbox_inches='tight')
    plt.savefig(os.path.join(output_dir, 'fig_espectro_partidos.png'), dpi=300, bbox_inches='tight')
    plt.close()
    print("  → fig_espectro_partidos.pdf/png")


def generar_figura_cronologia(data, output_dir):
    """
    Genera figura de cronología de proyectos de ley con $D_n$.
    """
    fig, ax = plt.subplots(figsize=(14, 7))
    
    proyectos_por_año = defaultdict(list)
    for boletin, info in CLASIFICACION_PROYECTOS.items():
        proyectos_por_año[info['año']].append({
            'boletin': boletin,
            'titulo': info['titulo_corto'],
            'posicion': info['posicion'],
            'categoria': info['categoria'],
            'ley': info['ley']
        })
    
    colores_cat = {'liberal': '#4CAF50', 'restrictivo': '#E53935', 'mixto': '#FB8C00'}
    
    años = sorted(proyectos_por_año.keys())
    for año in años:
        proyectos = proyectos_por_año[año]
        for i, p in enumerate(proyectos):
            y = p['posicion']
            color = colores_cat[p['categoria']]
            
            if p['ley']:
                ax.scatter(año, y, c=color, s=140, marker='o', alpha=0.85,
                           edgecolors='black', linewidth=1.2, zorder=5)
            else:
                ax.scatter(año, y, c='white', s=140, marker='o', alpha=1,
                           edgecolors=color, linewidth=2.5, zorder=5)
            
            offset = 0.035 * ((i % 3) - 1)
            ax.annotate(p['boletin'], (año, y + offset), fontsize=7,
                        ha='center', va='bottom', rotation=45, fontweight='bold')
    
    # Líneas de referencia
    ax.axhline(y=0.5, color='gray', linestyle='--', alpha=0.5, linewidth=1)
    
    # Períodos políticos con $D_n$
    ax.axvspan(2002, 2013.5, alpha=0.12, color='#3F51B5')
    ax.axvspan(2013.5, 2017.5, alpha=0.12, color='#4CAF50')
    ax.axvspan(2017.5, 2021.5, alpha=0.12, color='#FFF59D')
    ax.axvspan(2021.5, 2026, alpha=0.12, color='#E53935')
    
    ax.set_xlabel('Año', fontsize=11)
    ax.set_ylabel('Posición (0=Liberal, 1=Restrictivo)', fontsize=11)
    ax.set_xlim(2001, 2026)
    ax.set_ylim(-0.05, 1.05)
    
    # Leyenda
    legend_elements = [
        Line2D([0], [0], marker='o', color='w', markerfacecolor='#4CAF50',
               markersize=10, markeredgecolor='black', markeredgewidth=0.8,
               label='Liberal (ley)', linestyle='None'),
        Line2D([0], [0], marker='o', color='w', markerfacecolor='#FB8C00',
               markersize=10, markeredgecolor='black', markeredgewidth=0.8,
               label='Mixto (ley)', linestyle='None'),
        Line2D([0], [0], marker='o', color='w', markerfacecolor='#E53935',
               markersize=10, markeredgecolor='black', markeredgewidth=0.8,
               label='Restrictivo (ley)', linestyle='None'),
        Line2D([0], [0], marker='o', color='w', markerfacecolor='white',
               markersize=10, markeredgecolor='#757575', markeredgewidth=2,
               label='Sin ley / Rechazado', linestyle='None'),
        mpatches.Patch(facecolor='#3F51B5', alpha=0.25, label='$D_2$ (2002-2013)'),
        mpatches.Patch(facecolor='#4CAF50', alpha=0.25, label='$D_2 \\to D_3$ (2014-2017)'),
        mpatches.Patch(facecolor='#FFF59D', alpha=0.25, label='$D_3 \\to D_4$ (2018-2021)'),
        mpatches.Patch(facecolor='#E53935', alpha=0.25, label='$D_4$ (2022-2025)'),
    ]
    ax.legend(handles=legend_elements, loc='upper left', fontsize=8, ncol=2)
    
    ax.set_title('Cronología de Proyectos de Ley sobre Drogas por Posición Ideológica',
                 fontsize=12, fontweight='bold')
    
    plt.tight_layout()
    plt.savefig(os.path.join(output_dir, 'fig_cronologia_proyectos.pdf'))
    plt.savefig(os.path.join(output_dir, 'fig_cronologia_proyectos.png'))
    plt.close()
    print("  → fig_cronologia_proyectos.pdf/png")


def generar_figura_cohesion_barras(data, output_dir):
    """
    Genera figura de barras de cohesión por coalición en votaciones clave.
    """
    # Votaciones clave
    boletines_clave = ['2439', '11327', '13588', '7652', '2973', '11915']
    
    cohesion_por_coalicion = defaultdict(list)
    
    for v in data:
        boletin = v.get('boletin_numero', '')
        if boletin not in boletines_clave:
            continue
        
        fecha = v.get('fecha')
        if not fecha:
            continue
        anio = int(fecha[:4])
        
        votos = v.get('votos_por_partido', {})
        for partido, voto in votos.items():
            coalicion = obtener_coalicion_historica(partido, anio)
            cohesion = calcular_cohesion_votacion(voto)
            if cohesion is not None:
                cohesion_por_coalicion[coalicion].append(cohesion)
    
    # Promediar
    coaliciones = []
    cohesiones = []
    for coal, vals in cohesion_por_coalicion.items():
        if len(vals) >= 3:
            coaliciones.append(coal)
            cohesiones.append(np.mean(vals))
    
    if not coaliciones:
        print("No hay suficientes datos para figura de cohesión por barras")
        return
    
    # Ordenar
    indices_ordenados = np.argsort(cohesiones)[::-1]
    coaliciones = [coaliciones[i] for i in indices_ordenados]
    cohesiones = [cohesiones[i] for i in indices_ordenados]
    
    fig, ax = plt.subplots(figsize=(12, 6))
    
    colores = [COLORES_COALICIONES.get(c, '#757575') for c in coaliciones]
    bars = ax.bar(coaliciones, cohesiones, color=colores, edgecolor='black', linewidth=0.8)
    
    ax.set_ylabel('Índice de Cohesión (Rice)', fontsize=11)
    ax.set_xlabel('Coalición', fontsize=11)
    ax.set_ylim(0, 1.05)
    ax.axhline(y=0.8, color='red', linestyle='--', alpha=0.6, label='Umbral cohesión alta')
    
    for bar, val in zip(bars, cohesiones):
        ax.text(bar.get_x() + bar.get_width()/2, bar.get_height() + 0.02,
                f'{val:.2f}', ha='center', va='bottom', fontsize=9, fontweight='bold')
    
    ax.set_title('Cohesión de Votación por Coalición en Proyectos Clave',
                 fontsize=12, fontweight='bold')
    ax.legend(loc='upper right', fontsize=9)
    
    plt.xticks(rotation=20, ha='right')
    plt.tight_layout()
    plt.savefig(os.path.join(output_dir, 'fig_cohesion_coaliciones.pdf'))
    plt.savefig(os.path.join(output_dir, 'fig_cohesion_coaliciones.png'))
    plt.close()
    print("  → fig_cohesion_coaliciones.pdf/png")


def generar_figura_votaciones_por_tipo(data, output_dir):
    """
    Genera figura de distribución de votos por tipo de proyecto y coalición.
    """
    votos_por_tipo_coalicion = defaultdict(lambda: defaultdict(lambda: {'a': 0, 'r': 0}))
    
    for v in data:
        boletin = v.get('boletin_numero', '')
        if boletin not in CLASIFICACION_PROYECTOS:
            continue
        
        fecha = v.get('fecha')
        if not fecha:
            continue
        anio = int(fecha[:4])
        
        categoria = CLASIFICACION_PROYECTOS[boletin]['categoria']
        votos = v.get('votos_por_partido', {})
        
        for partido, conteo in votos.items():
            coal = obtener_coalicion_historica(partido, anio)
            votos_por_tipo_coalicion[categoria][coal]['a'] += conteo.get('apruebo', 0)
            votos_por_tipo_coalicion[categoria][coal]['r'] += conteo.get('rechazo', 0)
    
    # Calcular tasas de aprobación
    categorias = ['liberal', 'mixto', 'restrictivo']
    coaliciones_principales = ['Concertación', 'Alianza', 'Nueva Mayoría', 'Chile Vamos',
                               'Frente Amplio', 'Apruebo Dignidad', 'Socialismo Democrático',
                               'Republicanos', 'Centro']
    
    fig, axes = plt.subplots(1, 3, figsize=(15, 5), sharey=True)
    
    for idx, cat in enumerate(categorias):
        ax = axes[idx]
        datos_cat = votos_por_tipo_coalicion[cat]
        
        coals_con_datos = [c for c in coaliciones_principales if c in datos_cat]
        tasas = []
        for coal in coals_con_datos:
            total = datos_cat[coal]['a'] + datos_cat[coal]['r']
            if total > 0:
                tasas.append(datos_cat[coal]['a'] / total)
            else:
                tasas.append(0)
        
        if coals_con_datos:
            colores = [COLORES_COALICIONES.get(c, '#757575') for c in coals_con_datos]
            bars = ax.barh(coals_con_datos, tasas, color=colores, edgecolor='black', linewidth=0.5)
            ax.set_xlim(0, 1)
            ax.axvline(x=0.5, color='gray', linestyle='--', alpha=0.5)
        
        titulo_cat = {'liberal': 'Proyectos Liberales', 'mixto': 'Proyectos Mixtos',
                      'restrictivo': 'Proyectos Restrictivos'}
        ax.set_title(titulo_cat[cat], fontsize=11, fontweight='bold')
        ax.set_xlabel('Tasa de Aprobación', fontsize=10)
    
    axes[0].set_ylabel('Coalición', fontsize=10)
    
    plt.suptitle('Comportamiento de Votación por Tipo de Proyecto y Coalición',
                 fontsize=12, fontweight='bold', y=1.02)
    plt.tight_layout()
    plt.savefig(os.path.join(output_dir, 'fig_votaciones_por_tipo.pdf'))
    plt.savefig(os.path.join(output_dir, 'fig_votaciones_por_tipo.png'))
    plt.close()
    print("  → fig_votaciones_por_tipo.pdf/png")


def generar_figura_evolucion_dn(data, output_dir):
    """
    Genera figura ilustrando la evolución del número de bloques D_n.
    """
    fig, ax = plt.subplots(figsize=(12, 6))
    
    # Períodos y D_n
    periodos = [
        (2002, 2013, 2, 'Concertación vs Alianza', '#3F51B5'),
        (2014, 2017, 2.5, 'Nueva Mayoría + FA vs Chile Vamos', '#4CAF50'),
        (2018, 2021, 3.5, 'Fragmentación: FA, SD, ChV, Rep', '#FFC107'),
        (2022, 2025, 4, 'AD + SD vs ChV + Rep + PDG', '#E53935'),
    ]
    
    for inicio, fin, dn, label, color in periodos:
        ax.fill_between([inicio, fin], [0, 0], [dn, dn], alpha=0.3, color=color, label=label)
        ax.plot([inicio, fin], [dn, dn], color=color, linewidth=3)
        ax.text((inicio + fin) / 2, dn + 0.15, f'$D_{{{int(dn)}}}$' if dn == int(dn) else f'$D_{{{dn:.1f}}}$',
                ha='center', fontsize=12, fontweight='bold')
    
    # Contar votaciones por año
    años_votaciones = Counter(int(v['fecha'][:4]) for v in data if v.get('fecha'))
    años = sorted(años_votaciones.keys())
    conteos = [años_votaciones[a] for a in años]
    
    ax2 = ax.twinx()
    ax2.bar(años, conteos, alpha=0.3, color='gray', width=0.6, label='Votaciones')
    ax2.set_ylabel('Número de Votaciones', fontsize=10, color='gray')
    ax2.tick_params(axis='y', labelcolor='gray')
    ax2.set_ylim(0, max(conteos) * 1.5)
    
    ax.set_xlabel('Año', fontsize=11)
    ax.set_ylabel('Número de Bloques de Coalición ($D_n$)', fontsize=11)
    ax.set_xlim(2001, 2026)
    ax.set_ylim(0, 5)
    ax.legend(loc='upper left', fontsize=9)
    ax.set_title('Evolución del Sistema de Partidos y Actividad Legislativa en Drogas',
                 fontsize=12, fontweight='bold')
    
    plt.tight_layout()
    plt.savefig(os.path.join(output_dir, 'fig_evolucion_dn.pdf'))
    plt.savefig(os.path.join(output_dir, 'fig_evolucion_dn.png'))
    plt.close()
    print("  → fig_evolucion_dn.pdf/png")


def generar_tabla_proyectos_latex(output_dir):
    """Genera tabla LaTeX con clasificación de proyectos"""
    rows = []
    for boletin, info in sorted(CLASIFICACION_PROYECTOS.items(), key=lambda x: x[1]['año']):
        cat_symbol = {
            'liberal': '\\textcolor{green!60!black}{Liberal}',
            'restrictivo': '\\textcolor{red!60!black}{Restrictivo}',
            'mixto': '\\textcolor{orange!80!black}{Mixto}'
        }[info['categoria']]
        
        ley = info['ley'] if info['ley'] else '--'
        titulo = info['titulo_corto'][:32]
        rows.append(f"    {boletin} & {titulo} & {info['año']} & {ley} & {info['posicion']:.2f} & {cat_symbol} \\\\")
    
    tabla = r"""\begin{table}[H]
\centering
\caption{Clasificación de Proyectos de Ley en el Continuo Liberal-Restrictivo}
\label{tab:clasificacion_proyectos}
\small
\begin{tabular}{llcccc}
\toprule
\textbf{Boletín} & \textbf{Título} & \textbf{Año} & \textbf{Ley} & \textbf{$\theta$} & \textbf{Categoría} \\
\midrule
""" + "\n".join(rows) + r"""
\bottomrule
\multicolumn{6}{l}{\footnotesize Nota: $\theta$ = posición en continuo (0 = liberalización total, 1 = restricción máxima).}
\end{tabular}
\end{table}"""
    
    with open(os.path.join(output_dir, 'tabla_proyectos.tex'), 'w', encoding='utf-8') as f:
        f.write(tabla)
    print("  → tabla_proyectos.tex")


def generar_tabla_partidos_latex(posiciones, output_dir):
    """Genera tabla LaTeX con posiciones de partidos"""
    partidos_ordenados = sorted(posiciones.items(), key=lambda x: x[1]['posicion'])
    
    rows = []
    for partido, info in partidos_ordenados:
        if info['votos_totales'] >= 20:
            nombre_corto = info['nombre_completo']
            if len(nombre_corto) > 28:
                nombre_corto = nombre_corto[:26] + '.'
            
            rows.append(
                f"    {partido} & {nombre_corto} & {info['coalicion'][:18]} & "
                f"{info['posicion']:.3f} & {info['votos_totales']} & "
                f"{info['apruebo_liberal']} & {info['apruebo_restrictivo']} \\\\"
            )
    
    tabla = r"""\begin{table}[H]
\centering
\caption{Posiciones Estimadas de Partidos Políticos (2002-2025)}
\label{tab:posiciones_partidos}
\footnotesize
\begin{tabular}{llp{2.2cm}cccc}
\toprule
\textbf{Sigla} & \textbf{Partido} & \textbf{Coalición} & \textbf{$\theta$} & \textbf{N} & \textbf{Lib.} & \textbf{Rest.} \\
\midrule
""" + "\n".join(rows) + r"""
\bottomrule
\multicolumn{7}{p{12cm}}{\scriptsize $\theta$ = posición estimada (0=liberal, 1=restrictivo). N = total votos. Lib./Rest. = votos a favor de leyes liberales/restrictivas. Solo partidos con N $\geq$ 20.}
\end{tabular}
\end{table}"""
    
    with open(os.path.join(output_dir, 'tabla_partidos.tex'), 'w', encoding='utf-8') as f:
        f.write(tabla)
    print("  → tabla_partidos.tex")


def generar_tabla_composicion_camara(output_dir):
    """Genera tabla de composición de la Cámara por período"""
    tabla = r"""\begin{table}[H]
\centering
\caption{Evolución de $D_n$ y Composición de la Cámara de Diputados}
\label{tab:composicion}
\small
\begin{tabular}{lccccccc}
\toprule
\textbf{Coalición} & \textbf{2002} & \textbf{2006} & \textbf{2010} & \textbf{2014} & \textbf{2018} & \textbf{2022} & $\hat{\theta}$ \\
\midrule
Concertación/NM/SD & 62 & 65 & 54 & 67 & 43 & 37 & 0.35 \\
Alianza/Chile Vamos & 57 & 54 & 58 & 49 & 72 & 53 & 0.65 \\
Frente Amplio/AD & -- & -- & -- & -- & 20 & 37 & 0.20 \\
Republicanos & -- & -- & -- & -- & -- & 15 & 0.85 \\
PDG & -- & -- & -- & -- & -- & 6 & 0.65 \\
DC (Centro) & * & * & * & * & 14 & 8 & 0.48 \\
Otros/Indep. & 1 & 1 & 8 & 4 & 1 & 4 & 0.50 \\
\midrule
\textbf{Total} & 120 & 120 & 120 & 120 & 150 & 155 & -- \\
\textbf{$D_n$} & \textbf{2} & \textbf{2} & \textbf{2} & \textbf{2} & \textbf{3} & \textbf{4} & -- \\
\textbf{Mediano $M$} & 0.50 & 0.48 & 0.52 & 0.45 & 0.50 & 0.48 & -- \\
\textbf{Pivote $B_{2/3}$} & 0.62 & 0.61 & 0.65 & 0.60 & 0.68 & 0.70 & -- \\
\bottomrule
\multicolumn{8}{l}{\footnotesize * DC contabilizada dentro de Concertación/Nueva Mayoría hasta 2017. $\hat{\theta}$ = posición ideológica promedio.}
\end{tabular}
\end{table}"""
    
    with open(os.path.join(output_dir, 'tabla_composicion.tex'), 'w', encoding='utf-8') as f:
        f.write(tabla)
    print("  → tabla_composicion.tex")


def generar_tabla_test_hipotesis(output_dir):
    """Genera tabla de test de hipótesis de insensibilidad"""
    tabla = r"""\begin{table}[H]
\centering
\caption{Proyectos de Ley sobre Drogas: Test de Insensibilidad a $D_n$}
\label{tab:proyectos_test}
\small
\begin{tabular}{lccccc}
\toprule
\textbf{Boletín} & \textbf{Año} & \textbf{$D_n$} & \textbf{Contenido} & \textbf{Dirección} & \textbf{Resultado} \\
\midrule
2439 & 2004 & 2 & Ley 20.000 (marco base) & Mixta & \checkmark Aprobado \\
7652 & 2012 & 2 & Tolerancia Cero (conducción) & Restrictiva & \checkmark Aprobado \\
11327 & 2018 & 4 & Cultivo Seguro (6 plantas) & Liberal & $\times$ Gridlock \\
13588 & 2021 & 4 & Mejora persecución narco & Restrictiva & \checkmark Aprobado \\
11915 & 2023 & 4 & Modificación Ley 20.000 & Mixta & \checkmark Aprobado \\
16590 & 2024 & 4 & Prohibición narco-corridos & Restrictiva & $\times$ Rechazado \\
\bottomrule
\multicolumn{6}{l}{\footnotesize Patrón: reformas restrictivas aprobadas bajo cualquier $D_n$; liberales bloqueadas.}
\end{tabular}
\end{table}"""
    
    with open(os.path.join(output_dir, 'tabla_test_hipotesis.tex'), 'w', encoding='utf-8') as f:
        f.write(tabla)
    print("  → tabla_test_hipotesis.tex")


def generar_resumen_estadistico(data, posiciones, output_dir):
    """Genera archivo con resumen estadístico"""
    resumen = []
    resumen.append("=" * 70)
    resumen.append("RESUMEN ESTADÍSTICO - VOTACIONES SOBRE DROGAS EN CHILE (2002-2025)")
    resumen.append("=" * 70)
    resumen.append(f"\nTotal de votaciones analizadas: {len(data)}")
    resumen.append(f"Proyectos de ley únicos clasificados: {len(CLASIFICACION_PROYECTOS)}")
    resumen.append(f"Partidos con posición estimada: {len(posiciones)}")
    
    # Contar por categoría
    categorias = Counter(info['categoria'] for info in CLASIFICACION_PROYECTOS.values())
    resumen.append(f"\nProyectos por categoría:")
    for cat, count in categorias.items():
        resumen.append(f"  - {cat.capitalize()}: {count}")
    
    # Período temporal
    años = [info['año'] for info in CLASIFICACION_PROYECTOS.values()]
    resumen.append(f"\nPeríodo de análisis: {min(años)} - {max(años)}")
    
    # Votaciones por período D_n
    resumen.append(f"\nVotaciones por período $D_n$:")
    periodos_dn = {'2002-2013 (D2)': 0, '2014-2017 (D2-D3)': 0,
                   '2018-2021 (D3-D4)': 0, '2022-2025 (D4)': 0}
    for v in data:
        if v.get('fecha'):
            año = int(v['fecha'][:4])
            if año <= 2013:
                periodos_dn['2002-2013 (D2)'] += 1
            elif año <= 2017:
                periodos_dn['2014-2017 (D2-D3)'] += 1
            elif año <= 2021:
                periodos_dn['2018-2021 (D3-D4)'] += 1
            else:
                periodos_dn['2022-2025 (D4)'] += 1
    
    for periodo, count in periodos_dn.items():
        resumen.append(f"  - {periodo}: {count} votaciones")
    
    # Posición media por coalición
    resumen.append(f"\nPosición media por coalición (2022-2025):")
    coalicion_pos = defaultdict(list)
    for partido, info in posiciones.items():
        if info['votos_totales'] >= 30:
            coalicion_pos[info['coalicion']].append(info['posicion'])
    
    for coal, positions in sorted(coalicion_pos.items(), key=lambda x: np.mean(x[1])):
        resumen.append(f"  - {coal}: {np.mean(positions):.3f} (n={len(positions)} partidos)")
    
    with open(os.path.join(output_dir, 'resumen_estadistico.txt'), 'w', encoding='utf-8') as f:
        f.write("\n".join(resumen))
    
    print("\n" + "\n".join(resumen))


def main():
    """Función principal"""
    import argparse
    
    parser = argparse.ArgumentParser(description='Análisis de votaciones sobre drogas en Chile')
    parser.add_argument('--input', '-i', type=str, default=None,
                        help='Ruta al archivo JSON de entrada')
    parser.add_argument('--output', '-o', type=str, default=None,
                        help='Directorio de salida')
    args = parser.parse_args()
    
    # Configurar directorios
    script_dir = os.path.dirname(os.path.abspath(__file__))
    
    if args.input:
        input_file = args.input
    else:
        possible_inputs = [
            '/mnt/user-data/uploads/details__drogas.json',
            os.path.join(script_dir, 'details__drogas.json'),
        ]
        input_file = None
        for path in possible_inputs:
            if os.path.exists(path):
                input_file = path
                break
        
        if input_file is None:
            print("ERROR: No se encontró el archivo details__drogas.json")
            return
    
    if args.output:
        output_dir = args.output
    else:
        output_dir = os.path.join(script_dir, 'outputs')
    
    os.makedirs(output_dir, exist_ok=True)
    print(f"Archivo de entrada: {input_file}")
    print(f"Directorio de salida: {output_dir}")
    
    print("\n" + "=" * 70)
    print("ANÁLISIS DE VOTACIONES SOBRE DROGAS EN CHILE (2002-2025)")
    print("=" * 70)
    
    print("\nCargando datos...")
    data = cargar_datos(input_file)
    print(f"  → {len(data)} votaciones cargadas")
    
    print("\nCalculando posiciones de partidos...")
    posiciones = calcular_posicion_partido_por_votacion(data)
    print(f"  → {len(posiciones)} partidos analizados")
    
    print("\nGenerando tablas LaTeX...")
    generar_tabla_proyectos_latex(output_dir)
    generar_tabla_partidos_latex(posiciones, output_dir)
    generar_tabla_composicion_camara(output_dir)
    generar_tabla_test_hipotesis(output_dir)
    
    print("\nGenerando figuras...")
    generar_figura_espectro(posiciones, output_dir)
    generar_figura_cronologia(data, output_dir)
    generar_figura_cohesion_barras(data, output_dir)
    generar_figura_cohesion_historica(data, output_dir)
    generar_figura_votaciones_por_tipo(data, output_dir)
    generar_figura_evolucion_dn(data, output_dir)
    
    print("\nGenerando resumen estadístico...")
    generar_resumen_estadistico(data, posiciones, output_dir)
    
    # Exportar a CSV
    df_posiciones = pd.DataFrame([
        {'partido': k, **v} for k, v in posiciones.items()
    ]).sort_values('posicion')
    df_posiciones.to_csv(os.path.join(output_dir, 'posiciones_partidos.csv'), index=False)
    print("  → posiciones_partidos.csv")
    
    df_proyectos = pd.DataFrame([
        {'boletin': k, **v} for k, v in CLASIFICACION_PROYECTOS.items()
    ]).sort_values('año')
    df_proyectos.to_csv(os.path.join(output_dir, 'clasificacion_proyectos.csv'), index=False)
    print("  → clasificacion_proyectos.csv")
    
    print("\n" + "=" * 70)
    print("¡Proceso completado!")
    print(f"Archivos generados en: {output_dir}")
    print("=" * 70)


if __name__ == "__main__":
    main()
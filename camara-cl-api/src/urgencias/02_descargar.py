"""Uso: python3 -I camara-cl-api/src/urgencias/02_descargar.py <carpeta_intermedios> <carpeta_cache>

Paso 2. Descarga, con caché en disco, lo que falta para reconstruir la urgencia vigente en cada votación (2014-2026).
Port de 02_reconstruir_urgencias.py (versión del curso, 20-10-2026), extendido a 2014-2018 y a tres servicios más.

Fuentes (servicios abiertos, sin autenticación):
  Cámara  WSLegislativo.asmx/retornarVotacionDetalle?prmVotacionId=ID            fecha, boletín, quórum, resultado y votos
  Cámara  WSLegislativo.asmx/retornarVotacionesXProyectoLey?prmNumeroBoletin=N-SS  tipo de votación, artículo y trámite
  Cámara  WSDiputado.asmx/retornarDiputadosXPeriodo?prmPeriodoId=P               diputados, fechas en el cargo, militancias
  Senado  wspublico/tramitacion.php?boletin=N                                    historial de urgencias y datos del proyecto
www.camara.cl no se usa (responde 403 por Cloudflare).

Entradas (del paso 1): ids_2014_18.txt, new_vote_ids.txt, muestra_validacion_ids.txt, votes_details_2018_26.csv
Salidas en <carpeta_intermedios>:
  votes_2014_18.csv, votes_new.csv          metadatos de las votaciones pedidas a la Cámara (columnas de votes_new del curso + extras)
  votos_detalle_camara.csv.gz               voto de cada diputado en las votaciones pedidas (para validar las matrices)
  votaciones_proyecto_camara.csv            una fila por votación y boletín: tipo de votación, artículo, trámite
  proyectos_camara.csv                      una fila por boletín según la Cámara (iniciativa, fecha de ingreso)
  urgency_spells.csv, senado_proyectos.csv  historial de urgencias y estado de cada proyecto según el Senado (columnas del curso + extras)
  diputados_periodo.csv, militancias.csv    diputados por período (fechas en el cargo) y militancias con fechas
  fetch_failures.txt, descargas_resumen.csv fallos y resumen de cada lote
Reanudable: lo que ya está en <carpeta_cache> no se vuelve a pedir. Las respuestas que no son XML válido no se guardan en
la caché (quedan en _invalidas/ para revisarlas) y se vuelven a pedir en la próxima corrida.

Ritmo (por servidor): hasta 6 conexiones y 0,05 s de pausa. Cada 100 peticiones se mide la tasa de error; si pasa de 2 %,
primero sube la pausa (0,05 -> 0,1 -> 0,2 -> 0,4 -> 0,8 s) sin bajar conexiones; solo si sigue alta con 0,8 s baja las
conexiones (6 -> 4 -> 2 -> 1) y vuelve a la pausa mínima. Tras 500 peticiones con menos de 0,5 % de errores recupera un
escalón. Cada petición fallida se reintenta hasta 4 veces con espera creciente (2, 4, 8 s).
"""
import csv, gzip, os, queue, random, re, ssl, sys, threading, time, urllib.error, urllib.request
import xml.etree.ElementTree as ET
from datetime import datetime

OUT, CACHE = sys.argv[1], sys.argv[2]
SUB = {k: os.path.join(CACHE, k) for k in ["camara_votacion", "camara_proyecto", "camara_diputados", "senado", "_invalidas", "logs"]}
for d in SUB.values():
    os.makedirs(d, exist_ok=True)
UA = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0 Safari/537.36"
CTX = ssl.create_default_context(cafile="/etc/ssl/cert.pem")  # almacén de certificados del sistema (macOS)
CAM = "https://opendata.camara.cl/camaradiputados/WServices/"
SEN = "https://tramitacion.senado.cl/wspublico/tramitacion.php?boletin="
PERIODOS_CAMARA = [5, 6, 7, 8, 9, 10, 11]  # prmPeriodoId a probar; se identifican después por sus diputados
LOGF = open(os.path.join(SUB["logs"], "02_descargar.log"), "a", encoding="utf-8")
LOCK = threading.Lock()
failures, resumen = [], []


def log(msg):
    line = f"{datetime.now():%Y-%m-%d %H:%M:%S} {msg}"
    with LOCK:
        print(line, flush=True)
        LOGF.write(line + "\n"); LOGF.flush()


class Ritmo:
    """Controla conexiones simultáneas y pausa de un servidor, con escalones (ver docstring del módulo)."""
    PAUSAS = [0.05, 0.1, 0.2, 0.4, 0.8]
    CONEXIONES = [6, 4, 2, 1]

    def __init__(self, nombre):
        self.nombre, self.ip, self.ic, self.activos = nombre, 0, 0, 0
        self.cond = threading.Condition()
        self.ventana, self.racha = [], []
        self.n_ok = self.n_err = 0

    def pausa(self):
        return self.PAUSAS[self.ip]

    def entrar(self):
        with self.cond:
            while self.activos >= self.CONEXIONES[self.ic]:
                self.cond.wait()
            self.activos += 1

    def salir(self):
        with self.cond:
            self.activos -= 1
            self.cond.notify_all()

    def registrar(self, ok):
        with self.cond:
            self.n_ok += ok; self.n_err += (not ok)
            self.ventana.append(ok); self.racha.append(ok)
            if len(self.ventana) >= 100:
                tasa = self.ventana.count(False) / len(self.ventana); self.ventana = []
                if tasa > 0.02:
                    self.racha = []
                    if self.ip < len(self.PAUSAS) - 1:
                        self.ip += 1
                    elif self.ic < len(self.CONEXIONES) - 1:
                        self.ic += 1; self.ip = 0
                    log(f"[{self.nombre}] errores {tasa:.1%} en 100 peticiones -> pausa {self.pausa()} s, "
                        f"{self.CONEXIONES[self.ic]} conexiones")
            if len(self.racha) >= 500:
                tasa = self.racha.count(False) / len(self.racha); self.racha = []
                if tasa < 0.005 and (self.ip > 0 or self.ic > 0):
                    if self.ip > 0:
                        self.ip -= 1
                    else:
                        self.ic -= 1; self.ip = len(self.PAUSAS) - 1
                    log(f"[{self.nombre}] errores {tasa:.1%} en 500 peticiones -> recupera: pausa {self.pausa()} s, "
                        f"{self.CONEXIONES[self.ic]} conexiones")
            self.cond.notify_all()

    def estado(self):
        return f"pausa {self.pausa()} s, {self.CONEXIONES[self.ic]} conexiones, {self.n_ok} ok / {self.n_err} errores"


def local(tag):
    return tag.split("}")[-1]


def parse_bytes(data):
    return ET.fromstring(data.replace(b"&nbsp;", b" ").lstrip())


def validador(*raices):
    def v(data):
        if not data.strip():
            return "respuesta vacía"
        try:
            root = parse_bytes(data)
        except ET.ParseError as e:
            return f"XML inválido: {e}"
        return None if local(root.tag) in raices else f"raíz inesperada: {local(root.tag)}"
    return v


def fetch(ritmo, url, path, validar):
    """Devuelve 'cache', 'ok' o 'fallo'. Nunca vuelve a pedir lo que ya está en la caché."""
    if os.path.exists(path) and os.path.getsize(path) > 0:
        return "cache"
    ultimo = ""
    for intento in range(4):
        ritmo.entrar()
        ok, data = False, b""
        try:
            req = urllib.request.Request(url, headers={"User-Agent": UA})
            with urllib.request.urlopen(req, timeout=60, context=CTX) as r:
                data = r.read()
            motivo = validar(data)
            if motivo is None:
                ok = True
            else:
                ultimo = motivo
                inv = os.path.join(SUB["_invalidas"], os.path.basename(path) + f".{int(time.time())}")
                with open(inv, "wb") as f:
                    f.write(data[:200000])
        except urllib.error.HTTPError as e:
            ultimo = f"HTTP {e.code}"
        except Exception as e:  # noqa: BLE001 (timeouts, conexión, SSL)
            ultimo = repr(e)[:200]
        finally:
            ritmo.registrar(ok)
            time.sleep(ritmo.pausa())  # la pausa se cumple dentro del cupo de conexión
            ritmo.salir()
        if ok:
            tmp = path + ".tmp"
            with open(tmp, "wb") as f:
                f.write(data)
            os.replace(tmp, path)
            return "ok"
        time.sleep(2 * 2 ** intento + random.random() if intento < 3 else 0)
    with LOCK:
        failures.append(f"{url}\t{ultimo}")
    return "fallo"


def lote(nombre, ritmo, tareas):
    """tareas: lista de (url, path, validar). 6 hilos; el Ritmo limita cuántos piden a la vez."""
    t0, cont, q = time.time(), {"cache": 0, "ok": 0, "fallo": 0}, queue.Queue()
    for t in tareas:
        q.put(t)
    hechos = [0]

    def trabajador():
        while True:
            try:
                url, path, val = q.get_nowait()
            except queue.Empty:
                return
            res = fetch(ritmo, url, path, val)
            with LOCK:
                cont[res] += 1; hechos[0] += 1; n = hechos[0]
            if n % 250 == 0:
                log(f"[{nombre}] {n}/{len(tareas)} | caché {cont['cache']} | nuevas {cont['ok']} | fallos {cont['fallo']} | "
                    f"{ritmo.estado()} | {n / max(time.time() - t0, 1e-9):.1f} por s")

    log(f"[{nombre}] inicio: {len(tareas)} tareas")
    hilos = [threading.Thread(target=trabajador) for _ in range(6)]
    for h in hilos:
        h.start()
    for h in hilos:
        h.join()
    dur = time.time() - t0
    log(f"[{nombre}] fin en {dur:.0f} s | caché {cont['cache']} | nuevas {cont['ok']} | fallos {cont['fallo']} | {ritmo.estado()}")
    with LOCK:
        resumen.append({"lote": nombre, "inicio": datetime.fromtimestamp(t0).isoformat(timespec="seconds"),
                        "fin": datetime.now().isoformat(timespec="seconds"), "tareas": len(tareas), "cache": cont["cache"],
                        "nuevas": cont["ok"], "fallos": cont["fallo"], "segundos": round(dur), "estado_final": ritmo.estado()})


def en_paralelo(*funcs):
    hs = [threading.Thread(target=f) for f in funcs]
    for h in hs:
        h.start()
    for h in hs:
        h.join()


# ------------------------------------------------------------------ utilidades de parseo
def hijo(el, name):
    for ch in el:
        if local(ch.tag) == name:
            return ch
    return None


def htext(el, name):
    ch = hijo(el, name) if el is not None else None
    return (ch.text or "").strip() if ch is not None else ""


def hattr(el, name, attr):
    ch = hijo(el, name) if el is not None else None
    return ch.attrib.get(attr, "") if ch is not None else ""


def text_any(el, name):
    """Primer descendiente con ese nombre (como la versión del curso)."""
    for ch in el.iter():
        if local(ch.tag) == name:
            return (ch.text or "").strip()
    return ""


def boletin_de(desc):
    """'Boletín N° 9.366-04' -> ('9366', '9366-04'). Quita puntos de miles antes de buscar NNNN-SS."""
    s = re.sub(r"(?<=\d)\.(?=\d{3}\b)", "", desc or "")
    m = re.search(r"(\d+)-(\d+)", s)
    return (m.group(1), f"{m.group(1)}-{m.group(2)}") if m else ("", "")


def iso(d):
    """dd/mm/aaaa -> aaaa-mm-dd; corrige años escritos con dos dígitos (p.ej. 0010 -> 2010). Devuelve (iso, corregido)."""
    d = (d or "").strip()
    m = re.match(r"^(\d{1,2})/(\d{1,2})/(\d{1,4})$", d)
    if not m:
        return "", False
    dd, mm, yy = int(m.group(1)), int(m.group(2)), int(m.group(3))
    fixed = False
    if yy < 100:
        yy += 2000
        fixed = True
    try:
        return datetime(yy, mm, dd).strftime("%Y-%m-%d"), fixed
    except ValueError:
        return "", True


def leer_ids(nombre):
    return [l.strip() for l in open(os.path.join(OUT, nombre), encoding="utf-8") if l.strip()]


def escribir(nombre, filas, cols):
    path = os.path.join(OUT, nombre)
    op = gzip.open if nombre.endswith(".gz") else open
    with op(path + ".tmp", "wt", encoding="utf-8", newline="") as f:
        w = csv.DictWriter(f, fieldnames=cols, extrasaction="ignore")
        w.writeheader(); w.writerows(filas)
    os.replace(path + ".tmp", path)
    log(f"   escrito {nombre}: {len(filas)} filas")


def vpath(vid): return os.path.join(SUB["camara_votacion"], f"{vid}.xml")
def ppath(bol): return os.path.join(SUB["camara_proyecto"], f"{bol}.xml")
def spath(num): return os.path.join(SUB["senado"], f"{num}.xml")
def dpath(p): return os.path.join(SUB["camara_diputados"], f"periodo_{p}.xml")


V_VOT, V_PROY = validador("Votacion"), validador("ProyectoLey", "ProyectosLey")
V_DIP, V_SEN = validador("DiputadosPeriodoColeccion"), validador("proyectos")
cam, sen = Ritmo("Cámara"), Ritmo("Senado")

# ------------------------------------------------------------------ fase A: votaciones (Cámara) y boletines conocidos (Senado)
ids_1418, ids_new, ids_muestra = leer_ids("ids_2014_18.txt"), leer_ids("new_vote_ids.txt"), leer_ids("muestra_validacion_ids.txt")
ids_vot = list(dict.fromkeys(ids_1418 + ids_new + ids_muestra))
det = list(csv.DictReader(open(os.path.join(OUT, "votes_details_2018_26.csv"), encoding="utf-8")))
bol_json = sorted({r["boletin_numero"] for r in det if r["boletin_numero"]}, key=int)
log(f"Fase A: {len(ids_vot)} votaciones a la Cámara (2014-18 {len(ids_1418)}, nuevas {len(ids_new)}, muestra {len(ids_muestra)}), "
    f"{len(PERIODOS_CAMARA)} períodos de diputados; {len(bol_json)} boletines al Senado")


def fase_a_camara():
    lote("Cámara diputados", cam, [(f"{CAM}WSDiputado.asmx/retornarDiputadosXPeriodo?prmPeriodoId={p}", dpath(p), V_DIP)
                                   for p in PERIODOS_CAMARA])
    lote("Cámara votaciones", cam, [(f"{CAM}WSLegislativo.asmx/retornarVotacionDetalle?prmVotacionId={v}", vpath(v), V_VOT)
                                    for v in ids_vot])


en_paralelo(fase_a_camara, lambda: lote("Senado (boletines JSON)", sen, [(SEN + b, spath(b), V_SEN) for b in bol_json]))

# ------------------------------------------------------------------ parseo de votaciones
cols_v = ["vote_id", "fecha", "boletin_full", "boletin_numero", "quorum", "resultado", "tipo",
          "boletin_sufijo", "total_si", "total_no", "total_abst", "total_disp"]
meta, votos, raices_hijos = {}, [], {}
for vid in ids_vot:
    p = vpath(vid)
    if not (os.path.exists(p) and os.path.getsize(p) > 0):
        continue
    try:
        root = parse_bytes(open(p, "rb").read())
    except ET.ParseError as e:
        failures.append(f"parse camara votacion {vid}\t{e!r}")
        continue
    for ch in root:
        raices_hijos[local(ch.tag)] = raices_hijos.get(local(ch.tag), 0) + 1
    desc = htext(root, "Descripcion")
    num, suf = boletin_de(desc)
    meta[vid] = {"vote_id": vid, "fecha": htext(root, "Fecha"), "boletin_full": desc, "boletin_numero": num,
                 "quorum": htext(root, "Quorum"), "resultado": htext(root, "Resultado"), "tipo": htext(root, "Tipo"),
                 "boletin_sufijo": suf, "total_si": htext(root, "TotalSi"), "total_no": htext(root, "TotalNo"),
                 "total_abst": htext(root, "TotalAbstencion"), "total_disp": htext(root, "TotalDispensado")}
    for el in root.iter():
        if local(el.tag) == "Voto":
            dip = hijo(el, "Diputado")
            votos.append({"vote_id": vid, "diputado_id": htext(dip, "Id"), "opcion": htext(el, "OpcionVoto"),
                          "opcion_valor": hattr(el, "OpcionVoto", "Valor")})
log(f"Etiquetas de primer nivel en las votaciones de la Cámara: {raices_hijos}")
escribir("votes_2014_18.csv", [meta[v] for v in ids_1418 if v in meta], cols_v)
escribir("votes_new.csv", [meta[v] for v in ids_new if v in meta], cols_v)
escribir("votos_detalle_camara.csv.gz", votos, ["vote_id", "diputado_id", "opcion", "opcion_valor"])

# ------------------------------------------------------------------ fase B: proyectos (Cámara) y boletines nuevos (Senado)
bol_suf = {}
for r in det:
    num, suf = boletin_de(r.get("boletin_full", "")) if r.get("boletin_full") else ("", "")
    if not suf:
        num, suf = boletin_de(r.get("desc_boletin", ""))
    if num and suf:
        bol_suf.setdefault(num, suf)
for m in meta.values():
    if m["boletin_numero"] and m["boletin_sufijo"]:
        bol_suf.setdefault(m["boletin_numero"], m["boletin_sufijo"])
bol_sen = sorted(set(bol_json) | {m["boletin_numero"] for m in meta.values() if m["boletin_numero"]}, key=int)
nuevos_sen = [b for b in bol_sen if b not in set(bol_json)]
log(f"Fase B: {len(bol_suf)} proyectos a la Cámara (votaciones por proyecto); {len(nuevos_sen)} boletines nuevos al Senado "
    f"(total Senado {len(bol_sen)})")
en_paralelo(
    lambda: lote("Cámara proyectos", cam, [(f"{CAM}WSLegislativo.asmx/retornarVotacionesXProyectoLey?prmNumeroBoletin={s}",
                                            ppath(s), V_PROY) for s in sorted(bol_suf.values())]),
    lambda: lote("Senado (boletines nuevos)", sen, [(SEN + b, spath(b), V_SEN) for b in nuevos_sen]))

# ------------------------------------------------------------------ parseo: proyectos según la Cámara
vp, pc = [], []
for s in sorted(bol_suf.values()):
    p = ppath(s)
    if not (os.path.exists(p) and os.path.getsize(p) > 0):
        continue
    try:
        root = parse_bytes(open(p, "rb").read())
    except ET.ParseError as e:
        failures.append(f"parse camara proyecto {s}\t{e!r}")
        continue
    proys = [root] if local(root.tag) == "ProyectoLey" else [c for c in root if local(c.tag) == "ProyectoLey"]
    for pr in proys:
        vots = [v for v in pr.iter() if local(v.tag) == "VotacionProyectoLey"]
        mats = [htext(m, "Nombre") for m in pr.iter() if local(m.tag) == "Materia"]
        pc.append({"boletin_consultado": s, "numero_boletin": htext(pr, "NumeroBoletin"), "nombre": htext(pr, "Nombre"),
                   "fecha_ingreso": htext(pr, "FechaIngreso"), "tipo_iniciativa": htext(pr, "TipoIniciativa"),
                   "camara_origen": htext(pr, "CamaraOrigen"), "admisible": htext(pr, "Admisible"),
                   "n_votaciones": len(vots), "materias": " | ".join(m for m in mats if m)})
        for v in vots:
            vp.append({"boletin_consultado": s, "vote_id": htext(v, "Id"), "fecha": htext(v, "Fecha"),
                       "descripcion": htext(v, "Descripcion"), "tipo_votacion": htext(v, "TipoVotacionProyectoLey"),
                       "tipo_votacion_valor": hattr(v, "TipoVotacionProyectoLey", "Valor"), "articulo": htext(v, "Articulo"),
                       "tramite_constitucional": htext(v, "TramiteConstitucional"),
                       "tramite_constitucional_id": hattr(v, "TramiteConstitucional", "Id"),
                       "tramite_reglamentario": htext(v, "TramiteReglamentario"),
                       "tramite_reglamentario_id": hattr(v, "TramiteReglamentario", "Id"),
                       "quorum": htext(v, "Quorum"), "resultado": htext(v, "Resultado"), "tipo": htext(v, "Tipo"),
                       "total_si": htext(v, "TotalSi"), "total_no": htext(v, "TotalNo"),
                       "total_abst": htext(v, "TotalAbstencion"), "total_disp": htext(v, "TotalDispensado")})
escribir("votaciones_proyecto_camara.csv", vp, list(vp[0].keys()) if vp else ["vote_id"])
escribir("proyectos_camara.csv", pc, list(pc[0].keys()) if pc else ["boletin_consultado"])

# ------------------------------------------------------------------ parseo: Senado (igual que la versión del curso + extras)
spells, proyectos = [], []
for b in bol_sen:
    p = spath(b)
    if not (os.path.exists(p) and os.path.getsize(p) > 0):
        continue
    try:
        root = parse_bytes(open(p, "rb").read())
    except ET.ParseError as e:
        failures.append(f"parse senado {b}\t{e!r}")
        continue
    desc = next((el for el in root.iter() if local(el.tag) == "descripcion"), None)
    g = (lambda n: text_any(desc, n) if desc is not None else "")
    proyectos.append({"boletin_numero": b, "urgencia_actual_senado": g("urgencia_actual"), "etapa": g("etapa"),
                      "estado": g("estado"), "iniciativa": g("iniciativa"), "fecha_ingreso": g("fecha_ingreso"),
                      "n_urgencias": 0, "boletin_senado": g("boletin"), "camara_origen": g("camara_origen"),
                      "titulo": g("titulo")})
    n = 0
    for u in root.iter():
        if local(u.tag) != "urgencia" or len(list(u)) == 0:
            continue
        gg = {local(c.tag): (c.text or "").strip() for c in u}
        fi, fix_i = iso(gg.get("FECHAINGRESO"))
        fr, fix_r = iso(gg.get("FECHARETIRO"))
        spells.append({"boletin_numero": b, "tipo": gg.get("TIPO", ""), "camara_ingreso": gg.get("CAMARAINGRESO", ""),
                       "mensaje_ingreso": gg.get("MENSAJEINGRESO", ""), "f_ingreso": fi, "f_ingreso_corregida": int(fix_i),
                       "camara_retiro": gg.get("CAMARARETIRO", ""), "mensaje_retiro": gg.get("MENSAJERETIRO", ""),
                       "f_retiro": fr, "f_retiro_corregida": int(fix_r),
                       "raw_f_ingreso": gg.get("FECHAINGRESO", ""), "raw_f_retiro": gg.get("FECHARETIRO", "")})
        n += 1
    proyectos[-1]["n_urgencias"] = n
escribir("urgency_spells.csv", spells, ["boletin_numero", "tipo", "camara_ingreso", "mensaje_ingreso", "f_ingreso",
                                        "f_ingreso_corregida", "camara_retiro", "mensaje_retiro", "f_retiro",
                                        "f_retiro_corregida", "raw_f_ingreso", "raw_f_retiro"])
escribir("senado_proyectos.csv", proyectos, ["boletin_numero", "urgencia_actual_senado", "etapa", "estado", "iniciativa",
                                             "fecha_ingreso", "n_urgencias", "boletin_senado", "camara_origen", "titulo"])

# ------------------------------------------------------------------ parseo: diputados y militancias (Cámara)
dips, mils, vistos = [], [], set()
for per in PERIODOS_CAMARA:
    p = dpath(per)
    if not (os.path.exists(p) and os.path.getsize(p) > 0):
        continue
    root = parse_bytes(open(p, "rb").read())
    for dp in root:
        if local(dp.tag) != "DiputadoPeriodo":
            continue
        d = hijo(dp, "Diputado"); dist = hijo(dp, "Distrito")
        did = htext(d, "Id")
        dips.append({"periodo_id": per, "diputado_id": did, "nombre": htext(d, "Nombre"), "nombre2": htext(d, "Nombre2"),
                     "apellido_paterno": htext(d, "ApellidoPaterno"), "apellido_materno": htext(d, "ApellidoMaterno"),
                     "sexo": htext(d, "Sexo"), "fecha_nacimiento": htext(d, "FechaNacimiento"),
                     "fecha_inicio": htext(dp, "FechaInicio"), "fecha_termino": htext(dp, "FechaTermino"),
                     "distrito": htext(dist, "Numero")})
        ms = hijo(d, "Militancias")
        for m in (ms if ms is not None else []):
            par = hijo(m, "Partido")
            row = {"diputado_id": did, "fecha_inicio": htext(m, "FechaInicio"), "fecha_termino": htext(m, "FechaTermino"),
                   "partido_id": htext(par, "Id"), "partido_nombre": htext(par, "Nombre"), "partido_alias": htext(par, "Alias")}
            key = tuple(row.values())
            if key not in vistos:
                vistos.add(key); row["periodo_id_fuente"] = per; mils.append(row)
escribir("diputados_periodo.csv", dips, ["periodo_id", "diputado_id", "nombre", "nombre2", "apellido_paterno",
                                         "apellido_materno", "sexo", "fecha_nacimiento", "fecha_inicio", "fecha_termino",
                                         "distrito"])
escribir("militancias.csv", mils, ["diputado_id", "fecha_inicio", "fecha_termino", "partido_id", "partido_nombre",
                                   "partido_alias", "periodo_id_fuente"])

with open(os.path.join(OUT, "fetch_failures.txt"), "w", encoding="utf-8") as f:
    f.write("\n".join(failures))
escribir("descargas_resumen.csv", resumen, ["lote", "inicio", "fin", "tareas", "cache", "nuevas", "fallos", "segundos", "estado_final"])
log(f"Listo: {len(meta)} votaciones, {len(votos)} votos individuales, {len(vp)} votaciones por proyecto, {len(pc)} proyectos "
    f"Cámara, {len(spells)} urgencias, {len(proyectos)} proyectos Senado, {len(dips)} diputado-período, {len(mils)} militancias, "
    f"{len(failures)} fallos")

// =============================================================================
// visor.js — mapa del partit guanyador i pactòmetre
// Les dades les escriu R a <script id="dades-visor" type="application/json">.
// =============================================================================
(function () {
  "use strict";

  const dades = JSON.parse(document.getElementById("dades-visor").textContent);
  const colors = dades.colors;

  // ---------------------------------------------------------------- utilitats
  function crea(etiqueta, atributs, ...fills) {
    const el = document.createElement(etiqueta);
    for (const [k, v] of Object.entries(atributs || {})) el.setAttribute(k, v);
    for (const f of fills) el.append(f instanceof Node ? f : document.createTextNode(String(f)));
    return el;
  }
  function esc(s) {
    return String(s).replace(/[&<>"]/g, c => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]));
  }
  function es_clar(hex) {
    const n = parseInt(hex.slice(1), 16);
    return (0.299 * ((n >> 16) & 255) + 0.587 * ((n >> 8) & 255) + 0.114 * (n & 255)) > 170;
  }
  const nf = x => Number(x).toLocaleString("ca-ES");
  const pf = x => Number(x).toLocaleString("ca-ES", { minimumFractionDigits: 1, maximumFractionDigits: 1 }) + "%";

  // Grup de botons que fa de selector; crida `canvi(valor)` quan es tria
  function grup_botons(el, canvi) {
    el.addEventListener("click", e => {
      const b = e.target.closest("button");
      if (!b) return;
      el.querySelectorAll("button").forEach(x => x.setAttribute("aria-pressed", String(x === b)));
      canvi(b.dataset.v);
    });
  }

  // ================================================================ MAPA
  const contenidor = document.getElementById("mapa-leaflet");
  if (contenidor && window.L) {
    const estat = { nivell: "prov", metode: "nou", filtre: "Tots" };
    const limits = {
      peninsula: [[35.8, -9.6], [43.9, 4.5]],
      canaries: [[27.5, -18.3], [29.5, -13.3]]
    };

    const map = L.map(contenidor, { zoomSnap: 0.25, scrollWheelZoom: false });
    map.fitBounds(limits.peninsula);
    // Mapa de fons gris clar d'Esri (no necessita clau d'API)
    const esri = "https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/";
    L.tileLayer(esri + "World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}", {
      maxZoom: 16,
      attribution: 'Mapa de fons &copy; <a href="https://www.esri.com/">Esri</a>, HERE, Garmin, &copy; OpenStreetMap | Geometries: IGN (SIANE)'
    }).addTo(map);
    // Noms de ciutats i països per sobre dels colors
    map.createPane("etiquetes");
    map.getPane("etiquetes").style.zIndex = 650;
    map.getPane("etiquetes").style.pointerEvents = "none";
    L.tileLayer(esri + "World_Light_Gray_Reference/MapServer/tile/{z}/{y}/{x}",
      { maxZoom: 16, pane: "etiquetes" }).addTo(map);

    // Requadre amb els resultats del territori
    const info = L.control({ position: "topright" });
    info.onAdd = () => {
      const d = L.DomUtil.create("div", "mapa-info");
      L.DomEvent.disableScrollPropagation(d);
      return d;
    };
    info.addTo(map);

    // Botó Canàries / Península
    const salt = L.control({ position: "bottomleft" });
    salt.onAdd = () => {
      const b = L.DomUtil.create("button", "mapa-salt");
      b.type = "button";
      b.textContent = "Canàries";
      let canaries = false;
      L.DomEvent.on(b, "click", e => {
        L.DomEvent.stop(e);
        canaries = !canaries;
        map.fitBounds(canaries ? limits.canaries : limits.peninsula);
        b.textContent = canaries ? "Península" : "Canàries";
      });
      return b;
    };
    salt.addTo(map);

    // Cercador
    const llista = crea("datalist", { id: "mapa-llista" });
    const input = crea("input", { type: "search", list: "mapa-llista", "aria-label": "Cerca un territori" });
    const cerca = L.control({ position: "topleft" });
    cerca.onAdd = () => {
      const d = crea("div", { class: "mapa-cerca" }, input, llista);
      L.DomEvent.disableClickPropagation(d);
      L.DomEvent.disableScrollPropagation(d);
      return d;
    };
    cerca.addTo(map);

    let capa = null;
    const clau = () => (estat.metode === "dhondt" && estat.nivell !== "mun") ? "gd" : "gn";

    function taula_info(p) {
      const amb_escons = estat.nivell !== "mun";
      const ke = estat.metode === "dhondt" ? 3 : 4;
      const files = typeof p.files === "string" ? JSON.parse(p.files) : p.files;
      const cos = files.map(f => `<tr>
          <td class="sigla"><span class="punt" style="background:${colors[f[0]] || "#B9BFC7"}"></span>${esc(f[0])}</td>
          <td class="n">${nf(f[1])}</td><td class="n">${pf(f[2])}</td>
          ${amb_escons ? `<td class="n"><b>${f[ke]}</b></td>` : ""}</tr>`).join("");
      return `<h4>${esc(p.nom)}</h4><table>
        <tr><th>Partit</th><th class="n">Vots</th><th class="n">%</th>${amb_escons ? '<th class="n">Escons</th>' : ""}</tr>
        ${cos}</table>`;
    }
    const mostra = p => { info.getContainer().innerHTML = taula_info(p); };
    const ressalta = l => { l.setStyle({ weight: 2, color: "#111111" }); l.bringToFront(); };

    function estil(f) {
      const p = f.properties, k = clau();
      const actiu = estat.filtre === "Tots" || p[k] === estat.filtre;
      return {
        fillColor: actiu ? p[k === "gd" ? "cd" : "cn"] : "#dfe3e8",
        fillOpacity: actiu ? 0.85 : 0.6,
        color: "#ffffff",
        weight: estat.nivell === "mun" ? 0.25 : 0.9
      };
    }

    // Opcions del filtre: partits que guanyen algun territori
    function actualitza_filtre() {
      const n = new Map();
      for (const f of dades[estat.nivell].features) {
        const g = f.properties[clau()];
        if (g && g !== "Empat") n.set(g, (n.get(g) || 0) + 1);
      }
      const opcions = ["Tots", ...[...n].sort((a, b) => b[1] - a[1]).map(d => d[0])];
      if (!opcions.includes(estat.filtre)) estat.filtre = "Tots";
      const el = document.getElementById("mapa-filtre");
      el.replaceChildren(crea("span", { class: "filtre-etiqueta" }, "Filtra"),
        ...opcions.map(o => crea("label", {},
          crea("input", Object.assign({ type: "radio", name: "mapa-filtre", value: o },
            o === estat.filtre ? { checked: "" } : {})), o)));
    }

    function dibuixa() {
      if (capa) map.removeLayer(capa);
      info.getContainer().innerHTML = "";
      capa = L.geoJSON(dades[estat.nivell], {
        style: estil,
        onEachFeature: (f, l) => l.on({
          mouseover: e => { ressalta(e.target); mostra(f.properties); },
          mouseout: e => capa.resetStyle(e.target),
          click: () => mostra(f.properties)
        })
      }).addTo(map);
      input.value = "";
      input.placeholder = { prov: "Cerca una província", ccaa: "Cerca una comunitat", mun: "Cerca un municipi" }[estat.nivell];
      llista.replaceChildren(...dades[estat.nivell].features.map(f => crea("option", { value: f.properties.nom })));
    }

    input.addEventListener("change", () => {
      let trobat = null;
      capa.eachLayer(l => { if (l.feature.properties.nom === input.value) trobat = l; });
      if (!trobat) return;
      map.fitBounds(trobat.getBounds(), { maxZoom: 9 });
      ressalta(trobat);
      mostra(trobat.feature.properties);
    });

    document.getElementById("mapa-filtre").addEventListener("change", e => {
      estat.filtre = e.target.value;
      capa.setStyle(estil);
    });
    document.getElementById("mapa-nivell").addEventListener("change", e => {
      estat.nivell = e.target.value;
      actualitza_filtre();
      dibuixa();
    });
    grup_botons(document.getElementById("mapa-metode"), v => {
      estat.metode = v;
      actualitza_filtre();
      capa.setStyle(estil);
      info.getContainer().innerHTML = "";
    });

    actualitza_filtre();
    dibuixa();
  }

  // ================================================================ PACTÒMETRE
  const zona = document.getElementById("pacte-targetes");
  if (zona) {
    const partits = dades.partits;
    const majoria = dades.majoria, total = dades.total_escons;
    let metode = "nou";
    const triat = new Map(partits.map(p => [p.nom, "abst"]));
    const escons = p => metode === "nou" ? p.escons_nou : p.escons_dhondt;

    const boto = (v, text, cls) => crea("button",
      { type: "button", "data-v": v, class: cls || "", "aria-pressed": String(v === "abst") }, text);

    const targetes = partits.map(p => crea("div",
      { class: "pacte-targeta" + (es_clar(p.color) ? " clar" : ""), style: `--color:${p.color}`, "data-nom": p.nom },
      crea("div", { class: "pacte-nom" }, p.nom),
      crea("div", { class: "pacte-opcions" }, boto("si", "Sí"), boto("no", "No")),
      boto("abst", "Abstenció", "pacte-abst"),
      crea("div", { class: "pacte-escons" })));
    zona.replaceChildren(...targetes);

    function actualitza() {
      targetes.forEach(t => {
        const p = partits.find(x => x.nom === t.dataset.nom);
        t.querySelector(".pacte-escons").textContent = escons(p);
        t.querySelectorAll("button").forEach(b =>
          b.setAttribute("aria-pressed", String(b.dataset.v === triat.get(p.nom))));
      });

      const si = partits.filter(p => triat.get(p.nom) === "si");
      const no = partits.filter(p => triat.get(p.nom) === "no");
      const n_si = si.reduce((a, p) => a + escons(p), 0);
      const n_no = no.reduce((a, p) => a + escons(p), 0);
      const trams = (ps, t) => ps.map(p => crea("span",
        { style: `width:${100 * escons(p) / t}%;background:${p.color}`, title: `${p.nom}: ${escons(p)}` }));
      const pos = 100 * majoria / total;
      const missatge = n_si >= majoria
        ? "Aquesta aliança arriba a la majoria absoluta i podria investir president en primera votació."
        : n_si > n_no
          ? "No arriba a la majoria absoluta, però té majoria simple (més sí que no): podria investir president en segona votació."
          : "Aquesta aliança no arriba ni a la majoria absoluta ni a la majoria simple i no podria investir president.";

      document.getElementById("pacte-resultat").replaceChildren(
        crea("div", { class: "pacte-xifres" },
          crea("span", {}, crea("b", {}, "Sí "), String(n_si)),
          crea("span", {}, String(n_no), crea("b", {}, " No"))),
        crea("div", { class: "pacte-barra" },
          crea("div", { class: "si", style: `width:${100 * n_si / total}%` }, ...trams(si, n_si)),
          crea("div", { class: "no", style: `width:${100 * n_no / total}%` }, ...trams(no, n_no)),
          crea("span", { class: "pacte-linia", style: `left:${pos}%` })),
        crea("div", { class: "alianca-peu" },
          crea("span", { style: `left:${pos}%` }, crea("b", {}, String(majoria)), crea("br"), "Majoria absoluta")),
        crea("p", { class: "pacte-missatge" + (n_si > n_no ? " ok" : "") }, missatge));
    }

    zona.addEventListener("click", e => {
      const b = e.target.closest(".pacte-targeta button");
      if (!b) return;
      triat.set(b.closest(".pacte-targeta").dataset.nom, b.dataset.v);
      actualitza();
    });
    document.getElementById("pacte-reinicia").addEventListener("click", () => {
      triat.forEach((_, k) => triat.set(k, "abst"));
      actualitza();
    });
    grup_botons(document.getElementById("pacte-metode"), v => { metode = v; actualitza(); });

    actualitza();
  }
})();

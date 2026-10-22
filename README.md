# Eleccions al Congrés 2023: un sistema proporcional amb llista única

Què hauria passat el 23 de juliol de 2023 si els 350 escons del Congrés s'haguessin repartit amb un sistema proporcional?

Aquest projecte compara el **mètode d'Hondt actual** amb un **sistema proporcional amb llista única** que manté la representació territorial de cada província segons la seva població. El visor reprodueix el format de les pàgines de resultats del diari ARA.

**[Veure el visor](https://ivalls23.github.io/Sistema-electoral/)**

## El sistema proporcional

1. **Escons per província.** Cada província rep escons segons la seva població (padró oficial de l'INE), amb el mètode de Sainte-Laguë pur i un mínim d'un escó per província.
2. **Escons per partit.** Tot l'Estat compta com una sola circumscripció. Entren els partits amb almenys 1/350 dels vots vàlids (vots a candidatures més vots en blanc) i els 350 escons es reparteixen amb Sainte-Laguë pur.
3. **Escons de cada partit a cada província.** S'aplica el **mètode biproporcional** (Balinski i Demange, conegut com a "doble Pukelsheim"), que es fa servir en diversos cantons suïssos. Troba un divisor per a cada província i un per a cada partit de manera que tant els totals de les províncies com els dels partits quadrin exactament. Un partit mai obté escons en una província on no s'ha presentat.
4. **Assegurança de capacitat.** Si un grup de partits tingués dret a més escons dels que sumen les províncies on es presenta, rep exactament els escons del seu territori i la resta es reparteix entre els altres partits. Es detecta automàticament amb un problema de flux màxim.

## Fonts de dades

- **Resultats electorals:** Ministerio del Interior, via el paquet [`infoelectoral`](https://github.com/rOpenSpain/infoelectoral).
- **Població:** Institut Nacional d'Estadística (INE), taules 2852 i 67988, via la seva API.
- **Geometries:** Instituto Geográfico Nacional (SIANE), via el paquet [`mapSpain`](https://github.com/rOpenSpain/mapSpain).

## Com reproduir-ho

Cal R (≥ 4.2) i Quarto (inclòs a RStudio). Instal·la els paquets:

```r
install.packages(c("infoelectoral", "dplyr", "tidyr", "purrr", "stringr", "httr2",
                   "ggplot2", "ggiraph", "htmltools", "sf", "mapSpain", "jsonlite"))
```

Executa els scripts per ordre des de l'arrel del projecte:

```r
source("R/01_resultats.R")     # resultats del Ministerio
source("R/02_poblacio.R")      # població de l'INE
source("R/03_dhondt.R")        # d'Hondt actual, validat contra el resultat oficial
source("R/04_proporcional.R")  # sistema proporcional
source("R/05_dades_visor.R")   # dades estatals del visor
source("R/06_dades_mapa.R")    # mapa, comunitats i pactòmetre
```

Després, genera el visor amb `quarto render index.qmd` (o el botó **Render** de RStudio).

## Estructura

| Fitxer | Contingut |
|---|---|
| `R/funcions_repartiment.R` | d'Hondt, Sainte-Laguë, biproporcional i assegurança de capacitat |
| `R/01` a `R/06` | descàrrega de dades, càlculs i preparació del visor |
| `R/partits.R` | noms curts, colors i ordre dels partits a l'hemicicle |
| `R/grafics.R` | hemicicle, aliances, comparativa, taula i targetes |
| `index.qmd`, `estils.css`, `visor.js` | la pàgina web, el seu estil i el mapa i el pactòmetre |

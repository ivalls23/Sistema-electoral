# =============================================================================
# partits.R
# Noms curts, colors (inspirats en els de l'ARA) i ordre a l'hemicicle.
# `partit` ha de coincidir amb les sigles que surten a les taules estatals.
# `ordre` va d'esquerra a dreta de l'hemicicle: canvia'l si ho vols diferent.
# =============================================================================

library(tibble)

partits_info <- tribble(
  ~partit,          ~nom,    ~color,    ~ordre,
  "CUP-PR",         "CUP",   "#FFD400",  1,
  "EH Bildu",       "Bildu", "#C9D82B",  2,
  "B.N.G.",         "BNG",   "#8AEFF5",  3,
  "ERC",            "ERC",   "#EBC2A3",  4,
  "SUMAR",          "Sumar", "#E5175F",  5,
  "PACMA",          "PACMA", "#A9D88B",  6,
  "PSOE",           "PSOE",  "#C01D2E",  7,
  "JxCAT - JUNTS",  "Junts", "#00C5B0",  8,
  "EAJ-PNV",        "PNB",   "#2E8B1E",  9,
  "CCa",            "CC",    "#5BC2EF", 10,
  "U.P.N.",         "UPN",   "#5A5A5A", 11,
  "PP",             "PP",    "#4FA3F7", 12,
  "VOX",            "Vox",   "#7DBE2A", 13
)

color_altres <- "#B9BFC7"
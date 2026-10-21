# =============================================================================
# 01_resultats.R
# Descarrega els resultats oficials del Congrés (Ministerio del Interior)
# a nivell de província i els desa a data/
# =============================================================================

library(infoelectoral)
library(dplyr)

descarrega_congres <- function(anno, mes) {
  message("Descarregant Congrés ", anno, "-", mes, " ...")
  df <- provincias(tipo_eleccion = "congreso", anno = anno, mes = mes)
  if (is.null(df)) stop("No s'ha pogut descarregar. Comprova la connexió o la data.")

  # El fitxer inclou files de totals de comunitat i del total estatal
  # (codigo_provincia == "99"). Ens quedem només amb les 52 circumscripcions.
  df <- df |> filter(codigo_provincia != "99")

  # --- Taula 1: una fila per província (participació, blancs, nuls, escons) ---
  resum <- df |>
    distinct(codigo_ccaa, codigo_provincia, ambito_territorial,
             poblacion_derecho, censo_escrutinio, censo_cere,
             votos_blancos, votos_nulos, votos_candidaturas, n_diputados) |>
    rename(provincia = ambito_territorial)

  # --- Taula 2: una fila per província i candidatura ---
  vots <- df |>
    filter(!is.na(votos), votos > 0) |>
    select(codigo_ccaa, codigo_provincia, provincia = ambito_territorial,
           codigo_partido, codigo_partido_nacional, siglas, denominacion,
           votos, diputados_oficials = diputados) |>
    # Codi d'agrupació estatal: el Ministerio ja agrupa, p. ex., PSC amb PSOE.
    # Si ve buit, fem servir el codi de la pròpia candidatura.
    mutate(grup_estatal = if_else(
      is.na(codigo_partido_nacional) | codigo_partido_nacional %in% c("", "000000"),
      codigo_partido, codigo_partido_nacional
    ))

  stopifnot(nrow(resum) == 52)
  list(resum = resum, vots = vots)
}

dir.create("data", showWarnings = FALSE)

for (eleccio in list(c("2023", "07"), c("2019", "11"))) {
  r <- descarrega_congres(eleccio[1], eleccio[2])
  saveRDS(r$resum, sprintf("data/resum_prov_%s.rds", eleccio[1]))
  saveRDS(r$vots,  sprintf("data/vots_prov_%s.rds",  eleccio[1]))
}

# Comprovació ràpida
resum23 <- readRDS("data/resum_prov_2023.rds")
cat("\nProvíncies:", nrow(resum23),
    "| Escons:", sum(resum23$n_diputados),
    "| Vots a candidatures:", format(sum(resum23$votos_candidaturas), big.mark = "."),
    "\n")

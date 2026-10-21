# =============================================================================
# 05_dades_visor.R
# Ajunta els resultats dels dos mètodes en un sol objecte per al visor Quarto.
# Executa'l cada vegada que tornis a calcular el 03 o el 04.
# =============================================================================
install.packages(c("ggplot2", "ggiraph", "htmltools"))
library(dplyr)
library(tidyr)
source("R/partits.R")

any_eleccio <- "2023"

resum      <- readRDS(sprintf("data/resum_prov_%s.rds", any_eleccio)) |> as_tibble()
nou_estat  <- readRDS(sprintf("data/resultat_nou_estat_%s.rds", any_eleccio))

vots_valids <- sum(resum$votos_candidaturas + resum$votos_blancos)

# --- Resultat estatal amb els dos mètodes ------------------------------------
estat <- nou_estat |>
  transmute(grup_estatal, partit, vots,
            pct = 100 * vots / vots_valids,
            escons_nou, escons_dhondt = replace_na(as.integer(escons_dhondt), 0L)) |>
  left_join(partits_info, by = "partit") |>
  mutate(nom   = coalesce(nom, partit),
         color = coalesce(color, color_altres),
         ordre = coalesce(ordre, 99)) |>
  arrange(desc(vots))

# --- Xifres generals ------------------------------------------------------------
totals <- list(
  vots_valids    = vots_valids,
  vots_blancs    = sum(resum$votos_blancos),
  vots_nuls      = sum(resum$votos_nulos),
  vots_emesos    = vots_valids + sum(resum$votos_nulos),
  llindar_vots   = ceiling(vots_valids / 350),
  majoria        = 176,
  total_escons   = 350
)

# Comprovació: tots els partits amb escons han de tenir color propi
sense_color <- estat |>
  filter((escons_nou > 0 | escons_dhondt > 0), color == color_altres)
if (nrow(sense_color) > 0) {
  warning("Partits amb escons sense color definit a R/partits.R: ",
          paste(sense_color$partit, collapse = ", "))
}

saveRDS(list(estat = estat, totals = totals),
        sprintf("data/visor_%s.rds", any_eleccio))

cat("Dades del visor desades. Escons proporcional:", sum(estat$escons_nou),
    "| d'Hondt:", sum(estat$escons_dhondt), "\n")
# =============================================================================
# 02_poblacio.R
# Descarrega la població oficial per província de l'INE (API Tempus3)
#
# L'INE té la sèrie partida en dues taules:
#   - 2852  : Revisió del Padró, fins a 1-1-2021
#   - 67988 : xifres de 2022 en endavant
# Les descarreguem totes dues i les ajuntem.
#
# Per a cada elecció cal la xifra oficial vigent quan es convoca:
#   - Generals juliol 2023    -> padró a 1-1-2022
#   - Generals novembre 2019  -> padró a 1-1-2018
# =============================================================================

library(httr2)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

valors_total <- c("Total", "Ambos sexos", "Total Nacional", "Total nacional",
                  "Todas las edades", "Total edades")

descarrega_taula_ine <- function(id_taula, n_anys = 40) {
  message("Descarregant taula INE ", id_taula, " ...")
  resp <- request(paste0("https://servicios.ine.es/wstempus/js/ES/DATOS_TABLA/", id_taula)) |>
    req_url_query(tip = "AM", nult = n_anys) |>
    req_timeout(120) |>
    req_perform() |>
    resp_body_json()
  if (length(resp) == 0) stop("La taula ", id_taula, " no ha retornat cap sèrie.")
  
  # Metadades en format llarg: una fila per sèrie i variable
  meta <- imap_dfr(resp, function(s, i) {
    md <- s$MetaData
    tibble(
      serie    = i,
      variable = map_chr(md, ~ .x$T3_Variable %||% .x$Variable$Nombre %||% NA_character_),
      valor    = map_chr(md, ~ .x$Nombre %||% NA_character_),
      codi     = map_chr(md, ~ .x$Codigo %||% NA_character_)
    )
  })
  
  # Dades: una fila per sèrie i any
  dades <- imap_dfr(resp, function(s, i) {
    tibble(
      serie    = i,
      any      = map_int(s$Data, ~ as.integer(.x$Anyo)),
      poblacio = map_dbl(s$Data, ~ as.numeric(.x$Valor %||% NA))
    )
  })
  
  # Diagnòstic: quines variables té la taula i quants valors diferents
  cat("\nVariables de la taula", id_taula, ":\n")
  meta |> group_by(variable) |>
    summarise(n_valors = n_distinct(valor),
              exemples = paste(head(unique(valor), 4), collapse = " | ")) |>
    print()
  
  # Variables que són desglossaments (més d'un valor), sense comptar Provincias.
  # D'aquestes, només volem el valor "Total".
  desglossos <- meta |>
    filter(variable != "Provincias") |>
    group_by(variable) |>
    filter(n_distinct(valor) > 1) |>
    ungroup()
  
  series_descartades <- desglossos |>
    filter(!valor %in% valors_total) |>
    distinct(serie)
  
  prov <- meta |>
    filter(variable == "Provincias") |>
    select(serie, territori = valor, codigo_provincia = codi)
  
  dades |>
    anti_join(series_descartades, by = "serie") |>
    left_join(prov, by = "serie") |>
    mutate(
      # El total estatal pot venir sense variable Provincias o com a "Total Nacional"
      es_total = is.na(territori) | territori %in% valors_total,
      codigo_provincia = if_else(es_total, NA_character_, codigo_provincia),
      territori = if_else(es_total, "Total Nacional", territori),
      taula = id_taula
    ) |>
    select(taula, codigo_provincia, territori, any, poblacio)
}

pob_ine <- bind_rows(
  descarrega_taula_ine(2852),
  descarrega_taula_ine(67988)
) |>
  filter(!is.na(poblacio)) |>
  # Si un any apareix a les dues taules, prioritzem la més nova
  arrange(desc(taula)) |>
  distinct(codigo_provincia, any, .keep_all = TRUE)

pob_prov <- pob_ine |>
  filter(!is.na(codigo_provincia)) |>
  mutate(codigo_provincia = str_pad(codigo_provincia, 2, pad = "0"))

pob_total <- pob_ine |>
  filter(is.na(codigo_provincia)) |>
  select(any, poblacio_espanya = poblacio)

saveRDS(pob_prov,  "data/poblacio_provincies.rds")
saveRDS(pob_total, "data/poblacio_espanya.rds")

# Comprovació: 52 províncies per any i la suma ha de coincidir amb el total
pob_prov |>
  group_by(any) |>
  summarise(n = n(), suma = sum(poblacio)) |>
  left_join(pob_total, by = "any") |>
  arrange(desc(any)) |>
  print(n = 15)
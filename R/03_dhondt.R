# =============================================================================
# 03_dhondt.R
# Calcula el repartiment actual (d'Hondt per província) i el valida
# contra els resultats oficials. Si tot quadra, el pipeline és correcte.
# =============================================================================

library(dplyr)
source("R/funcions_repartiment.R")

any_eleccio <- "2023"
any_padro   <- 2022          # 2019 -> 2018

resum    <- readRDS(sprintf("data/resum_prov_%s.rds", any_eleccio))
vots     <- readRDS(sprintf("data/vots_prov_%s.rds",  any_eleccio))
pob_prov <- readRDS("data/poblacio_provincies.rds")

# -----------------------------------------------------------------------------
# VALIDACIÓ 1: escons per província a partir de la població (LOREG art. 162)
# -----------------------------------------------------------------------------
escons_prov <- resum |>
  left_join(pob_prov |> filter(any == any_padro) |> select(codigo_provincia, poblacio),
            by = "codigo_provincia") |>
  mutate(escons_ine      = escons_per_provincia(poblacio, codigo_provincia),
         escons_ministeri = escons_per_provincia(poblacion_derecho, codigo_provincia))

cat("\n== Validació 1: escons per província ==\n")
cat("Amb població INE", any_padro, "-> diferències:",
    sum(escons_prov$escons_ine != escons_prov$n_diputados, na.rm = TRUE), "\n")
cat("Amb població del fitxer del Ministerio -> diferències:",
    sum(escons_prov$escons_ministeri != escons_prov$n_diputados), "\n")

# -----------------------------------------------------------------------------
# VALIDACIÓ 2: d'Hondt per província
# Llindar 3% sobre vots vàlids = vots a candidatures + vots en blanc
# -----------------------------------------------------------------------------
resultat <- vots |>
  left_join(resum |> select(codigo_provincia, n_diputados,
                            votos_candidaturas, votos_blancos),
            by = "codigo_provincia") |>
  group_by(codigo_provincia) |>
  mutate(vots_valids   = first(votos_candidaturas) + first(votos_blancos),
         escons_dhondt = dhondt(votos, first(n_diputados),
                                llindar = 0.03, vots_valids = first(vots_valids))) |>
  ungroup()

discrepancies <- resultat |> filter(escons_dhondt != diputados_oficials)
cat("\n== Validació 2: d'Hondt per província ==\n")
cat("Files amb discrepància:", nrow(discrepancies), "\n")
if (nrow(discrepancies) > 0) print(discrepancies |>
  select(provincia, siglas, votos, escons_dhondt, diputados_oficials))

# -----------------------------------------------------------------------------
# Resultat estatal (agrupant candidatures amb el codi estatal del Ministerio)
# -----------------------------------------------------------------------------
vots_valids_estat <- sum(resum$votos_candidaturas + resum$votos_blancos)

estatal <- resultat |>
  group_by(grup_estatal) |>
  summarise(partit  = siglas[which.max(votos)],
            vots    = sum(votos),
            escons  = sum(escons_dhondt),
            oficials = sum(diputados_oficials),
            .groups = "drop") |>
  mutate(pct = round(100 * vots / vots_valids_estat, 2)) |>
  arrange(desc(escons), desc(vots))

cat("\n== Resultat estatal (d'Hondt) ==\n")
print(estatal |> filter(escons > 0 | pct >= 0.5), n = 30)
cat("\nTotal escons:", sum(estatal$escons), "\n")

saveRDS(resultat, sprintf("data/resultat_dhondt_prov_%s.rds", any_eleccio))
saveRDS(estatal,  sprintf("data/resultat_dhondt_estat_%s.rds", any_eleccio))

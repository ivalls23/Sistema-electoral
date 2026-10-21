# =============================================================================
# 04_proporcional.R
# Sistema proporcional amb llista única i repartiment biproporcional
#
#   1. Escons per província: Sainte-Laguë pur sobre la població, mínim 1
#   2. Escons per partit:    Sainte-Laguë pur sobre vots estatals,
#                            llindar 1/350 dels vots vàlids (blancs inclosos)
#   3. Partit x província:   biproporcional amb arrodoniment Sainte-Laguë
# =============================================================================

library(dplyr)
library(tidyr)
source("R/funcions_repartiment.R")

any_eleccio <- "2023"
any_padro   <- 2022
total_escons <- 350

# as_tibble(): infoelectoral retorna data.frame, i print(n = ...) només funciona amb tibbles
resum    <- readRDS(sprintf("data/resum_prov_%s.rds", any_eleccio)) |> as_tibble()
vots     <- readRDS(sprintf("data/vots_prov_%s.rds",  any_eleccio)) |> as_tibble()
pob_prov <- readRDS("data/poblacio_provincies.rds") |> filter(any == any_padro)
dhondt_estat <- readRDS(sprintf("data/resultat_dhondt_estat_%s.rds", any_eleccio))

# -----------------------------------------------------------------------------
# 1. Escons per província segons la població
# -----------------------------------------------------------------------------
provincies <- resum |>
  select(codigo_ccaa, codigo_provincia, provincia, escons_dhondt = n_diputados) |>
  left_join(pob_prov |> select(codigo_provincia, poblacio), by = "codigo_provincia") |>
  arrange(codigo_provincia) |>
  mutate(escons_nou = sainte_lague(poblacio, total_escons, minim = 1),
         diferencia = escons_nou - escons_dhondt)

cat("\n== 1. Escons per província (població", any_padro, ") ==\n")
provincies |>
  filter(diferencia != 0) |>
  arrange(desc(diferencia)) |>
  select(provincia, poblacio, escons_dhondt, escons_nou, diferencia) |>
  print(n = 52)

# -----------------------------------------------------------------------------
# 2. Escons per partit (llista única estatal)
# -----------------------------------------------------------------------------
vots_valids_estat <- sum(resum$votos_candidaturas + resum$votos_blancos)
llindar <- 1 / total_escons

partits <- vots |>
  group_by(grup_estatal) |>
  summarise(partit = siglas[which.max(votos)], vots = sum(votos), .groups = "drop") |>
  mutate(pct = 100 * vots / vots_valids_estat,
         supera_llindar = vots >= llindar * vots_valids_estat) |>
  arrange(desc(vots))

# Matriu província x partit amb els partits que superen el llindar
partits_llindar <- partits |> filter(supera_llindar)

V <- vots |>
  filter(grup_estatal %in% partits_llindar$grup_estatal) |>
  group_by(codigo_provincia, grup_estatal) |>
  summarise(votos = sum(votos), .groups = "drop") |>
  pivot_wider(names_from = grup_estatal, values_from = votos, values_fill = 0) |>
  right_join(provincies |> select(codigo_provincia), by = "codigo_provincia") |>
  arrange(codigo_provincia) |>
  mutate(across(-codigo_provincia, ~ replace_na(.x, 0)))

mat <- as.matrix(V[, partits_llindar$grup_estatal])
rownames(mat) <- V$codigo_provincia

# Sainte-Laguë estatal + assegurança de capacitat
assegurat <- escons_partits_assegurats(mat, provincies$escons_nou)

partits <- partits |>
  left_join(tibble(grup_estatal = names(assegurat$escons), escons_nou = assegurat$escons),
            by = "grup_estatal") |>
  mutate(escons_nou = replace_na(escons_nou, 0L)) |>
  left_join(dhondt_estat |> select(grup_estatal, escons_dhondt = escons), by = "grup_estatal") |>
  arrange(desc(escons_nou), desc(vots))

cat("\n== 2. Resultat estatal ==\n")
cat("Vots vàlids:", format(vots_valids_estat, big.mark = ".", decimal.mark = ","),
    "| Llindar (1/350):", format(ceiling(llindar * vots_valids_estat), big.mark = ".", decimal.mark = ","),
    "vots\n")
partits |>
  filter(escons_nou > 0 | escons_dhondt > 0 | pct >= 0.2) |>
  mutate(pct = round(pct, 2), diferencia = escons_nou - escons_dhondt) |>
  select(partit, vots, pct, escons_dhondt, escons_nou, diferencia) |>
  print(n = 40)

cat("\nAssegurança de capacitat:",
    if (is.null(assegurat$ajustos)) "no ha calgut activar-la\n" else "ACTIVADA\n")
if (!is.null(assegurat$ajustos)) print(assegurat$ajustos)

# -----------------------------------------------------------------------------
# 3. Repartiment biproporcional partit x província
# -----------------------------------------------------------------------------
partits_amb_escons <- partits |> filter(escons_nou > 0)
mat <- assegurat$V[, partits_amb_escons$grup_estatal, drop = FALSE]

bp <- biproporcional(mat,
                     escons_fila = provincies$escons_nou,
                     escons_col  = partits_amb_escons$escons_nou)

cat("\n== 3. Biproporcional ==\n")
cat("Convergència en", bp$iteracions, "iteracions\n")

# Resultat en format llarg, el mateix que el del d'Hondt
noms_partit <- setNames(partits$partit, partits$grup_estatal)
noms_prov   <- setNames(provincies$provincia, provincies$codigo_provincia)

resultat_nou <- as.data.frame(as.table(bp$escons), stringsAsFactors = FALSE) |>
  setNames(c("codigo_provincia", "grup_estatal", "escons_nou")) |>
  right_join(vots |> select(codigo_ccaa, codigo_provincia, provincia, grup_estatal,
                            siglas, votos, diputados_oficials),
             by = c("codigo_provincia", "grup_estatal")) |>
  mutate(escons_nou = replace_na(as.integer(escons_nou), 0L),
         partit = unname(noms_partit[grup_estatal])) |>
  as_tibble()

# Comprovacions
cat("Escons totals:", sum(resultat_nou$escons_nou), "\n")
cat("Escons en províncies on el partit no es presentava:",
    sum(bp$escons[mat == 0]), "\n")

inv <- inversions(mat, bp$escons)
cat("Inversions (més vots però menys escons dins una província):",
    if (is.null(inv)) 0 else nrow(inv), "\n")
if (!is.null(inv)) {
  inv |>
    mutate(provincia  = unname(noms_prov[provincia]),
           mes_vots   = unname(noms_partit[mes_vots]),
           menys_vots = unname(noms_partit[menys_vots])) |>
    as_tibble() |>
    print(n = 50)
}

# Exemple: partits territorials, on han tret els escons
cat("\nOn treuen escons els partits territorials:\n")
resultat_nou |>
  filter(escons_nou > 0, partit %in% c("ERC", "JxCAT - JUNTS", "EH Bildu", "EAJ-PNV", "B.N.G.", "CCa")) |>
  select(partit, provincia, votos, escons_nou) |>
  arrange(partit, desc(escons_nou)) |>
  print(n = 40)

# -----------------------------------------------------------------------------
# Desar
# -----------------------------------------------------------------------------
saveRDS(provincies,   sprintf("data/escons_provincia_nou_%s.rds", any_eleccio))
saveRDS(partits,      sprintf("data/resultat_nou_estat_%s.rds", any_eleccio))
saveRDS(resultat_nou, sprintf("data/resultat_nou_prov_%s.rds", any_eleccio))
saveRDS(bp,           sprintf("data/biproporcional_%s.rds", any_eleccio))
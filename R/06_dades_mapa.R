# =============================================================================
# 06_dades_mapa.R
# Prepara les dades territorials del visor:
#   - resultats per província, comunitat i municipi amb els dos mètodes
#   - geometries simplificades (mapSpain, dades oficials del SIANE)
#   - tot desat a data/mapa_2023.rds per al mapa, les targetes i el pactòmetre
# Executa'l després del 05. La primera vegada triga uns minuts (descàrregues).
# =============================================================================
install.packages(c("sf", "mapSpain", "jsonlite"))
library(dplyr)
library(tidyr)
library(sf)
library(mapSpain)
library(infoelectoral)
library(jsonlite)
source("R/partits.R")
sf_use_s2(FALSE)   # geometria plana: més tolerant amb polígons imperfectes

any_eleccio <- "2023"
mes         <- "07"
data_eleccio <- "2023-07-23"

visor <- readRDS(sprintf("data/visor_%s.rds", any_eleccio))
resum <- readRDS(sprintf("data/resum_prov_%s.rds", any_eleccio)) |> as_tibble()
dh    <- readRDS(sprintf("data/resultat_dhondt_prov_%s.rds", any_eleccio)) |> as_tibble()
nou   <- readRDS(sprintf("data/resultat_nou_prov_%s.rds", any_eleccio)) |> as_tibble()

# Nom curt i color estatal de cada candidatura (PSC -> color del PSOE, etc.)
info_grup <- visor$estat |> select(grup_estatal, nom_estatal = nom, color)

# Noms de les comunitats en català (codis INE, que són els de mapSpain)
noms_ccaa <- c(
  "01" = "Andalusia", "02" = "Aragó", "03" = "Astúries", "04" = "Illes Balears",
  "05" = "Canàries", "06" = "Cantàbria", "07" = "Castella i Lleó",
  "08" = "Castella-la Manxa", "09" = "Catalunya", "10" = "País Valencià",
  "11" = "Extremadura", "12" = "Galícia", "13" = "Madrid", "14" = "Múrcia",
  "15" = "Navarra", "16" = "País Basc", "17" = "La Rioja", "18" = "Ceuta", "19" = "Melilla"
)

# -----------------------------------------------------------------------------
# 1. Geometries (coordenades reals per al mapa web, Canàries al seu lloc)
# -----------------------------------------------------------------------------
message("Descarregant geometries ...")
geo_prov <- esp_get_prov_siane(year = data_eleccio, epsg = 4326, resolution = 3, moveCAN = FALSE)
geo_ccaa <- esp_get_ccaa_siane(year = data_eleccio, epsg = 4326, resolution = 3, moveCAN = FALSE)
geo_mun  <- esp_get_munic_siane(year = data_eleccio, epsg = 4326, resolution = 3, moveCAN = FALSE)

# Simplificació: menys vèrtexs perquè la pàgina pesi poc (tolerància en metres)
simplifica <- function(g, metres) {
  g |>
    st_transform(3857) |>
    st_make_valid() |>
    st_simplify(preserveTopology = TRUE, dTolerance = metres) |>
    st_transform(4326)
}
geo_prov <- simplifica(geo_prov, 400)
geo_ccaa <- simplifica(geo_ccaa, 500)
geo_mun  <- simplifica(geo_mun, 350)

prov_ccaa <- geo_prov |> st_drop_geometry() |> distinct(codigo_provincia = cpro, codauto)

# -----------------------------------------------------------------------------
# 2. Resultats per província i partit, amb els dos mètodes
# -----------------------------------------------------------------------------
valids_prov <- resum |>
  transmute(codigo_provincia, provincia, valids = votos_candidaturas + votos_blancos)

escons_nou <- nou |> distinct(codigo_provincia, grup_estatal, escons_nou)

prov_partit <- dh |>
  group_by(codigo_provincia, grup_estatal) |>
  summarise(sigles = siglas[which.max(votos)], vots = sum(votos),
            escons_dhondt = sum(escons_dhondt), .groups = "drop") |>
  left_join(escons_nou, by = c("codigo_provincia", "grup_estatal")) |>
  mutate(escons_nou = replace_na(as.integer(escons_nou), 0L)) |>
  left_join(valids_prov, by = "codigo_provincia") |>
  left_join(prov_ccaa, by = "codigo_provincia") |>
  left_join(info_grup, by = "grup_estatal") |>
  mutate(pct = 100 * vots / valids,
         nom_estatal = coalesce(nom_estatal, sigles),
         color = coalesce(color, color_altres))

# -----------------------------------------------------------------------------
# 3. Resultats per comunitat (suma de les seves províncies)
# -----------------------------------------------------------------------------
ccaa_partit <- prov_partit |>
  group_by(codauto, grup_estatal, nom_estatal, color) |>
  summarise(sigles = sigles[which.max(vots)], vots = sum(vots),
            escons_dhondt = sum(escons_dhondt), escons_nou = sum(escons_nou),
            .groups = "drop") |>
  left_join(prov_partit |> distinct(codigo_provincia, codauto, valids) |>
              group_by(codauto) |> summarise(valids = sum(valids)),
            by = "codauto") |>
  mutate(pct = 100 * vots / valids, comunitat = unname(noms_ccaa[codauto]))

# -----------------------------------------------------------------------------
# 4. Resultats per municipi (només vots: als municipis no hi ha escons)
# -----------------------------------------------------------------------------
message("Descarregant resultats municipals ...")
mun_raw <- municipios(tipo_eleccion = "congreso", anno = any_eleccio, mes = mes) |>
  as_tibble() |>
  filter(codigo_municipio != "999")          # 999 = vot de l'estranger (CERA)

grup_de_partit <- dh |> distinct(siglas, grup_estatal)

mun_partit <- mun_raw |>
  mutate(id = paste0(codigo_provincia, codigo_municipio),
         valids = votos_candidaturas + votos_blancos) |>
  filter(!is.na(votos), votos > 0) |>
  group_by(id, municipio, siglas) |>
  summarise(vots = sum(votos), valids = first(valids), .groups = "drop") |>
  left_join(grup_de_partit |> distinct(siglas, .keep_all = TRUE), by = "siglas") |>
  left_join(info_grup, by = "grup_estatal") |>
  mutate(pct = 100 * vots / valids,
         sigles = siglas,
         nom_estatal = coalesce(nom_estatal, siglas),
         color = coalesce(color, color_altres),
         escons_dhondt = NA_integer_, escons_nou = NA_integer_)

# -----------------------------------------------------------------------------
# 5. Guanyador de cada territori i taula del requadre
# -----------------------------------------------------------------------------
color_empat <- "#8A8F98"

#' Guanyador: més escons (o més vots als municipis); si empaten, "Empat".
guanyador <- function(df, criteri) {
  df |>
    group_by(id) |>
    arrange(desc(.data[[criteri]]), desc(vots), .by_group = TRUE) |>
    summarise(
      # Empat: el primer i el segon igualen en el criteri i també en vots
      empat  = n() > 1 && .data[[criteri]][1] == .data[[criteri]][2] && vots[1] == vots[2],
      guanya = if (empat) "Empat" else nom_estatal[1],
      color  = if (empat) color_empat else color[1],
      .groups = "drop"
    ) |>
    select(-empat)
}

#' Taula del requadre: els partits més votats, en JSON compacte.
#' Cada fila: [sigles, vots, %] i, si n'hi ha, [escons d'Hondt, escons proporcional].
#' El color de cada sigla va a part (diccionari `colors`) perquè no es repeteixi.
files_json <- function(df, n_files, amb_escons) {
  df |>
    group_by(id) |>
    arrange(desc(vots), .by_group = TRUE) |>
    slice_head(n = n_files) |>
    summarise(files = toJSON(
      lapply(seq_len(n()), function(k) {
        fila <- list(sigles[k], vots[k], round(pct[k], 1))
        if (amb_escons) c(fila, escons_dhondt[k], escons_nou[k]) else fila
      }),
      auto_unbox = TRUE), .groups = "drop")
}

prepara_capa <- function(geo, df, criteri_nou, criteri_dhondt,
                         n_files = 6, amb_escons = TRUE) {
  g_nou <- guanyador(df, criteri_nou) |> rename(gn = guanya, cn = color)
  g_dh  <- guanyador(df, criteri_dhondt) |> rename(gd = guanya, cd = color)
  geo |>
    left_join(g_nou, by = "id") |>
    left_join(g_dh, by = "id") |>
    left_join(files_json(df, n_files, amb_escons), by = "id")
}

capa_prov <- prepara_capa(
  geo_prov |> transmute(id = cpro),
  prov_partit |> mutate(id = codigo_provincia),
  "escons_nou", "escons_dhondt"
) |>
  left_join(valids_prov |> transmute(id = codigo_provincia, nom = provincia), by = "id")

capa_ccaa <- prepara_capa(
  geo_ccaa |> transmute(id = codauto),
  ccaa_partit |> mutate(id = codauto),
  "escons_nou", "escons_dhondt"
) |>
  mutate(nom = unname(noms_ccaa[id]))

capa_mun <- prepara_capa(
  geo_mun |> transmute(id = LAU_CODE, nom = name),
  mun_partit,
  "vots", "vots", n_files = 5, amb_escons = FALSE
) |>
  filter(!is.na(files)) |>
  select(-gd, -cd)          # als municipis el guanyador és el mateix amb els dos mètodes

# -----------------------------------------------------------------------------
# 6. Desar
# -----------------------------------------------------------------------------
a_geojson <- function(x, decimals = 4) {
  f <- tempfile(fileext = ".geojson")
  st_write(x, f, driver = "GeoJSON", quiet = TRUE,
           layer_options = c(paste0("COORDINATE_PRECISION=", decimals), "RFC7946=YES"))
  # minify() treu els espais: el fitxer passa a pesar gairebé la meitat
  as.character(minify(paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "")))
}

colors <- bind_rows(prov_partit, ccaa_partit, mun_partit) |>
  distinct(sigles, color) |>
  distinct(sigles, .keep_all = TRUE)

mapa <- list(
  colors   = as.character(toJSON(setNames(as.list(colors$color), colors$sigles), auto_unbox = TRUE)),
  geo_prov = a_geojson(capa_prov),
  geo_ccaa = a_geojson(capa_ccaa),
  geo_mun  = a_geojson(capa_mun, decimals = 3),   # ~100 m de precisió, suficient per a municipis
  ccaa_partit = ccaa_partit,
  prov_partit = prov_partit
)
saveRDS(mapa, sprintf("data/mapa_%s.rds", any_eleccio))

cat("Mapa desat.",
    "Províncies:", nrow(capa_prov), "| Comunitats:", nrow(capa_ccaa),
    "| Municipis:", nrow(capa_mun), "\n",
    "Mida aproximada (MB):", round(sum(nchar(unlist(mapa[c("geo_prov", "geo_ccaa", "geo_mun")]), type = "bytes")) / 1e6, 1), "\n")
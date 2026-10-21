# =============================================================================
# grafics.R
# Funcions gràfiques del visor
# =============================================================================

library(dplyr)
library(ggplot2)
library(ggiraph)
library(htmltools)

# Format català: 24.688.087 i 33,1%
num <- function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE)
pct <- function(x, d = 2) paste0(formatC(x, format = "f", digits = d, decimal.mark = ","), "%")

etiqueta_metode <- c(nou = "Mètode proporcional", dhondt = "Mètode d'Hondt")

# -----------------------------------------------------------------------------
# Hemicicle doble
# -----------------------------------------------------------------------------

#' Polígons dels arcs d'un anell: un sector per partit, d'esquerra a dreta
arcs_anell <- function(df, col_escons, r_int, r_ext, anell, total = 350, n = 80) {
  df <- df |> filter(.data[[col_escons]] > 0) |> arrange(ordre)
  fi  <- cumsum(df[[col_escons]])
  ini <- fi - df[[col_escons]]
  
  bind_rows(lapply(seq_len(nrow(df)), function(k) {
    theta <- seq(pi * (1 - ini[k] / total), pi * (1 - fi[k] / total), length.out = n)
    tibble(
      x = c(r_ext * cos(theta), rev(r_int * cos(theta))),
      y = c(r_ext * sin(theta), rev(r_int * sin(theta))),
      partit = df$partit[k], nom = df$nom[k], color = df$color[k],
      escons = df[[col_escons]][k], pct = df$pct[k], anell = anell
    )
  }))
}

#' Hemicicle amb el mètode proporcional (anell exterior) i el d'Hondt (interior).
#' L'anell del mètode actiu es mostra amb color ple; l'altre, esvaït.
#' hemicicle_gg() construeix el gràfic ggplot; hemicicle() el fa interactiu.
hemicicle_gg <- function(estat, actiu = c("nou", "dhondt"), totals) {
  actiu <- match.arg(actiu)
  poligons <- bind_rows(
    arcs_anell(estat, "escons_nou",    0.64, 1.00, "nou"),
    arcs_anell(estat, "escons_dhondt", 0.30, 0.56, "dhondt")
  ) |>
    mutate(
      alpha = if_else(anell == actiu, 1, 0.38),
      id = paste(partit, anell),
      tooltip = sprintf(
        "<b>%s</b><br>%s<br>%d escons<br>%s dels vots",
        nom, etiqueta_metode[anell], escons, pct(pct)
      )
    )
  
  peu <- -0.075
  p <- ggplot(poligons) +
    geom_polygon_interactive(
      aes(x, y, group = id, fill = color, alpha = alpha,
          tooltip = tooltip, data_id = partit),
      colour = "white", linewidth = 0.5
    ) +
    scale_fill_identity() +
    scale_alpha_identity() +
    # Línia de la majoria absoluta
    annotate("segment", x = 0, xend = 0, y = 0.02, yend = 1.06,
             linetype = "22", linewidth = 0.3, colour = "#555555") +
    annotate("text", x = -0.82, y = peu, label = "Proporcional", size = 3.4, colour = "#333333") +
    annotate("text", x = -0.43, y = peu, label = "d'Hondt", size = 3.4, colour = "#333333") +
    annotate("text", x = 0, y = peu, label = totals$majoria, size = 4.2, fontface = "bold") +
    annotate("text", x = 0, y = peu - 0.07, label = "Majoria absoluta", size = 3.2) +
    annotate("text", x = 0.86, y = peu, label = totals$total_escons, size = 4.2, fontface = "bold") +
    annotate("text", x = 0.86, y = peu - 0.07, label = "Diputats", size = 3.2) +
    coord_fixed(xlim = c(-1.02, 1.02), ylim = c(-0.2, 1.07), expand = FALSE) +
    theme_void()
  p
}

hemicicle <- function(estat, actiu = c("nou", "dhondt"), totals) {
  p <- hemicicle_gg(estat, actiu, totals)
  girafe(
    ggobj = p, width_svg = 8, height_svg = 5,
    options = list(
      opts_hover(css = "stroke:#111111;stroke-width:1.2px;"),
      opts_hover_inv(css = "opacity:0.35;"),
      opts_tooltip(css = paste(
        "background:#ffffff;color:#111111;border:1px solid #c9ced6;",
        "padding:6px 9px;font-family:Inter,Arial,sans-serif;font-size:13px;",
        "line-height:1.35;box-shadow:0 2px 6px rgba(0,0,0,0.12);"
      ), opacity = 1),
      opts_toolbar(hidden = c("selection", "zoom", "misc"), saveaspng = FALSE),
      opts_sizing(rescale = TRUE)
    )
  )
}

# -----------------------------------------------------------------------------
# Llegenda: escons, partit i % de vots, en quatre columnes com a l'ARA
# -----------------------------------------------------------------------------
llegenda <- function(estat, actiu = c("nou", "dhondt")) {
  actiu <- match.arg(actiu)
  col <- if (actiu == "nou") "escons_nou" else "escons_dhondt"
  files <- estat |> filter(.data[[col]] > 0) |> arrange(desc(.data[[col]]), desc(vots))
  
  div(class = "llegenda",
      lapply(seq_len(nrow(files)), function(k) {
        f <- files[k, ]
        div(class = "llegenda-fila",
            span(class = "llegenda-escons", f[[col]]),
            span(class = "llegenda-punt", style = sprintf("background:%s", f$color)),
            span(class = "llegenda-nom", f$nom),
            span(class = "llegenda-pct", pct(f$pct))
        )
      })
  )
}

# -----------------------------------------------------------------------------
# Franja de xifres generals
# -----------------------------------------------------------------------------
franja_totals <- function(totals) {
  item <- function(etiqueta, valor) span(class = "franja-item", etiqueta, tags$b(valor))
  div(class = "franja",
      item("Vots vàlids", num(totals$vots_valids)),
      item("En blanc", num(totals$vots_blancs)),
      item("Nuls", num(totals$vots_nuls)),
      item("Llindar proporcional", paste(num(totals$llindar_vots), "vots"))
  )
}

# -----------------------------------------------------------------------------
# Possibles aliances: barres apilades amb la línia de la majoria absoluta
# -----------------------------------------------------------------------------
coalicions_defecte <- list(
  c("PP", "VOX"),
  c("PP", "VOX", "CCa", "U.P.N."),
  c("PP", "VOX", "EAJ-PNV", "CCa", "U.P.N."),
  c("PSOE", "SUMAR"),
  c("PSOE", "SUMAR", "ERC", "EH Bildu", "EAJ-PNV", "B.N.G."),
  c("PSOE", "SUMAR", "ERC", "JxCAT - JUNTS", "EH Bildu", "EAJ-PNV", "B.N.G."),
  c("PP", "PSOE")
)

aliances <- function(estat, actiu = c("nou", "dhondt"), totals,
                     coalicions = coalicions_defecte) {
  actiu <- match.arg(actiu)
  col <- if (actiu == "nou") "escons_nou" else "escons_dhondt"
  pos_majoria <- 100 * totals$majoria / totals$total_escons
  
  files <- lapply(coalicions, function(cc) {
    membres <- estat |> filter(partit %in% cc) |>
      mutate(k = match(partit, cc)) |> arrange(k)
    total <- sum(membres[[col]])
    noms <- paste(membres$nom, collapse = " + ")
    div(class = "alianca",
        div(class = "alianca-cap",
            span(class = "alianca-nom", noms),
            span(class = if (total >= totals$majoria) "alianca-total majoria" else "alianca-total", total)
        ),
        div(class = "alianca-barra",
            lapply(seq_len(nrow(membres)), function(k) {
              span(class = "alianca-tram",
                   title = sprintf("%s: %d", membres$nom[k], membres[[col]][k]),
                   style = sprintf("width:%.3f%%;background:%s",
                                   100 * membres[[col]][k] / totals$total_escons, membres$color[k]))
            }),
            span(class = "alianca-majoria", style = sprintf("left:%.3f%%", pos_majoria))
        )
    )
  })
  
  div(class = "aliances",
      files,
      div(class = "alianca-peu",
          span(style = sprintf("left:%.3f%%", pos_majoria),
               tags$b(totals$majoria), br(), "Majoria absoluta"))
  )
}

# -----------------------------------------------------------------------------
# Del d'Hondt al proporcional: gràfic de pendent (com l'"Evolució" de l'ARA)
# -----------------------------------------------------------------------------
comparativa_gg <- function(estat, min_etiqueta = 10) {
  df <- estat |> filter(escons_nou > 0 | escons_dhondt > 0)
  llarg <- bind_rows(
    df |> transmute(partit, nom, color, x = 0, escons = escons_dhondt),
    df |> transmute(partit, nom, color, x = 1, escons = escons_nou)
  ) |>
    left_join(df |> transmute(partit, d = escons_nou - escons_dhondt,
                              ed = escons_dhondt, en = escons_nou, pct), by = "partit") |>
    mutate(tooltip = sprintf("<b>%s</b><br>d'Hondt: %d<br>Proporcional: %d (%s%d)<br>%s dels vots",
                             nom, ed, en, if_else(d > 0, "+", ""), d, pct(pct)))
  
  # Etiquetes dels partits grans, separades verticalment perquè no es trepitgin
  separa <- function(y, gap) {
    o <- order(y); ys <- y[o]
    for (i in seq_along(ys)[-1]) if (ys[i] - ys[i - 1] < gap) ys[i] <- ys[i - 1] + gap
    y[o] <- ys; y
  }
  gap <- max(llarg$escons) * 0.055
  etiquetes <- df |>
    filter(pmax(escons_nou, escons_dhondt) >= min_etiqueta) |>
    mutate(y_nou = separa(escons_nou, gap), y_dhondt = separa(escons_dhondt, gap))
  
  ggplot(llarg, aes(x, escons, group = partit, colour = color)) +
    annotate("segment", x = c(0, 1), xend = c(0, 1), y = 0, yend = max(llarg$escons) * 1.05,
             colour = "#d5dae0", linewidth = 0.4) +
    geom_line_interactive(aes(tooltip = tooltip, data_id = partit), linewidth = 0.9) +
    geom_point_interactive(aes(tooltip = tooltip, data_id = partit), size = 2.6) +
    geom_text(data = etiquetes, aes(x = 1.04, y = y_nou, label = paste(nom, escons_nou)),
              inherit.aes = FALSE, hjust = 0, size = 3.3, colour = "#111111") +
    geom_text(data = etiquetes, aes(x = -0.04, y = y_dhondt, label = escons_dhondt),
              inherit.aes = FALSE, hjust = 1, size = 3.3, colour = "#111111") +
    annotate("text", x = c(0, 1), y = max(llarg$escons) * 1.12,
             label = c("d'Hondt", "Proporcional"), size = 3.4, colour = "#333333") +
    scale_colour_identity() +
    scale_x_continuous(limits = c(-0.25, 1.45)) +
    theme_void()
}

comparativa <- function(estat) {
  girafe(
    ggobj = comparativa_gg(estat), width_svg = 4.6, height_svg = 4.6,
    options = list(
      opts_hover(css = "stroke-width:2.2px;"),
      opts_hover_inv(css = "opacity:0.2;"),
      opts_tooltip(css = paste(
        "background:#ffffff;color:#111111;border:1px solid #c9ced6;",
        "padding:6px 9px;font-family:Inter,Arial,sans-serif;font-size:13px;line-height:1.35;"
      ), opacity = 1),
      opts_toolbar(hidden = c("selection", "zoom", "misc"), saveaspng = FALSE),
      opts_sizing(rescale = TRUE)
    )
  )
}

# -----------------------------------------------------------------------------
# Taula de vots i escons amb els dos sistemes (ordenable i desplegable)
# -----------------------------------------------------------------------------
taula_vots <- function(estat, n_visibles = 10, min_vots = 10000) {
  df <- estat |> filter(vots >= min_vots) |> arrange(desc(vots))
  max_pct <- max(df$pct)
  
  capcalera <- function(etiqueta, clau, num = TRUE, actiu = FALSE) {
    tags$th(class = paste("ordenable", if (num) "num", if (actiu) "actiu"),
            `data-clau` = clau, tabindex = "0", etiqueta)
  }
  
  files <- lapply(seq_len(nrow(df)), function(k) {
    f <- df[k, ]
    tags$tr(class = if (k > n_visibles) "amagada" else NULL,
            `data-nom` = f$nom, `data-vots` = f$vots, `data-pct` = f$pct,
            `data-dhondt` = f$escons_dhondt, `data-nou` = f$escons_nou,
            tags$td(class = "partit",
                    span(class = "partit-nom", f$nom),
                    span(class = "partit-barra",
                         span(style = sprintf("width:%.2f%%;background:%s", 100 * f$pct / max_pct, f$color)))
            ),
            tags$td(class = "num", num(f$vots)),
            tags$td(class = "num fort", formatC(f$pct, format = "f", digits = 2, decimal.mark = ",")),
            tags$td(class = "num", f$escons_dhondt),
            tags$td(class = "num fort", f$escons_nou)
    )
  })
  
  div(class = "taula-vots plegada", `data-visibles` = n_visibles,
      tags$table(
        tags$thead(tags$tr(
          capcalera("Partit", "nom", num = FALSE),
          capcalera("Vots", "vots"),
          capcalera("%", "pct", actiu = TRUE),
          capcalera("d'Hondt", "dhondt"),
          capcalera("Proporcional", "nou")
        )),
        tags$tbody(files)
      ),
      if (nrow(df) > n_visibles)
        tags$button(class = "desplega", type = "button", "Mostra tots els partits")
  )
}

#' JavaScript petit per ordenar i desplegar les taules (s'inclou un cop)
js_taules <- function() {
  tags$script(HTML("
document.addEventListener('click', function (e) {
  var boto = e.target.closest('.taula-vots .desplega');
  if (boto) {
    var t = boto.closest('.taula-vots');
    var plegada = t.classList.toggle('plegada');
    boto.textContent = plegada ? 'Mostra tots els partits' : 'Mostra només els principals';
    return;
  }
  var th = e.target.closest('.taula-vots th.ordenable');
  if (!th) return;
  var taula = th.closest('table'), cos = taula.tBodies[0];
  var clau = th.dataset.clau, num = th.classList.contains('num');
  var desc = !(th.classList.contains('actiu') && th.dataset.dir === 'desc');
  taula.querySelectorAll('th').forEach(function (x) { x.classList.remove('actiu'); delete x.dataset.dir; });
  th.classList.add('actiu'); th.dataset.dir = desc ? 'desc' : 'asc';
  var files = Array.from(cos.rows);
  files.sort(function (a, b) {
    var va = a.dataset[clau], vb = b.dataset[clau];
    var r = num ? (+va - +vb) : va.localeCompare(vb, 'ca');
    return desc ? -r : r;
  });
  var n = parseInt(taula.closest('.taula-vots').dataset.visibles || '10', 10);
  files.forEach(function (f, i) { f.classList.toggle('amagada', i >= n); cos.appendChild(f); });
});
document.addEventListener('keydown', function (e) {
  if ((e.key === 'Enter' || e.key === ' ') && e.target.matches('.taula-vots th.ordenable')) {
    e.preventDefault(); e.target.click();
  }
});
"))
}

# -----------------------------------------------------------------------------
# Com funciona el sistema proporcional (requadre explicatiu)
# -----------------------------------------------------------------------------
explicacio_sistema <- function(totals) {
  div(class = "explicacio",
      h3("Com es reparteixen els escons"),
      p(tags$b("1. Escons per província."),
        " Cada província rep escons segons la seva població, amb Sainte-Laguë i un mínim d'un escó."),
      p(tags$b("2. Escons per partit."),
        sprintf(" Es compten tots els vots d'Espanya com una sola llista. Entren els partits amb almenys 1/350 dels vots vàlids (%s vots) i es reparteixen els 350 escons amb Sainte-Laguë.",
                num(totals$llindar_vots))),
      p(tags$b("3. Escons de cada partit a cada província."),
        " El mètode biproporcional col·loca els escons de cada partit allà on té els vots relativament més forts, respectant els totals de les províncies. Un partit mai obté escons on no s'ha presentat.")
  )
}

# -----------------------------------------------------------------------------
# Targetes de comunitats (carrusel horitzontal)
# -----------------------------------------------------------------------------
ordre_comunitats <- c("Catalunya", "Illes Balears", "País Valencià")

targetes_comunitats <- function(ccaa_partit, actiu = c("nou", "dhondt")) {
  actiu <- match.arg(actiu)
  col <- if (actiu == "nou") "escons_nou" else "escons_dhondt"
  
  totals <- ccaa_partit |>
    group_by(comunitat) |>
    summarise(total = sum(.data[[col]]), .groups = "drop") |>
    mutate(primer = match(comunitat, ordre_comunitats)) |>
    arrange(primer, desc(total))
  
  targetes <- lapply(totals$comunitat, function(cc) {
    d <- ccaa_partit |>
      filter(comunitat == cc, .data[[col]] > 0) |>
      arrange(desc(.data[[col]]), desc(vots))
    total <- sum(d[[col]])
    div(class = "targeta",
        div(class = "targeta-nom", cc),
        div(class = "targeta-barra",
            lapply(seq_len(nrow(d)), function(k)
              span(title = sprintf("%s: %d", d$sigles[k], d[[col]][k]),
                   style = sprintf("width:%.3f%%;background:%s", 100 * d[[col]][k] / total, d$color[k])))
        ),
        div(class = "targeta-total", span("Total"), tags$b(total)),
        lapply(seq_len(nrow(d)), function(k)
          div(class = "targeta-fila",
              span(class = "llegenda-punt", style = sprintf("background:%s", d$color[k])),
              span(class = "targeta-partit", title = d$sigles[k], d$sigles[k]),
              tags$b(d[[col]][k])))
    )
  })
  
  div(class = "carrusel",
      div(class = "carrusel-pista", tabindex = "0", `aria-label` = "Comunitats", targetes),
      div(class = "carrusel-fletxes",
          tags$button(class = "carrusel-ant", type = "button", `aria-label` = "Comunitats anteriors", HTML("&#8249;")),
          tags$button(class = "carrusel-seg", type = "button", `aria-label` = "Comunitats següents", HTML("&#8250;")))
  )
}

js_carrusel <- function() {
  tags$script(HTML("
document.addEventListener('click', function (e) {
  var b = e.target.closest('.carrusel-ant, .carrusel-seg');
  if (!b) return;
  var pista = b.closest('.carrusel').querySelector('.carrusel-pista');
  var pas = pista.querySelector('.targeta').offsetWidth + 24;
  pista.scrollBy({ left: b.classList.contains('carrusel-seg') ? pas : -pas, behavior: 'smooth' });
});
"))
}
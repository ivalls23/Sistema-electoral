# =============================================================================
# funcions_repartiment.R
# Funcions de repartiment d'escons (base R, sense dependències)
# =============================================================================

#' Repartiment d'escons amb el mètode d'Hondt
#'
#' @param vots         Vector numèric amb els vots de cada candidatura.
#' @param escons       Nombre d'escons a repartir.
#' @param llindar      Llindar mínim sobre els vots vàlids (0.03 = 3%, LOREG art. 163).
#' @param vots_valids  Vots vàlids de la circumscripció (candidatures + blancs).
#'                     Si és NULL, s'usa sum(vots).
#' @return Vector enter, alineat amb `vots`, amb els escons de cada candidatura.
#'
#' Empats de quocient: guanya qui té més vots totals (LOREG art. 163.1.d).
#' Un empat total (mateix quocient i mateixos vots) es resoldria per sorteig;
#' aquí no es contempla perquè a la pràctica no passa.
dhondt <- function(vots, escons, llindar = 0.03, vots_valids = NULL) {
  if (is.null(vots_valids)) vots_valids <- sum(vots)
  resultat <- integer(length(vots))
  if (escons <= 0) return(resultat)

  elegibles <- which(vots > 0 & vots >= llindar * vots_valids)
  if (length(elegibles) == 0) return(resultat)

  quocients <- expand.grid(i = elegibles, divisor = seq_len(escons))
  quocients$q <- vots[quocients$i] / quocients$divisor
  quocients$v <- vots[quocients$i]
  quocients <- quocients[order(-quocients$q, -quocients$v), ]

  guanyadors <- quocients$i[seq_len(escons)]
  as.integer(table(factor(guanyadors, levels = seq_along(vots))))
}

#' Escons per província segons la LOREG (art. 162)
#'
#' 350 escons: Ceuta i Melilla 1 cadascuna, la resta un mínim de 2,
#' i els 248 restants proporcionals a la població (resto més gran).
#'
#' @param poblacio   Vector numèric amb la població de dret de cada província.
#' @param codi_prov  Vector amb el codi INE de província ("01" ... "52").
#' @param total      Total d'escons (350).
#' @param minim      Mínim per província (2).
#' @return Vector enter amb els escons de cada província.
escons_per_provincia <- function(poblacio, codi_prov, total = 350, minim = 2) {
  es_ceuta_melilla <- codi_prov %in% c("51", "52")
  escons <- ifelse(es_ceuta_melilla, 1L, as.integer(minim))

  restants <- total - sum(escons)                  # 248
  pob <- poblacio[!es_ceuta_melilla]
  quota <- sum(pob) / restants                     # quota de repartiment
  q <- pob / quota

  base <- floor(q)
  falten <- restants - sum(base)
  ordre <- order(q - base, decreasing = TRUE)      # fraccions decimals més grans
  base[ordre[seq_len(falten)]] <- base[ordre[seq_len(falten)]] + 1

  escons[!es_ceuta_melilla] <- escons[!es_ceuta_melilla] + base
  as.integer(escons)
}

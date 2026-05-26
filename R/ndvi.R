#' Calcul du NDVI (Normalized Difference Vegetation Index)
#'
#' Calcule l'indice de végétation par différence normalisée à partir
#' des bandes spectrales NIR et Rouge.
#'
#' @param nir Valeur de la bande proche infrarouge (0-1 ou 0-255)
#' @param red Valeur de la bande rouge (0-1 ou 0-255)
#'
#' @return Valeur NDVI entre -1 et 1
#'
#' @details
#' Le NDVI est calculé selon la formule :
#' \deqn{NDVI = \frac{NIR - Rouge}{NIR + Rouge}}
#'
#' Interprétation :
#' - NDVI < 0 : eau, neige, nuages
#' - 0.0 - 0.2 : sol nu, zones urbanisées
#' - 0.2 - 0.4 : végétation éparse
#' - 0.4 - 0.6 : végétation modérée (cultures)
#' - 0.6 - 1.0 : végétation dense et saine
#'
#' @examples
#' # NDVI d'un pixel de culture saine
#' calc_ndvi(nir = 0.8, red = 0.1)
#'
#' # Sur un vecteur de pixels
#' nir_vals <- c(0.8, 0.3, 0.05, 0.6)
#' red_vals <- c(0.1, 0.2, 0.04, 0.15)
#' calc_ndvi(nir_vals, red_vals)
#'
#' @export
calc_ndvi <- function(nir, red) {
  if (any((nir + red) == 0)) {
    warning("Certains pixels ont NIR + Rouge = 0, résultat NA pour ces pixels")
  }
  ndvi <- ifelse((nir + red) == 0, NA, (nir - red) / (nir + red))
  return(round(ndvi, 4))
}


#' Classifier le NDVI en catégories de végétation
#'
#' @param ndvi Vecteur de valeurs NDVI
#'
#' @return Facteur avec les classes de végétation
#'
#' @examples
#' valeurs <- c(-0.1, 0.1, 0.3, 0.5, 0.75)
#' classifier_ndvi(valeurs)
#'
#' @export
classifier_ndvi <- function(ndvi) {
  classes <- cut(ndvi,
                 breaks = c(-Inf, 0, 0.2, 0.4, 0.6, Inf),
                 labels = c("Eau/Nuage", "Sol nu", "Végétation éparse",
                            "Végétation modérée", "Végétation dense"),
                 include.lowest = TRUE
  )
  return(classes)
}

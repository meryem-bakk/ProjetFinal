#' Cartographie pastorale des variables environnementales
#'
#' Génère des cartes raster des variables pastorales
#' et environnementales utilisées dans le package.
#'
#' @param raster Raster à afficher
#' @param title Titre de la carte
#'
#' @return Carte raster affichée
#'
#' @details
#' Cette fonction permet de visualiser spatialement :
#' - le NDVI,
#' - la biomasse,
#' - la capacité de charge,
#' - la pression pastorale.
#'
#' Les cartes pastorales permettent d’identifier :
#' - les zones favorables au pâturage,
#' - les zones dégradées,
#' - les secteurs à forte pression animale.
#'
#' @references
#' Hijmans, R.J. et al. (2023).
#' terra: Spatial Data Analysis.
#' R package version.
#'
#' @examples
#' # Exemple avec un raster
#' # plot_rangeland_maps(raster, "NDVI")
#'
#' @export

#' Import climate data from WorldClim
#'
#' Import des donnees climatiques pour
#' la modelisation des especes fourrageres.
#'
#' @param path Dossier de stockage des donnees
#' climatiques.
#'
#' @param resolution Resolution spatiale
#' WorldClim en minutes.
#'
#' @return Raster climatique de type SpatRaster.
#'
#' @details
#' Cette fonction telecharge les variables
#' bioclimatiques depuis WorldClim.
#'
#' Variables incluses :
#' - temperature,
#' - precipitations.
#'
#' Les donnees sont utilisees dans :
#' - les SDM,
#' - la modelisation spatiale,
#' - l'analyse environnementale.
#'
#' Fonctionnalites :
#' - telechargement automatique,
#' - raster stacking,
#' - harmonisation spatiale.
#'
#' Source :
#' WorldClim database.
#'
#' @references
#' Fick, S.E. & Hijmans, R.J. (2017).
#' WorldClim 2: new climate surfaces for global land areas.
#' International Journal of Climatology.
#'
#' WorldClim:
#' https://www.worldclim.org/
#'
#' @examples
#' \dontrun{
#' climate <- load_climate_data()
#' plot(climate[[1]])
#' }
#'
#' @export

load_climate_data <- function(

  path = "climate_data",

  resolution = 10

){

  climate <- geodata::worldclim_global(

    var = "bio",

    res = resolution,

    path = path

  )

  return(climate)

}

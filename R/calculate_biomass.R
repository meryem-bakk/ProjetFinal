#' Estimate vegetation biomass
#'
#' Estime la biomasse vegetale disponible
#' a partir du NDVI et du LAI.
#'
#' @param ndvi Raster ou valeurs NDVI (numerique ou SpatRaster).
#' @param lai Raster ou valeurs LAI optionnel.
#' @param method Methode d'estimation. Actuellement : "linear".
#'
#' @return Biomasse estimee (kg/ha).
#'
#' @details
#' Relation simplifiee :
#'
#' Biomass = a + b * NDVI
#'
#' Par defaut :
#'
#' Biomass = 1000 * NDVI
#'
#' Si LAI disponible :
#'
#' Biomass = 800 * NDVI + 200 * LAI
#'
#' Compatible avec les rasters terra.
#'
#' @references
#' Tucker, C.J. (1979).
#' Red and photographic infrared linear combinations
#' for monitoring vegetation.
#' Remote Sensing of Environment.
#'
#' @examples
#' biomass <- calculate_biomass(ndvi = 0.6)
#'
#' @export

calculate_biomass <- function(

  ndvi,
  lai    = NULL,
  method = "linear"

){

  # ==========================
  # Verification
  # ==========================

  if(missing(ndvi)){
    stop("NDVI est obligatoire.")
  }

  method <- match.arg(method, choices = "linear")

  # ==========================
  # NDVI seul
  # ==========================

  if(is.null(lai)){

    biomass <- 1000 * ndvi

  } else {

    # ==========================
    # NDVI + LAI
    # ==========================

    biomass <- (800 * ndvi) + (200 * lai)

  }

  return(biomass)

}

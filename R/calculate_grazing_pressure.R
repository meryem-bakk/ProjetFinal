#' Calcul de la pression pastorale
#'
#' Analyse la pression de paturage en comparant
#' la densite animale avec la capacite de charge disponible.
#'
#' @param biomass Biomasse disponible (kg/ha).
#' Numerique ou SpatRaster.
#'
#' @param carrying_capacity Capacite de charge estimee
#' (UBT/ha). Numerique ou SpatRaster.
#'
#' @param animal_density Densite animale reelle
#' (UBT/ha). Optionnelle.
#'
#' @return Indice de pression pastorale.
#'
#' @details
#' La pression pastorale est calculee par :
#'
#' \deqn{
#' GrazingPressure =
#' \frac{AnimalDensity}{CarryingCapacity}
#' }
#'
#' Interpretation :
#'
#' * inferieur a 1 : faible pression (sous-paturage)
#' * egal a 1 : equilibre
#' * superieur a 1 : forte pression (surpaturage)
#'
#' Si aucune densite animale n'est fournie,
#' la fonction utilise la biomasse disponible
#' pour produire un indice relatif normalise
#' entre 0 et 1.
#'
#' Compatible avec les rasters terra.
#'
#' @references
#' Holechek, J.L., Pieper, R.D.,
#' & Herbel, C.H. (2010).
#' Range Management:
#' Principles and Practices.
#'
#' @examples
#'
#' pressure <- calculate_grazing_pressure(
#'   biomass = 500,
#'   carrying_capacity = 2,
#'   animal_density = 1
#' )
#'
#' @export

calculate_grazing_pressure <- function(

  biomass,
  carrying_capacity,
  animal_density = NULL

){

  # ==========================
  # Verification carrying_capacity > 0
  # Compatible raster et scalaire
  # ==========================

  if(inherits(carrying_capacity, "SpatRaster")){

    min_cap <- terra::global(carrying_capacity, "min", na.rm = TRUE)$min

  } else {

    min_cap <- min(carrying_capacity, na.rm = TRUE)

  }

  if(!is.na(min_cap) && min_cap <= 0){
    stop("La capacite de charge doit etre strictement positive.")
  }

  # ==========================
  # Calcul pression pastorale
  # ==========================

  if(!is.null(animal_density)){

    pressure_index <- animal_density / carrying_capacity

  } else {

    # Indice relatif base sur la biomasse (normalise 0-1)
    # Compatible raster et scalaire

    if(inherits(biomass, "SpatRaster")){

      max_bio <- terra::global(biomass, "max", na.rm = TRUE)$max

    } else {

      max_bio <- max(biomass, na.rm = TRUE)

    }

    pressure_index <- biomass / max_bio

  }

  return(pressure_index)

}

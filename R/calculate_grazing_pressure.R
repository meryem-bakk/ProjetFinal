#' Calcul de la pression pastorale
#'
#' Analyse la pression de pâturage en comparant
#' la densité animale avec la capacité de charge disponible.
#'
#' @param biomass Biomasse disponible (kg/ha).
#'
#' @param carrying_capacity Capacité de charge estimée
#' (UBT/ha).
#'
#' @param animal_density Densité animale réelle
#' (UBT/ha). Optionnelle.
#'
#' @return Indice de pression pastorale.
#'
#' @details
#' La pression pastorale est calculée par :
#'
#' \deqn{
#' GrazingPressure =
#' \frac{AnimalDensity}{CarryingCapacity}
#' }
#'
#' Interprétation :
#'
#' * inférieur à 1 : faible pression (sous-pâturage)
#' * égal à 1 : équilibre
#' * supérieur à 1 : forte pression (surpâturage)
#'
#' Si aucune densité animale n'est fournie,
#' la fonction utilise la biomasse disponible
#' pour produire un indice relatif.
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
  # Verification
  # ==========================

  if(any(carrying_capacity <= 0, na.rm = TRUE)){

    stop("Carrying capacity must be positive")

  }

  # ==========================
  # Calcul pression pastorale
  # ==========================

  if(!is.null(animal_density)){

    pressure_index <-

      animal_density / carrying_capacity

  } else {

    # Indice relatif base sur la biomasse

    pressure_index <-

      biomass / max(

        biomass,

        na.rm = TRUE

      )

  }

  # ==========================
  # Output
  # ==========================

  return(

    pressure_index

  )

}

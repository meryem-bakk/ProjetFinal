#' Identification des zones refuges pastorales
#'
#' Identifie les zones prioritaires pour la conservation
#' en combinant biomasse, pression pastorale et climat.
#'
#' @param biomass Raster ou valeurs de biomasse disponible.
#'
#' @param grazing_pressure Raster ou valeurs d'indice
#' de pression pastorale.
#'
#' @param climate_score Conditions climatiques favorables.
#' Valeurs comprises entre 0 et 1.
#'
#' @return Liste contenant :
#' - refuge_index
#' - priority_area
#'
#' @details
#' Une zone refuge correspond à :
#'
#' - biomasse élevée,
#' - faible pression pastorale,
#' - conditions climatiques favorables.
#'
#' L'indice est calculé par :
#'
#' \deqn{
#' RefugeIndex =
#' BiomassScore *
#' ClimateScore /
#' GrazingPressure
#' }
#'
#' Classification :
#'
#' - FALSE : zone non prioritaire
#' - TRUE : zone refuge prioritaire
#'
#' Compatible avec les rasters terra.
#'
#' @references
#' Franklin, J. (2010).
#' Mapping Species Distributions:
#' Spatial Inference and Prediction.
#'
#' @examples
#'
#' refuge <- identify_refuge_areas(
#'   biomass = 800,
#'   grazing_pressure = 0.5,
#'   climate_score = 0.8
#' )
#'
#' @export

identify_refuge_areas <- function(

  biomass,

  grazing_pressure,

  climate_score = 1

){

  # ==========================
  # Verification
  # ==========================

  if(any(grazing_pressure <= 0, na.rm = TRUE)){

    stop(
      "Grazing pressure must be positive"
    )

  }

  # ==========================
  # Normalisation biomasse
  # ==========================

  biomass_score <-

    biomass /

    max(
      biomass,
      na.rm = TRUE
    )

  # ==========================
  # Calcul indice refuge
  # ==========================

  refuge_index <-

    (biomass_score *

       climate_score) /

    grazing_pressure

  # ==========================
  # Classification
  # ==========================

  threshold <-

    mean(
      refuge_index,
      na.rm = TRUE
    )

  priority_area <-

    refuge_index >= threshold

  # ==========================
  # Output
  # ==========================

  return(

    list(

      refuge_index = refuge_index,

      priority_area = priority_area

    )

  )

}

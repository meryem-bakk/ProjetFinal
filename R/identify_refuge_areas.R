#' Identification des zones refuges pastorales
#'
#' Identifie les zones prioritaires pour la conservation
#' en combinant biomasse, pression pastorale et climat.
#'
#' @param biomass Raster ou valeurs de biomasse disponible
#' (numerique ou SpatRaster).
#'
#' @param grazing_pressure Raster ou valeurs d'indice
#' de pression pastorale (numerique ou SpatRaster).
#' Doit etre strictement positif.
#'
#' @param climate_score Conditions climatiques favorables.
#' Valeurs comprises entre 0 et 1.
#'
#' @return Liste contenant :
#' - refuge_index : indice de valeur refuge (normalise)
#' - priority_area : zones au-dessus de la moyenne (TRUE/FALSE)
#'
#' @details
#' Une zone refuge correspond a :
#'
#' - biomasse elevee,
#' - faible pression pastorale,
#' - conditions climatiques favorables.
#'
#' L'indice est calcule par :
#'
#' \deqn{
#' RefugeIndex =
#' BiomassScore *
#' ClimateScore /
#' GrazingPressure
#' }
#'
#' ou BiomassScore = biomass / max(biomass).
#'
#' Les pixels avec pression <= 0 generent un avertissement
#' et sont remplaces par NA.
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
  # Verification grazing_pressure > 0
  # Compatible raster et scalaire
  # ==========================

  if(inherits(grazing_pressure, "SpatRaster")){

    min_gp <- terra::global(grazing_pressure, "min", na.rm = TRUE)$min

  } else {

    min_gp <- min(grazing_pressure, na.rm = TRUE)

  }

  if(!is.na(min_gp) && min_gp <= 0){

    warning(
      "grazing_pressure contient des valeurs <= 0. ",
      "Ces pixels seront remplaces par NA dans le calcul de l'indice refuge."
    )

    if(inherits(grazing_pressure, "SpatRaster")){
      grazing_pressure[grazing_pressure <= 0] <- NA
    } else {
      grazing_pressure[grazing_pressure <= 0] <- NA
    }

  }

  # ==========================
  # Normalisation biomasse (0-1)
  # Compatible raster et scalaire
  # ==========================

  if(inherits(biomass, "SpatRaster")){

    max_bio <- terra::global(biomass, "max", na.rm = TRUE)$max

  } else {

    max_bio <- max(biomass, na.rm = TRUE)

  }

  biomass_score <- biomass / max_bio

  # ==========================
  # Calcul indice refuge
  # ==========================

  refuge_index <- (biomass_score * climate_score) / grazing_pressure

  # ==========================
  # Classification (au-dessus de la moyenne)
  # ==========================

  if(inherits(refuge_index, "SpatRaster")){

    threshold <- terra::global(refuge_index, "mean", na.rm = TRUE)$mean

  } else {

    threshold <- mean(refuge_index, na.rm = TRUE)

  }

  priority_area <- refuge_index >= threshold

  return(

    list(
      refuge_index  = refuge_index,
      priority_area = priority_area
    )

  )

}

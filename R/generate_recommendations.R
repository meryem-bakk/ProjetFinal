#' Generation de recommandations de gestion durable
#'
#' Produit des recommandations de gestion pastorale
#' a partir des indicateurs calcules.
#'
#' @param carrying_capacity Capacite de charge (UBT/ha).
#'
#' @param grazing_pressure Indice de pression pastorale.
#'
#' @param refuge_area Zones refuges prioritaires
#' (optionnel).
#'
#' @return Vecteur contenant les recommandations.
#'
#' @details
#' Les recommandations peuvent inclure :
#'
#' - rotation du paturage,
#' - reduction de la pression animale,
#' - restauration de la vegetation,
#' - protection des zones refuges.
#'
#' @examples
#' recommendations <- generate_recommendations(
#'   carrying_capacity = 0.8,
#'   grazing_pressure = 1.6
#' )
#'
#' recommendations
#'
#' @export

generate_recommendations <- function(

  carrying_capacity,

  grazing_pressure,

  refuge_area = NULL

){

  recommendations <- character()

  # ==========================
  # Gestion des rasters
  # ==========================

  if(inherits(grazing_pressure, "SpatRaster")){

    mean_pressure <- mean(

      terra::values(grazing_pressure),

      na.rm = TRUE

    )

  } else {

    mean_pressure <- mean(

      grazing_pressure,

      na.rm = TRUE

    )

  }

  if(inherits(carrying_capacity, "SpatRaster")){

    mean_capacity <- mean(

      terra::values(carrying_capacity),

      na.rm = TRUE

    )

  } else {

    mean_capacity <- mean(

      carrying_capacity,

      na.rm = TRUE

    )

  }

  # ==========================
  # Pression pastorale
  # ==========================

  if(mean_pressure > 1){

    recommendations <- c(

      recommendations,

      "Reduire temporairement la pression animale sur les parcours.",

      "Mettre en place une rotation des zones de paturage."

    )

  }

  # ==========================
  # Capacite de charge
  # ==========================

  if(mean_capacity < 1){

    recommendations <- c(

      recommendations,

      "Restaurer la couverture vegetale dans les zones degradees.",

      "Ameliorer les ressources fourrageres par des actions de rehabilitation."

    )

  }

  # ==========================
  # Zones refuges
  # ==========================

  if(!is.null(refuge_area)){

    recommendations <- c(

      recommendations,

      "Proteger les zones refuges a forte valeur ecologique.",

      "Limiter le paturage dans les zones prioritaires de conservation."

    )

  }

  # ==========================
  # Situation favorable
  # ==========================

  if(length(recommendations) == 0){

    recommendations <- c(

      "La gestion pastorale actuelle est compatible avec une utilisation durable des parcours."

    )

  }

  # ==========================
  # Output
  # ==========================

  return(

    unique(recommendations)

  )

}

#' Clean species occurrences
#'
#' Nettoie les donnees spatiales d'occurrences.
#'
#' @param occurrences Objet sf contenant les occurrences
#'
#' @return Objet sf nettoye
#'
#' @details
#' La fonction realise :
#' - suppression des coordonnees invalides
#' - suppression des doublons spatiaux
#' - filtrage spatial simple
#'
#' @examples
#' \dontrun{
#' clean <- clean_occurrences(species$sf_object)
#' }
#'
#' @export

clean_occurrences <- function(occurrences){

  # ==========================
  # Verification
  # ==========================

  if(!inherits(occurrences, "sf")){

    stop("Input must be an sf object")

  }

  # ==========================
  # Suppression geometries vides
  # ==========================

  occurrences <- occurrences[

    !sf::st_is_empty(occurrences),

  ]

  # ==========================
  # Suppression doublons spatiaux
  # ==========================

  coords <- sf::st_coordinates(

    occurrences

  )

  occurrences$lon <- coords[,1]

  occurrences$lat <- coords[,2]

  occurrences <- occurrences[

    !duplicated(

      occurrences[, c("lon", "lat")]

    ),

  ]

  # ==========================
  # Suppression coordonnees invalides
  # ==========================

  occurrences <- occurrences[

    occurrences$lon >= -180 &
      occurrences$lon <= 180 &
      occurrences$lat >= -90 &
      occurrences$lat <= 90,

  ]

  # ==========================
  # Nettoyage colonnes temporaires
  # ==========================

  occurrences$lon <- NULL

  occurrences$lat <- NULL

  # ==========================
  # Output
  # ==========================

  return(

    occurrences

  )

}

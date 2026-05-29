#' Import des espèces fourragères
#'
#' Importe les données d'espèces fourragères depuis
#' GBIF ou un fichier CSV terrain.
#'
#' @param species_name Nom scientifique de l'espèce (optionnel)
#' @param csv_file Chemin du fichier CSV terrain (optionnel)
#' @param limit Nombre maximum d'occurrences GBIF
#'
#' @return
#' Une liste contenant :
#' - sf_object : objet spatial sf
#' - dataframe : dataframe des occurrences
#'
#' @details
#' Cette fonction permet :
#' - l'import des occurrences depuis GBIF,
#' - l'import des données terrain CSV,
#' - la suppression des doublons,
#' - le nettoyage des coordonnées invalides,
#' - la conversion en objet spatial sf.
#'
#' Les variables utilisées sont :
#' - espèce
#' - coordonnées GPS
#' - abondance optionnelle
#'
#' @references
#' GBIF.org (2025).
#' Global Biodiversity Information Facility.
#' https://www.gbif.org/
#'
#' Pebesma, E. (2018).
#' Simple Features for R: Standardized Support
#' for Spatial Vector Data.
#'
#' @examples
#' # Import depuis GBIF
#' # species <- import_forage_species(
#' #   species_name = "Stipa tenacissima"
#' # )
#'
#' # Import depuis CSV
#' # species <- import_forage_species(
#' #   csv_file = "species.csv"
#' # )
#'
#' @export

import_forage_species <- function(
    species_name = NULL,
    csv_file = NULL,
    limit = 100
){

  # =========================
  # Import depuis GBIF
  # =========================

  if(!is.null(species_name)){

    occ <- rgbif::occ_search(

      scientificName = species_name,

      hasCoordinate = TRUE,

      limit = limit

    )

    data <- occ$data[, c(

      "species",

      "decimalLongitude",

      "decimalLatitude"

    )]

    names(data) <- c(

      "species",

      "longitude",

      "latitude"

    )

  }

  # =========================
  # Import CSV terrain
  # =========================

  else if(!is.null(csv_file)){

    data <- read.csv(csv_file)

  }

  else{

    stop(
      "Veuillez fournir species_name ou csv_file"
    )

  }

  # =========================
  # Nettoyage coordonnées
  # =========================

  data <- data[

    !is.na(data$longitude) &
      !is.na(data$latitude),

  ]

  # =========================
  # Suppression doublons
  # =========================

  data <- unique(data)

  # =========================
  # Conversion objet spatial
  # =========================

  sf_object <- sf::st_as_sf(

    data,

    coords = c(
      "longitude",
      "latitude"
    ),

    crs = 4326

  )

  # =========================
  # Output final
  # =========================

  return(list(

    sf_object = sf_object,

    dataframe = data

  ))

}

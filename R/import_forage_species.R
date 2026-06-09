#' Import forage species data
#'
#' Import forage species occurrences from
#' GBIF or field CSV files.
#'
#' @param species_name Scientific species name.
#' @param csv_file CSV file path.
#' @param limit Number of GBIF occurrences.
#'
#' @return
#' A list containing:
#' - sf_object : spatial sf object
#' - dataframe : species dataframe
#'
#' @details
#' This function allows:
#' - GBIF import,
#' - field CSV import,
#' - duplicate removal,
#' - coordinate cleaning,
#' - conversion to spatial sf object.
#'
#' Variables:
#' - species
#' - GPS coordinates
#' - optional abundance
#'
#' @references
#' GBIF.org (2025).
#' Global Biodiversity Information Facility.
#'
#' Pebesma, E. (2018).
#' Simple Features for R.
#'
#' @examples
#' \dontrun{
#' species <- import_forage_species(
#'   species_name = "Stipa tenacissima"
#' )
#' }
#'
#' @export

import_forage_species <- function(

  species_name = NULL,
  csv_file     = NULL,
  limit        = 100

){

  # =========================
  # Import from GBIF
  # =========================

  if(!is.null(species_name)){

    occ <- rgbif::occ_search(
      scientificName = species_name,
      hasCoordinate  = TRUE,
      limit          = limit
    )

    if(is.null(occ$data) || nrow(occ$data) == 0){
      stop("Aucune occurrence GBIF trouvee pour : ", species_name)
    }

    data <- occ$data[, c(
      "species",
      "decimalLongitude",
      "decimalLatitude"
    )]

    names(data) <- c("species", "longitude", "latitude")

  }

  # =========================
  # Import from CSV
  # =========================

  else if(!is.null(csv_file)){

    if(!file.exists(csv_file)){
      stop("Fichier CSV introuvable : ", csv_file)
    }

    data <- utils::read.csv(csv_file)

    required_cols <- c("longitude", "latitude")

    missing_cols <- setdiff(required_cols, names(data))

    if(length(missing_cols) > 0){
      stop(
        "Colonnes manquantes dans le fichier CSV : ",
        paste(missing_cols, collapse = ", ")
      )
    }

  }

  else{
    stop("Fournir species_name (GBIF) ou csv_file (terrain).")
  }

  # =========================
  # Suppression coordonnees manquantes
  # =========================

  data <- data[
    !is.na(data$longitude) & !is.na(data$latitude),
  ]

  # =========================
  # Suppression coordonnees invalides
  # =========================

  data <- data[
    data$longitude >= -180 & data$longitude <= 180 &
    data$latitude  >= -90  & data$latitude  <= 90,
  ]

  # =========================
  # Suppression doublons
  # =========================

  data <- unique(data)

  if(nrow(data) == 0){
    stop("Aucune occurrence valide apres nettoyage des coordonnees.")
  }

  # =========================
  # Conversion en objet sf
  # =========================

  sf_object <- sf::st_as_sf(
    data,
    coords = c("longitude", "latitude"),
    crs    = 4326
  )

  return(

    list(
      sf_object = sf_object,
      dataframe = data
    )

  )

}

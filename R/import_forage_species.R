#' Import forage species data
#'
#' Import forage species occurrences from
#' GBIF or field CSV files.
#'
#' @param species_name Scientific species name
#' @param csv_file CSV file path
#' @param limit Number of GBIF occurrences
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

  csv_file = NULL,

  limit = 100

){

  # =========================
  # Import from GBIF
  # =========================

  if(!is.null(species_name)){

    occ <- rgbif::occ_search(

      scientificName = species_name,

      hasCoordinate = TRUE,

      limit = limit

    )

    if(is.null(occ$data) || nrow(occ$data) == 0){

      stop("No GBIF occurrences found")

      return(NULL)

    }

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
  # Import from CSV
  # =========================

  else if(!is.null(csv_file)){

    data <- utils::read.csv(csv_file)

  }

  else{

    stop(
      "Please provide species_name or csv_file"
    )

  }

  # =========================
  # Remove missing coordinates
  # =========================

  data <- data[

    !is.na(data$longitude) &
      !is.na(data$latitude),

  ]

  # =========================
  # Remove invalid coordinates
  # =========================

  data <- data[

    data$longitude >= -180 &
      data$longitude <= 180 &

      data$latitude >= -90 &
      data$latitude <= 90,

  ]

  # =========================
  # Remove duplicates
  # =========================

  data <- unique(data)

  # =========================
  # Convert to sf object
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
  # Return outputs
  # =========================

  return(list(

    sf_object = sf_object,

    dataframe = data

  ))

}

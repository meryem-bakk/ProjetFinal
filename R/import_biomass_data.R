#' Import biomass field data
#'
#' Importe les donnees terrain de biomasse.
#'
#' @param file Chemin du fichier CSV ou Excel
#'
#' @return Dataframe biomasse nettoye
#'
#' @details
#' Fonctionnalites :
#' - import CSV / Excel
#' - gestion des valeurs manquantes
#' - harmonisation des unites
#'
#' Variables attendues :
#' - plot
#' - date
#' - biomass
#'
#' @examples
#' \dontrun{
#' biomass <- import_biomass_data(
#'   "biomass.csv"
#' )
#' }
#'
#' @export

import_biomass_data <- function(file){

  # ==========================
  # Verification
  # ==========================

  if(!file.exists(file)){

    stop("File not found")

  }

  # ==========================
  # Extension
  # ==========================

  extension <- tools::file_ext(file)

  # ==========================
  # CSV
  # ==========================

  if(extension == "csv"){

    data <- utils::read.csv(

      file

    )

  }

  # ==========================
  # Excel
  # ==========================

  else if(extension %in% c("xls", "xlsx")){

    data <- readxl::read_excel(

      file

    )

    data <- as.data.frame(

      data

    )

  }

  else{

    stop(

      "Unsupported file format"

    )

  }

  # ==========================
  # Verification colonnes
  # ==========================

  if(!"biomass" %in% names(data)){

    stop(

      "Column 'biomass' not found"

    )

  }

  # ==========================
  # Suppression NA
  # ==========================

  data <- data[

    !is.na(data$biomass),

  ]

  # ==========================
  # Harmonisation unite
  # ==========================

  if(max(

    data$biomass,

    na.rm = TRUE

  ) < 100){

    data$biomass <-

      data$biomass * 10000

  }

  # ==========================
  # Output
  # ==========================

  return(

    data

  )

}

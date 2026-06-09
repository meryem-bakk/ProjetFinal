#' Import biomass field data
#'
#' Importe les donnees terrain de biomasse.
#'
#' @param file Chemin du fichier CSV ou Excel.
#'
#' @return Dataframe biomasse nettoye.
#'
#' @details
#' Fonctionnalites :
#' - import CSV / Excel
#' - gestion des valeurs manquantes
#' - harmonisation des unites avec avertissement
#'
#' Variables attendues :
#' - plot
#' - date
#' - biomass
#'
#' Si les valeurs de biomasse sont toutes inferieures
#' a 100, la fonction suppose qu'elles sont exprimees
#' en t/ha et les convertit en kg/ha (* 10 000).
#' Un avertissement est emis dans ce cas.
#'
#' @examples
#' \dontrun{
#' biomass <- import_biomass_data("biomass.csv")
#' }
#'
#' @export

import_biomass_data <- function(file){

  # ==========================
  # Verification
  # ==========================

  if(!file.exists(file)){
    stop("Fichier introuvable : ", file)
  }

  extension <- tools::file_ext(file)

  # ==========================
  # CSV
  # ==========================

  if(extension == "csv"){

    data <- utils::read.csv(file)

  }

  # ==========================
  # Excel
  # ==========================

  else if(extension %in% c("xls", "xlsx")){

    data <- as.data.frame(readxl::read_excel(file))

  }

  else{
    stop(
      "Format non supporte : '", extension, "'. ",
      "Formats acceptes : csv, xls, xlsx."
    )
  }

  # ==========================
  # Verification colonne biomass
  # ==========================

  if(!"biomass" %in% names(data)){
    stop(
      "Colonne 'biomass' introuvable dans le fichier. ",
      "Colonnes disponibles : ", paste(names(data), collapse = ", ")
    )
  }

  # ==========================
  # Suppression NA
  # ==========================

  data <- data[!is.na(data$biomass), ]

  # ==========================
  # Harmonisation unite (avec avertissement)
  # ==========================

  if(max(data$biomass, na.rm = TRUE) < 100){

    warning(
      "Les valeurs de biomasse sont toutes inferieures a 100. ",
      "Conversion automatique supposee de t/ha vers kg/ha (x 10 000). ",
      "Verifier que les unites du fichier sont correctes."
    )

    data$biomass <- data$biomass * 10000

  }

  return(data)

}

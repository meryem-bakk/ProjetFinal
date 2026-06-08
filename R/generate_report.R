#' Generate HTML/PDF rangeland report
#'
#' Genere automatiquement un rapport HTML ou PDF
#' synthetisant les resultats du projet.
#'
#' @param species Donnees especes fourrageres.
#' @param biomass Biomasse estimee.
#' @param carrying_capacity Capacite de charge.
#' @param grazing_pressure Pression pastorale.
#' @param recommendations Recommandations de gestion.
#' @param output_format "html" ou "pdf".
#' @param output_dir Dossier de sortie.
#'
#' @return Chemin du rapport genere.
#'
#' @export

generate_report <- function(

  species,

  biomass,

  carrying_capacity,

  grazing_pressure,

  recommendations,

  output_format = "html",

  output_dir = "outputs"

){

  if(!dir.exists(output_dir)){

    dir.create(
      output_dir,
      recursive = TRUE
    )

  }

  biomass_mean <- round(

    mean(
      terra::values(biomass),
      na.rm = TRUE
    ),

    2

  )

  capacity_mean <- round(

    mean(
      terra::values(carrying_capacity),
      na.rm = TRUE
    ),

    2

  )

  pressure_mean <- round(

    mean(
      terra::values(grazing_pressure),
      na.rm = TRUE
    ),

    2

  )

  n_species <- nrow(

    species$species_data

  )

  rec_text <- paste(

    recommendations,

    collapse = "<br>"

  )

  rmd_file <- file.path(

    output_dir,

    "report.Rmd"

  )

  lines <- c(

    "---",

    paste0(
      "title: \"Rapport de gestion pastorale\""
    ),

    if(output_format == "html")
      "output: html_document"
    else
      "output: pdf_document",

    "---",

    "",

    "# Resume",

    "",

    paste(
      "Nombre d'especes fourrageres :",
      n_species
    ),

    "",

    paste(
      "Biomasse moyenne :",
      biomass_mean,
      "kg/ha"
    ),

    "",

    paste(
      "Capacite de charge moyenne :",
      capacity_mean,
      "UBT/ha"
    ),

    "",

    paste(
      "Pression pastorale moyenne :",
      pressure_mean
    ),

    "",

    "# Recommandations",

    "",

    rec_text,

    "",

    "# Cartes produites",

    "",

    "- Biomasse",

    "- Capacite de charge",

    "- Pression pastorale",

    "",

    "# Fin du rapport"

  )

  writeLines(

    lines,

    rmd_file

  )

  rmarkdown::render(

    input = rmd_file,

    output_dir = output_dir,

    quiet = TRUE

  )

  if(output_format == "html"){

    report_file <- file.path(
      output_dir,
      "report.html"
    )

  } else {

    report_file <- file.path(
      output_dir,
      "report.pdf"
    )

  }

  return(report_file)

}

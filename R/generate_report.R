#' Generate HTML/PDF rangeland report
#'
#' Genere automatiquement un rapport HTML ou PDF
#' synthetisant les resultats du projet.
#'
#' @param species Liste retournee par import_forage_species()
#' contenant au moins un element \code{dataframe}.
#' @param biomass Biomasse estimee (SpatRaster ou numerique).
#' @param carrying_capacity Capacite de charge (SpatRaster ou numerique).
#' @param grazing_pressure Pression pastorale (SpatRaster ou numerique).
#' @param recommendations Vecteur de recommandations de gestion.
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
  output_dir    = "outputs"

){

  # ==========================
  # Verifications
  # ==========================

  if(!output_format %in% c("html", "pdf")){
    stop("output_format doit etre 'html' ou 'pdf'.")
  }

  if(is.null(species$dataframe)){
    stop(
      "species doit contenir un element 'dataframe'. ",
      "Verifier que species provient de import_forage_species()."
    )
  }

  # Fonction utilitaire : moyenne compatible raster/scalaire
  .safe_mean <- function(x, label){
    if(inherits(x, "SpatRaster")){
      round(terra::global(x, "mean", na.rm = TRUE)$mean, 2)
    } else if(is.numeric(x)){
      round(mean(x, na.rm = TRUE), 2)
    } else {
      stop(label, " doit etre un SpatRaster ou un vecteur numerique.")
    }
  }

  # ==========================
  # Calcul des indicateurs
  # ==========================

  biomass_mean   <- .safe_mean(biomass,           "biomass")
  capacity_mean  <- .safe_mean(carrying_capacity, "carrying_capacity")
  pressure_mean  <- .safe_mean(grazing_pressure,  "grazing_pressure")

  n_species <- nrow(species$dataframe)

  rec_text <- paste(recommendations, collapse = "<br>")

  # ==========================
  # Creation dossier
  # ==========================

  if(!dir.exists(output_dir)){
    dir.create(output_dir, recursive = TRUE)
  }

  # ==========================
  # Ecriture du fichier Rmd
  # ==========================

  rmd_file <- file.path(output_dir, "report.Rmd")

  # Supprimer le fichier Rmd temporaire a la sortie
  on.exit(unlink(rmd_file), add = TRUE)

  lines <- c(
    "---",
    "title: \"Rapport de gestion pastorale\"",
    if(output_format == "html") "output: html_document" else "output: pdf_document",
    "---",
    "",
    "# Resume",
    "",
    paste("Nombre d'especes fourrageres :", n_species),
    "",
    paste("Biomasse moyenne :", biomass_mean, "kg/ha"),
    "",
    paste("Capacite de charge moyenne :", capacity_mean, "UBT/ha"),
    "",
    paste("Pression pastorale moyenne :", pressure_mean),
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

  writeLines(lines, rmd_file)

  # ==========================
  # Rendu
  # ==========================

  rmarkdown::render(
    input      = rmd_file,
    output_dir = output_dir,
    quiet      = TRUE
  )

  report_file <- file.path(
    output_dir,
    paste0("report.", if(output_format == "html") "html" else "pdf")
  )

  return(report_file)

}

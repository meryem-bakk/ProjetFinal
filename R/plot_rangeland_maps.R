#' Cartographie pastorale
#'
#' Produit les cartes finales du package
#' rangelandSDM.
#'
#' @param biomass Raster de biomasse.
#'
#' @param ndvi Raster NDVI.
#'
#' @param carrying_capacity Raster de capacité de charge.
#'
#' @param grazing_pressure Raster de pression pastorale.
#'
#' @param output_dir Dossier d'export.
#'
#' @param format Format d'export ("png" ou "pdf").
#'
#' @return Liste contenant les cartes produites.
#'
#' @details
#' Cette fonction génère :
#'
#' - carte biomasse,
#' - carte NDVI,
#' - carte capacité de charge,
#' - carte pression pastorale.
#'
#' Fonctionnalités :
#'
#' - légende automatique,
#' - export PNG,
#' - export PDF.
#'
#' Les cartes sont enregistrées dans le dossier
#' spécifié par l'utilisateur.
#'
#' @references
#' Hijmans, R.J. (2024).
#' terra: Spatial Data Analysis.
#'
#' @examples
#' \dontrun{
#' plot_rangeland_maps(
#'   biomass,
#'   ndvi,
#'   carrying_capacity,
#'   grazing_pressure
#' )
#' }
#'
#' @export

plot_rangeland_maps <- function(

  biomass,

  ndvi,

  carrying_capacity,

  grazing_pressure,

  output_dir = "outputs",

  format = "png"

){

  # ==========================
  # Verification
  # ==========================

  if(
    !format %in% c(
      "png",
      "pdf"
    )
  ){

    stop(
      "Format must be 'png' or 'pdf'"
    )

  }

  # ==========================
  # Creation dossier
  # ==========================

  if(
    !dir.exists(output_dir)
  ){

    dir.create(
      output_dir,
      recursive = TRUE
    )

  }

  # ==========================
  # Liste cartes
  # ==========================

  maps <- list(

    biomass = biomass,

    ndvi = ndvi,

    carrying_capacity = carrying_capacity,

    grazing_pressure = grazing_pressure

  )

  # ==========================
  # Export cartes
  # ==========================

  for(i in names(maps)){

    obj <- maps[[i]]

    filename <- file.path(

      output_dir,

      paste0(
        i,
        ".",
        format
      )

    )

    if(format == "png"){

      grDevices::png(

        filename,

        width = 1200,

        height = 800

      )

    } else {

      grDevices::pdf(

        filename,

        width = 10,

        height = 8

      )

    }

    terra::plot(

      obj,

      main = gsub(
        "_",
        " ",
        i
      )

    )

    grDevices::dev.off()

  }

  # ==========================
  # Output
  # ==========================

  return(

    maps

  )

}

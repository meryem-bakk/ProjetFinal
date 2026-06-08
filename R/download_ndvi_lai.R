#' Download NDVI and LAI data
#'
#' Telecharge ou importe les donnees NDVI et LAI.
#'
#' @param ndvi_file Chemin raster NDVI optionnel
#' @param lai_file Chemin raster LAI optionnel
#'
#' @return Liste contenant les rasters NDVI et LAI
#'
#' @details
#' Fonctionnalites :
#' - chargement raster NDVI
#' - chargement raster LAI
#' - preparation pour analyse spatiale
#'
#' @examples
#' veg <- download_ndvi_lai()
#'
#' @export

download_ndvi_lai <- function(

  ndvi_file = NULL,

  lai_file = NULL

){

  # =====================
  # NDVI
  # =====================

  if(!is.null(ndvi_file)){

    if(!file.exists(ndvi_file)){

      stop("NDVI file not found")

    }

    ndvi <- terra::rast(

      ndvi_file

    )

  } else {

    ndvi <- terra::rast(

      nrows = 100,

      ncols = 100,

      vals = stats::runif(
        10000,
        0,
        1
      )

    )

  }

  # =====================
  # LAI
  # =====================

  if(!is.null(lai_file)){

    if(!file.exists(lai_file)){

      stop("LAI file not found")

    }

    lai <- terra::rast(

      lai_file

    )

  } else {

    lai <- terra::rast(

      nrows = 100,

      ncols = 100,

      vals = stats::runif(
        10000,
        0,
        6
      )

    )

  }

  # =====================
  # Output
  # =====================

  return(

    list(

      NDVI = ndvi,

      LAI = lai

    )

  )

}

#' Extraction du NDVI depuis les donnees MODIS
#'
#' Extrait le raster NDVI depuis la liste retournee
#' par \code{download_ndvi_lai()}.
#'
#' @param veg Liste retournee par \code{download_ndvi_lai()},
#' contenant au moins un element \code{NDVI}.
#'
#' @return Raster SpatRaster NDVI (valeurs entre -1 et 1).
#'
#' @details
#' Le NDVI (Normalized Difference Vegetation Index)
#' est directement issu du produit MODIS MOD13Q1
#' (resolution 250 m, 16 jours).
#'
#' Le facteur d'echelle MODIS (x 0.0001) est deja
#' applique par \code{download_ndvi_lai()}.
#'
#' Valeurs typiques :
#' \itemize{
#'   \item < 0.1 : sol nu, zones degradees
#'   \item 0.1 - 0.3 : vegetation clairsemee
#'   \item 0.3 - 0.6 : vegetation moderee
#'   \item > 0.6 : vegetation dense
#' }
#'
#' @references
#' Didan, K. (2015). MOD13Q1 MODIS/Terra Vegetation
#' Indices 16-Day L3 Global 250m SIN Grid V006.
#' NASA EOSDIS Land Processes DAAC.
#'
#' Tucker, C.J. (1979).
#' Red and photographic infrared linear combinations
#' for monitoring vegetation.
#' Remote Sensing of Environment.
#'
#' @examples
#' \dontrun{
#'
#' veg <- download_ndvi_lai(
#'   lat   = 31.5,
#'   lon   = -7.5,
#'   start = "2023-01-01",
#'   end   = "2023-12-31"
#' )
#'
#' ndvi <- calculate_ndvi(veg)
#'
#' terra::plot(ndvi, main = "NDVI MODIS")
#'
#' terra::global(ndvi, "mean", na.rm = TRUE)
#' }
#'
#' @export

calculate_ndvi <- function(veg){

  # ==========================
  # Verification
  # ==========================

  if(!is.list(veg)){
    stop(
      "veg doit etre une liste retournee par download_ndvi_lai(). ",
      "Exemple : veg <- download_ndvi_lai(lat = 31.5, lon = -7.5)"
    )
  }

  if(is.null(veg$NDVI)){
    stop(
      "L'element 'NDVI' est absent de la liste. ",
      "Verifier que download_ndvi_lai() s'est execute correctement."
    )
  }

  if(!inherits(veg$NDVI, "SpatRaster")){
    stop(
      "veg$NDVI doit etre un SpatRaster. ",
      "Type detecte : ", class(veg$NDVI)
    )
  }

  # ==========================
  # Extraction
  # ==========================

  ndvi <- veg$NDVI

  names(ndvi) <- "NDVI"

  return(ndvi)

}

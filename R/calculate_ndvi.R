#' Calcul du NDVI (Normalized Difference Vegetation Index)
#'
#' Calcule le NDVI a partir des bandes NIR et Rouge.
#'
#' @param nir Bande proche infrarouge (valeur numerique ou SpatRaster).
#' @param red Bande rouge (valeur numerique ou SpatRaster).
#'
#' @return Valeurs ou raster NDVI compris entre -1 et 1.
#'
#' @details
#' Le NDVI est calcule selon :
#'
#' \deqn{
#' NDVI = \frac{NIR - Red}{NIR + Red}
#' }
#'
#' Les pixels ou NIR + Red = 0 produisent NaN
#' (comportement standard conforme a la norme NDVI).
#'
#' Compatible avec les scalaires, vecteurs et SpatRaster.
#'
#' @references
#' Tucker, C.J. (1979).
#' Red and photographic infrared linear combinations
#' for monitoring vegetation.
#' Remote Sensing of Environment.
#'
#' @examples
#' calculate_ndvi(0.8, 0.1)
#'
#' @export

calculate_ndvi <- function(nir, red){

  # ==========================
  # Verification types
  # ==========================

  if(inherits(nir, "SpatRaster") != inherits(red, "SpatRaster")){
    stop("nir et red doivent etre du meme type (scalaire ou SpatRaster).")
  }

  if(inherits(nir, "SpatRaster")){
    if(!terra::compareGeom(nir, red, stopOnError = FALSE)){
      stop("nir et red doivent avoir la meme geometrie (extent, resolution, CRS).")
    }
  }

  # ==========================
  # Calcul NDVI
  # (NIR + Red = 0 -> NaN, comportement standard)
  # ==========================

  ndvi <- (nir - red) / (nir + red)

  return(ndvi)

}

#' Calcul du NDVI (Normalized Difference Vegetation Index)
#'
#' Calcule le NDVI à partir des bandes NIR et Rouge.
#'
#' @param nir Bande proche infrarouge
#' @param red Bande rouge
#'
#' @return Valeurs ou raster NDVI
#'
#' @details
#' Le NDVI est calculé selon :
#'
#' \deqn{
#' NDVI = \frac{NIR - Red}{NIR + Red}
#' }
#'
#' Les valeurs sont généralement comprises
#' entre -1 et 1.
#'
#' @references
#' Tucker, C.J. (1979).
#' Red and photographic infrared linear combinations
#' for monitoring vegetation.
#'
#' @examples
#' calculate_ndvi(0.8, 0.1)
#'
#' @export

calculate_ndvi <- function(

  nir,

  red

){

  # ==========================
  # Verification
  # ==========================

  if(any((nir + red) == 0, na.rm = TRUE)){

    stop(

      "NIR + Red must not be equal to zero"

    )

  }

  # ==========================
  # Calcul NDVI
  # ==========================

  ndvi <-

    (nir - red) /

    (nir + red)

  # ==========================
  # Output
  # ==========================

  return(

    ndvi

  )

}

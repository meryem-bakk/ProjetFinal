#' Telechargement ou import des donnees NDVI et LAI
#'
#' Charge les donnees NDVI et LAI depuis des fichiers
#' raster locaux, ou les telecharge automatiquement
#' depuis MODIS via le package MODISTools.
#'
#' @param ndvi_file Chemin vers un fichier raster NDVI
#' local (GeoTIFF ou tout format lisible par terra).
#' Si NULL, telechargement MODIS automatique.
#'
#' @param lai_file Chemin vers un fichier raster LAI
#' local. Si NULL, telechargement MODIS automatique.
#'
#' @param site_name Nom du site d'etude (utilise lors
#' du telechargement MODIS).
#'
#' @param lat Latitude du centre de la zone d'etude.
#' Obligatoire si ndvi_file ou lai_file est NULL.
#'
#' @param lon Longitude du centre de la zone d'etude.
#' Obligatoire si ndvi_file ou lai_file est NULL.
#'
#' @param start Date de debut au format "YYYY-MM-DD".
#'
#' @param end Date de fin au format "YYYY-MM-DD".
#'
#' @param km_lr Extension en km vers la gauche/droite.
#'
#' @param km_ab Extension en km vers le haut/bas.
#'
#' @param path Dossier de stockage des fichiers telecharges.
#'
#' @return Liste nommee contenant :
#' \describe{
#'   \item{NDVI}{Raster SpatRaster NDVI (valeurs 0-1)}
#'   \item{LAI}{Raster SpatRaster LAI (m2/m2)}
#' }
#'
#' @details
#' Deux modes de fonctionnement :
#'
#' \strong{Mode local} _ si \code{ndvi_file} et/ou
#' \code{lai_file} sont fournis, la fonction charge
#' directement ces fichiers avec \code{terra::rast()}.
#'
#' \strong{Mode MODIS automatique} _ si aucun fichier
#' n'est fourni, la fonction utilise MODISTools pour
#' telecharger :
#' \itemize{
#'   \item NDVI : produit MOD13Q1 (250 m, 16 jours)
#'   \item LAI  : produit MOD15A2H (500 m, 8 jours)
#' }
#' Les donnees brutes sont converties en raster
#' \code{SpatRaster} georeference (CRS WGS84).
#'
#' Le facteur d'echelle MODIS est applique
#' automatiquement (NDVI x 0.0001, LAI x 0.1).
#'
#' @references
#' Didan, K. (2015). MOD13Q1 MODIS/Terra Vegetation
#' Indices 16-Day L3 Global 250m SIN Grid V006.
#' NASA EOSDIS Land Processes DAAC.
#'
#' Myneni, R. et al. (2015). MOD15A2H MODIS/Terra
#' Leaf Area Index/FPAR 8-Day L4 Global 500m SIN
#' Grid V006. NASA EOSDIS Land Processes DAAC.
#'
#' Tuck, S.L. et al. (2014). MODISTools: downloading
#' and processing MODIS remotely sensed data in R.
#' Ecology and Evolution.
#'
#' @examples
#' \dontrun{
#'
#' # Mode local
#' veg <- download_ndvi_lai(
#'   ndvi_file = "data/ndvi.tif",
#'   lai_file  = "data/lai.tif"
#' )
#'
#' # Mode MODIS automatique
#' veg <- download_ndvi_lai(
#'   lat   = 31.5,
#'   lon   = -7.5,
#'   start = "2023-01-01",
#'   end   = "2023-12-31"
#' )
#'
#' terra::plot(veg$NDVI)
#' terra::plot(veg$LAI)
#' }
#'
#' @export

download_ndvi_lai <- function(

  ndvi_file = NULL,
  lai_file  = NULL,

  site_name = "study_area",
  lat       = NULL,
  lon       = NULL,
  start     = "2023-01-01",
  end       = "2023-12-31",
  km_lr     = 10,
  km_ab     = 10,
  path      = "modis_data"

){

  # ==========================
  # Fonction interne : MODIS -> SpatRaster
  # ==========================

  .modis_to_raster <- function(df, scale_factor){

    # Supprimer les valeurs hors plage MODIS
    df <- df[df$value > -3000, ]

    # Appliquer le facteur d'echelle
    df$value <- df$value * scale_factor

    # Moyenne par pixel si plusieurs dates
    df_mean <- stats::aggregate(
      value ~ latitude + longitude,
      data = df,
      FUN  = mean,
      na.rm = TRUE
    )

    # Conversion en SpatRaster georeference
    rast_obj <- terra::rast(
      df_mean[, c("longitude", "latitude", "value")],
      type = "xyz",
      crs  = "EPSG:4326"
    )

    return(rast_obj)

  }

  # ==========================
  # NDVI
  # ==========================

  if(!is.null(ndvi_file)){

    if(!file.exists(ndvi_file)){
      stop("Fichier NDVI introuvable : ", ndvi_file)
    }

    ndvi <- terra::rast(ndvi_file)

  } else {

    if(is.null(lat) || is.null(lon)){
      stop(
        "lat et lon sont obligatoires pour le ",
        "telechargement MODIS automatique."
      )
    }

    if(!requireNamespace("MODISTools", quietly = TRUE)){
      stop(
        "Le package MODISTools est requis. ",
        "Installez-le avec : install.packages('MODISTools')"
      )
    }

    message("Telechargement NDVI (MOD13Q1)...")

    ndvi_raw <- MODISTools::mt_subset(
      product    = "MOD13Q1",
      band       = "250m_16_days_NDVI",
      lat        = lat,
      lon        = lon,
      start      = start,
      end        = end,
      km_lr      = km_lr,
      km_ab      = km_ab,
      site_name  = site_name,
      internal   = TRUE,
      progress   = FALSE
    )

    ndvi <- .modis_to_raster(ndvi_raw, scale_factor = 0.0001)

    # Sauvegarde locale optionnelle
    if(!dir.exists(path)){
      dir.create(path, recursive = TRUE)
    }

    terra::writeRaster(
      ndvi,
      filename  = file.path(path, "ndvi.tif"),
      overwrite = TRUE
    )

  }

  # ==========================
  # LAI
  # ==========================

  if(!is.null(lai_file)){

    if(!file.exists(lai_file)){
      stop("Fichier LAI introuvable : ", lai_file)
    }

    lai <- terra::rast(lai_file)

  } else {

    if(is.null(lat) || is.null(lon)){
      stop(
        "lat et lon sont obligatoires pour le ",
        "telechargement MODIS automatique."
      )
    }

    if(!requireNamespace("MODISTools", quietly = TRUE)){
      stop(
        "Le package MODISTools est requis. ",
        "Installez-le avec : install.packages('MODISTools')"
      )
    }

    message("Telechargement LAI (MOD15A2H)...")

    lai_raw <- MODISTools::mt_subset(
      product    = "MOD15A2H",
      band       = "Lai_500m",
      lat        = lat,
      lon        = lon,
      start      = start,
      end        = end,
      km_lr      = km_lr,
      km_ab      = km_ab,
      site_name  = site_name,
      internal   = TRUE,
      progress   = FALSE
    )

    lai <- .modis_to_raster(lai_raw, scale_factor = 0.1)

    terra::writeRaster(
      lai,
      filename  = file.path(path, "lai.tif"),
      overwrite = TRUE
    )

  }

  # ==========================
  # Output
  # ==========================

  return(

    list(
      NDVI = ndvi,
      LAI  = lai
    )

  )

}

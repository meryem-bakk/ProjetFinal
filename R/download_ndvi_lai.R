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
#' @param boundary_file Chemin vers un fichier spatial
#' de la zone d'etude (.shp ou .geojson).
#' Si fourni, le centre de la zone est extrait
#' automatiquement et utilise pour le telechargement MODIS.
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
#' \strong{Mode local} - si \code{ndvi_file} et/ou
#' \code{lai_file} sont fournis, la fonction charge
#' directement ces fichiers avec \code{terra::rast()}.
#'
#' \strong{Mode MODIS automatique} - si aucun fichier
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
#' Les pixels invalides (valeur < -3000) sont remplaces
#' par NA avant la moyenne temporelle, sans decaler
#' les indices de position dans le raster.
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
#' # Mode MODIS automatique avec coordonnees
#' veg <- download_ndvi_lai(
#'   lat   = 31.5,
#'   lon   = -7.5,
#'   start = "2023-01-01",
#'   end   = "2023-12-31"
#' )
#'
#' # Mode MODIS automatique avec shapefile
#' veg <- download_ndvi_lai(
#'   boundary_file = "study_area.shp"
#' )
#'
#' terra::plot(veg$NDVI)
#' terra::plot(veg$LAI)
#' }
#'
#' @export

download_ndvi_lai <- function(

  ndvi_file     = NULL,
  lai_file      = NULL,
  boundary_file = NULL,
  site_name     = "study_area",
  lat           = NULL,
  lon           = NULL,
  start         = "2023-01-01",
  end           = "2023-12-31",
  km_lr         = 10,
  km_ab         = 10,
  path          = "modis_data"

){

  # ==========================
  # Zone d'etude via SHP/GeoJSON
  # ==========================

  if(!is.null(boundary_file)){

    if(!file.exists(boundary_file)){
      stop("Fichier spatial introuvable : ", boundary_file)
    }

    boundary <- sf::st_read(boundary_file, quiet = TRUE)

    centroid <- sf::st_centroid(sf::st_union(boundary))

    coords <- sf::st_coordinates(centroid)

    lon <- coords[1]
    lat <- coords[2]

    message(
      "Centre de la zone d'etude : ",
      round(lat, 4), ", ", round(lon, 4)
    )

  }

  # ==========================
  # Fonction interne : MODIS -> SpatRaster
  # ==========================

  .modis_to_raster <- function(df, scale_factor){

    # Remplacer les valeurs hors plage par NA
    # (sans supprimer les lignes — preserve les indices pixel)
    df$value[df$value <= -3000] <- NA

    # Appliquer le facteur d'echelle
    df$value <- df$value * scale_factor

    # Extraire les dimensions et l'emprise depuis les metadonnees
    nrows    <- unique(df$nrows)[1]
    ncols    <- unique(df$ncols)[1]
    xll      <- as.numeric(unique(df$xllcorner)[1])
    yll      <- as.numeric(unique(df$yllcorner)[1])
    cellsize <- as.numeric(unique(df$cellsize)[1])

    n_pixels <- nrows * ncols

    # Grille complete de reference (tous les pixels 1..N)
    all_pixels <- data.frame(pixel = seq_len(n_pixels))

    # Moyenne temporelle par pixel (NA ignores)
    df_mean <- stats::aggregate(
      value ~ pixel,
      data  = df,
      FUN   = mean,
      na.rm = TRUE
    )

    # Fusion sur la grille complete :
    # les pixels entierement NA restent NA (pas de decalage)
    df_full <- merge(
      all_pixels,
      df_mean,
      by    = "pixel",
      all.x = TRUE
    )

    # Tri par pixel pour garantir l'ordre raster
    df_full <- df_full[order(df_full$pixel), ]

    # Construction du SpatRaster georeference
    r <- terra::rast(
      nrows = nrows,
      ncols = ncols,
      xmin  = xll,
      xmax  = xll + ncols * cellsize,
      ymin  = yll,
      ymax  = yll + nrows * cellsize,
      crs   = "EPSG:4326"
    )

    terra::values(r) <- df_full$value

    return(r)

  }

  # ==========================
  # Verification MODISTools
  # ==========================

  .check_modistools <- function(){
    if(!requireNamespace("MODISTools", quietly = TRUE)){
      stop(
        "Le package MODISTools est requis. ",
        "Installez-le avec : install.packages('MODISTools')"
      )
    }
  }

  .check_coords <- function(){
    if(is.null(lat) || is.null(lon)){
      stop(
        "lat et lon sont obligatoires pour le ",
        "telechargement MODIS automatique."
      )
    }
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

    .check_coords()
    .check_modistools()

    message("Telechargement NDVI (MOD13Q1)...")

    ndvi_raw <- MODISTools::mt_subset(
      product   = "MOD13Q1",
      band      = "250m_16_days_NDVI",
      lat       = lat,
      lon       = lon,
      start     = start,
      end       = end,
      km_lr     = km_lr,
      km_ab     = km_ab,
      site_name = site_name,
      internal  = TRUE,
      progress  = FALSE
    )

    ndvi <- .modis_to_raster(ndvi_raw, scale_factor = 0.0001)

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

    .check_coords()
    .check_modistools()

    message("Telechargement LAI (MOD15A2H)...")

    lai_raw <- MODISTools::mt_subset(
      product   = "MOD15A2H",
      band      = "Lai_500m",
      lat       = lat,
      lon       = lon,
      start     = start,
      end       = end,
      km_lr     = km_lr,
      km_ab     = km_ab,
      site_name = site_name,
      internal  = TRUE,
      progress  = FALSE
    )

    lai <- .modis_to_raster(lai_raw, scale_factor = 0.1)

    if(!dir.exists(path)){
      dir.create(path, recursive = TRUE)
    }

    terra::writeRaster(
      lai,
      filename  = file.path(path, "lai.tif"),
      overwrite = TRUE
    )

  }

  # ==========================
  # Alignement LAI sur grille NDVI
  # (MOD13Q1 = 250m, MOD15A2H = 500m)
  # Resample bilineaire si resolutions differentes
  # ==========================

  if(!terra::compareGeom(ndvi, lai, stopOnError = FALSE)){

    message(
      "Resolutions differentes detectees (NDVI 250m / LAI 500m). ",
      "Reechantillonnage du LAI sur la grille NDVI..."
    )

    lai <- terra::resample(lai, ndvi, method = "bilinear")

    # Mettre a jour le fichier sauvegarde
    if(!is.null(lai_file) == FALSE){
      terra::writeRaster(
        lai,
        filename  = file.path(path, "lai.tif"),
        overwrite = TRUE
      )
    }

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

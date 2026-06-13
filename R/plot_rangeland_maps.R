#' Cartographie pastorale
#'
#' Produit les cartes finales du package rangelandSDM
#' avec legendes, titres et palettes adaptes.
#'
#' @param biomass Raster de biomasse (SpatRaster).
#' @param ndvi Raster NDVI (SpatRaster).
#' @param carrying_capacity Raster de capacite de charge (SpatRaster).
#' @param grazing_pressure Raster de pression pastorale (SpatRaster).
#' @param output_dir Dossier d'export. Par defaut "outputs".
#' @param format Format d'export : "png" ou "pdf".
#' @param species_name Nom de l'espece fourragere (pour le titre).
#' @param zone_name Nom de la zone d'etude (pour le titre).
#'
#' @return Vecteur des chemins des fichiers produits (invisible).
#'
#' @details
#' Produit 5 cartes :
#' \itemize{
#'   \item NDVI (palette RdYlGn)
#'   \item Biomasse en kg/ha (palette YlOrBr)
#'   \item Capacite de charge en UBT/ha (palette Blues)
#'   \item Pression pastorale (palette RdYlGn inversee)
#'   \item Dashboard 4 cartes reunies
#' }
#'
#' @references
#' Hijmans, R.J. (2024). terra: Spatial Data Analysis.
#'
#' @examples
#' \dontrun{
#' plot_rangeland_maps(
#'   biomass           = biomass,
#'   ndvi              = ndvi,
#'   carrying_capacity = capacity$capacity,
#'   grazing_pressure  = pressure,
#'   output_dir        = "outputs",
#'   format            = "png"
#' )
#' }
#'
#' @export

plot_rangeland_maps <- function(

  biomass,
  ndvi,
  carrying_capacity,
  grazing_pressure,
  output_dir   = "outputs",
  format       = "png",
  species_name = "Stipa tenacissima",
  zone_name    = "Maroc semi-aride"

){

  # ==========================
  # Verification
  # ==========================

  if(!format %in% c("png", "pdf")){
    stop("Format doit etre 'png' ou 'pdf'.")
  }

  for(nm in c("biomass", "ndvi", "carrying_capacity", "grazing_pressure")){
    obj <- get(nm)
    if(!inherits(obj, "SpatRaster")){
      stop(nm, " doit etre un SpatRaster.")
    }
  }

  if(!dir.exists(output_dir)){
    dir.create(output_dir, recursive = TRUE)
  }

  # ==========================
  # Palettes de couleurs
  # ==========================

  pal_ndvi     <- grDevices::colorRampPalette(
    c("#d73027", "#fee08b", "#1a9850")
  )(100)

  pal_biomass  <- grDevices::colorRampPalette(
    c("#ffffd4", "#fed98e", "#fe9929", "#d95f0e", "#993404")
  )(100)

  pal_capacity <- grDevices::colorRampPalette(
    c("#deebf7", "#9ecae1", "#3182bd", "#08306b")
  )(100)

  pal_pressure <- grDevices::colorRampPalette(
    c("#1a9850", "#fee08b", "#d73027")
  )(100)

  # ==========================
  # Fonction interne : ouvrir device
  # ==========================

  .open_device <- function(path, format){
    if(format == "png"){
      grDevices::png(path, width = 1400, height = 1000, res = 150)
    } else {
      grDevices::pdf(path, width = 10, height = 7)
    }
  }

  paths <- c()

  # ==========================
  # Carte 1 : NDVI
  # ==========================

  path_ndvi <- file.path(output_dir, paste0("ndvi.", format))
  .open_device(path_ndvi, format)

  terra::plot(
    ndvi,
    col   = pal_ndvi,
    main  = paste0("NDVI — ", species_name, "\n", zone_name),
    range = c(0, 0.6),
    mar   = c(3, 3, 3, 5)
  )

  grDevices::dev.off()
  paths <- c(paths, path_ndvi)
  message("ndvi.", format, " genere")

  # ==========================
  # Carte 2 : Biomasse
  # ==========================

  path_bio <- file.path(output_dir, paste0("biomass.", format))
  .open_device(path_bio, format)

  terra::plot(
    biomass,
    col  = pal_biomass,
    main = paste0("Biomasse vegetale (kg/ha)\n", zone_name),
    mar  = c(3, 3, 3, 5)
  )

  grDevices::dev.off()
  paths <- c(paths, path_bio)
  message("biomass.", format, " genere")

  # ==========================
  # Carte 3 : Capacite de charge
  # ==========================

  path_cap <- file.path(output_dir, paste0("carrying_capacity.", format))
  .open_device(path_cap, format)

  terra::plot(
    carrying_capacity,
    col  = pal_capacity,
    main = paste0("Capacite de charge (UBT/ha)\n", zone_name),
    mar  = c(3, 3, 3, 5)
  )

  grDevices::dev.off()
  paths <- c(paths, path_cap)
  message("carrying_capacity.", format, " genere")

  # ==========================
  # Carte 4 : Pression pastorale
  # ==========================

  path_pres <- file.path(output_dir, paste0("grazing_pressure.", format))
  .open_device(path_pres, format)

  terra::plot(
    grazing_pressure,
    col  = pal_pressure,
    main = paste0("Pression pastorale\n", zone_name),
    mar  = c(3, 3, 3, 5)
  )

  grDevices::dev.off()
  paths <- c(paths, path_pres)
  message("grazing_pressure.", format, " genere")

  # ==========================
  # Carte 5 : Dashboard 2x2
  # ==========================

  path_dash <- file.path(output_dir, paste0("dashboard_rangeland.", format))

  if(format == "png"){
    grDevices::png(path_dash, width = 2400, height = 1800, res = 150)
  } else {
    grDevices::pdf(path_dash, width = 14, height = 10)
  }

  graphics::par(mfrow = c(2, 2), mar = c(3, 3, 3, 5))

  terra::plot(ndvi,             col = pal_ndvi,     main = "NDVI",
              range = c(0, 0.6))
  terra::plot(biomass,          col = pal_biomass,  main = "Biomasse (kg/ha)")
  terra::plot(carrying_capacity,col = pal_capacity, main = "Capacite de charge (UBT/ha)")
  terra::plot(grazing_pressure, col = pal_pressure, main = "Pression pastorale")

  graphics::mtext(
    paste0("rangelandSDM — ", species_name, " — ", zone_name),
    side = 3, line = -1.5, outer = TRUE, cex = 1.1, font = 2
  )

  grDevices::dev.off()
  graphics::par(mfrow = c(1, 1))

  paths <- c(paths, path_dash)
  message("dashboard_rangeland.", format, " genere")

  # ==========================
  # Output
  # ==========================

  message("\n", length(paths), " cartes exportees dans : ", output_dir)
  return(invisible(paths))

}

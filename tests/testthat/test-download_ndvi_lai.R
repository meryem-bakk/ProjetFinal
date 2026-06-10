test_that("download_ndvi_lai charge des fichiers locaux", {
  # Créer deux rasters temporaires
  ndvi_tmp <- tempfile(fileext = ".tif")
  lai_tmp  <- tempfile(fileext = ".tif")

  r <- terra::rast(
    nrows = 5, ncols = 5,
    xmin = -8, xmax = -7, ymin = 31, ymax = 32,
    crs = "EPSG:4326"
  )
  terra::values(r) <- runif(25, 0.1, 0.6)
  terra::writeRaster(r, ndvi_tmp, overwrite = TRUE)
  terra::writeRaster(r, lai_tmp,  overwrite = TRUE)

  result <- download_ndvi_lai(
    ndvi_file = ndvi_tmp,
    lai_file  = lai_tmp
  )

  expect_named(result, c("NDVI", "LAI"))
  expect_true(inherits(result$NDVI, "SpatRaster"))
  expect_true(inherits(result$LAI,  "SpatRaster"))

  unlink(c(ndvi_tmp, lai_tmp))
})

test_that("download_ndvi_lai erreur si fichier NDVI introuvable", {
  expect_error(
    download_ndvi_lai(ndvi_file = "inexistant.tif", lai_file = "inexistant.tif"),
    "introuvable"
  )
})

test_that("download_ndvi_lai erreur si lat/lon manquants en mode auto", {
  expect_error(
    download_ndvi_lai(),
    "lat et lon"
  )
})

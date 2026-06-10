test_that("prepare_predictors retourne un dataframe avec colonne presence", {
  # Raster predicteur simple
  r <- terra::rast(
    nrows = 10, ncols = 10,
    xmin = -10, xmax = 0, ymin = 30, ymax = 40,
    crs = "EPSG:4326"
  )
  terra::values(r) <- runif(100)
  names(r) <- "bio_1"

  # Points sf
  pts <- sf::st_as_sf(
    data.frame(
      species   = rep("A", 5),
      longitude = c(-9, -8, -7, -6, -5),
      latitude  = c(31, 32, 33, 34, 35)
    ),
    coords = c("longitude", "latitude"),
    crs    = 4326
  )

  result <- prepare_predictors(occurrence = pts, predictors = r, remove_cor = FALSE)

  expect_true(is.data.frame(result))
  expect_true("presence" %in% names(result))
  expect_true(all(result$presence == 1))
})

test_that("prepare_predictors erreur si occurrence non sf", {
  r <- terra::rast(nrows = 5, ncols = 5,
                   xmin = -8, xmax = -7, ymin = 31, ymax = 32,
                   crs = "EPSG:4326")
  terra::values(r) <- runif(25)
  expect_error(
    prepare_predictors(occurrence = data.frame(x = 1), predictors = r),
    "sf"
  )
})

test_that("prepare_predictors ne supprime pas la variable cible lors du filtrage cor", {
  r <- terra::rast(
    nrows = 10, ncols = 10,
    xmin = -10, xmax = 0, ymin = 30, ymax = 40,
    crs = "EPSG:4326"
  )
  # Deux couches très corrélées
  vals <- runif(100)
  terra::values(r) <- vals
  r2 <- r
  r3 <- r
  terra::values(r3) <- vals * 1.001  # quasi identique -> correlation > 0.9
  rstack <- c(r2, r3)
  names(rstack) <- c("bio_1", "bio_2")

  pts <- sf::st_as_sf(
    data.frame(longitude = seq(-9, -5, length.out = 8),
               latitude  = seq(31, 38, length.out = 8)),
    coords = c("longitude", "latitude"), crs = 4326
  )

  result <- prepare_predictors(pts, rstack, remove_cor = TRUE)

  # La variable cible "presence" doit toujours etre presente
  expect_true("presence" %in% names(result))
})

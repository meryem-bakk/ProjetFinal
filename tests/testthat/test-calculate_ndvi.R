test_that("calculate_ndvi extrait correctement le NDVI", {

  ndvi_rast <- terra::rast(
    nrows = 5, ncols = 5,
    xmin = -8, xmax = -7,
    ymin = 31, ymax = 32,
    crs = "EPSG:4326"
  )
  terra::values(ndvi_rast) <- seq(0.1, 0.5, length.out = 25)

  veg <- list(NDVI = ndvi_rast, LAI = ndvi_rast)

  result <- calculate_ndvi(veg)

  expect_true(inherits(result, "SpatRaster"))
  expect_equal(names(result), "NDVI")
  expect_equal(
    as.vector(terra::values(result)),
    as.vector(terra::values(ndvi_rast))
  )

})

test_that("calculate_ndvi erreur si veg n'est pas une liste", {
  expect_error(calculate_ndvi("pas_une_liste"), "liste")
})

test_that("calculate_ndvi erreur si NDVI absent", {
  expect_error(calculate_ndvi(list(LAI = terra::rast())), "absent")
})

test_that("calculate_ndvi erreur si NDVI n'est pas SpatRaster", {
  expect_error(calculate_ndvi(list(NDVI = c(0.1, 0.2))), "SpatRaster")
})

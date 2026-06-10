test_that("calculate_biomass NDVI seul", {
  expect_equal(calculate_biomass(ndvi = 0.5), 500)
  expect_equal(calculate_biomass(ndvi = 1.0), 1000)
  expect_equal(calculate_biomass(ndvi = 0.0), 0)
})

test_that("calculate_biomass NDVI + LAI", {
  expect_equal(calculate_biomass(ndvi = 0.5, lai = 2), 800)
  # 800 * 0.5 + 200 * 2 = 400 + 400 = 800
})

test_that("calculate_biomass sur SpatRaster", {
  r <- terra::rast(nrows = 3, ncols = 3,
                   xmin = -8, xmax = -7, ymin = 31, ymax = 32,
                   crs = "EPSG:4326")
  terra::values(r) <- rep(0.4, 9)
  result <- calculate_biomass(ndvi = r)
  expect_true(inherits(result, "SpatRaster"))
  expect_equal(unique(as.vector(terra::values(result))), 400)
})

test_that("calculate_biomass erreur si methode invalide", {
  expect_error(calculate_biomass(ndvi = 0.5, method = "invalid"))
})

test_that("calculate_biomass erreur si ndvi manquant", {
  expect_error(calculate_biomass())
})

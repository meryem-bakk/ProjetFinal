test_that("pression pastorale avec densite animale", {
  result <- calculate_grazing_pressure(
    biomass           = 500,
    carrying_capacity = 2,
    animal_density    = 1
  )
  expect_equal(result, 0.5)
})

test_that("pression pastorale sans densite animale (indice relatif)", {
  result <- calculate_grazing_pressure(
    biomass           = c(200, 400, 800),
    carrying_capacity = 2
  )
  expect_equal(result, c(0.25, 0.5, 1.0))
})

test_that("erreur si carrying_capacity <= 0", {
  expect_error(
    calculate_grazing_pressure(biomass = 500, carrying_capacity = 0),
    "strictement positive"
  )
  expect_error(
    calculate_grazing_pressure(biomass = 500, carrying_capacity = -1),
    "strictement positive"
  )
})

test_that("pression pastorale sur SpatRaster", {
  r <- terra::rast(nrows = 3, ncols = 3,
                   xmin = -8, xmax = -7, ymin = 31, ymax = 32,
                   crs = "EPSG:4326")
  terra::values(r) <- rep(2, 9)
  result <- calculate_grazing_pressure(
    biomass           = r,
    carrying_capacity = r,
    animal_density    = 1
  )
  expect_true(inherits(result, "SpatRaster"))
  expect_equal(unique(as.vector(terra::values(result))), 0.5)
})

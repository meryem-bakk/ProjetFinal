test_that("capacite de charge scalaire", {
  result <- estimate_carrying_capacity(biomass = 500, animal_demand = 250)
  expect_equal(result$capacity, 2)
})

test_that("classification pression pastorale scalaire", {
  low  <- estimate_carrying_capacity(biomass = 100, animal_demand = 250)
  mid  <- estimate_carrying_capacity(biomass = 200, animal_demand = 250)
  high <- estimate_carrying_capacity(biomass = 400, animal_demand = 250)
  expect_equal(as.character(low$pressure),  "Faible")
  expect_equal(as.character(mid$pressure),  "Moderee")
  expect_equal(as.character(high$pressure), "Elevee")
})

test_that("capacite de charge sur SpatRaster", {
  r <- terra::rast(nrows = 3, ncols = 3,
                   xmin = -8, xmax = -7, ymin = 31, ymax = 32,
                   crs = "EPSG:4326")
  terra::values(r) <- rep(500, 9)
  result <- estimate_carrying_capacity(r, animal_demand = 250)
  expect_true(inherits(result$capacity, "SpatRaster"))
  expect_equal(unique(as.vector(terra::values(result$capacity))), 2)
})

test_that("erreur si animal_demand <= 0", {
  expect_error(estimate_carrying_capacity(500, animal_demand = 0))
  expect_error(estimate_carrying_capacity(500, animal_demand = -10))
})

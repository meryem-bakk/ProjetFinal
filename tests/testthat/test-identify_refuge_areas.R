test_that("identify_refuge_areas retourne refuge_index et priority_area", {
  result <- identify_refuge_areas(
    biomass          = c(200, 400, 800, 1000),
    grazing_pressure = c(0.5, 0.8, 1.2, 0.3),
    climate_score    = 0.8
  )
  expect_named(result, c("refuge_index", "priority_area"))
  expect_length(result$refuge_index, 4)
  expect_type(result$priority_area, "logical")
})

test_that("identify_refuge_areas warning si grazing_pressure <= 0", {
  expect_warning(
    identify_refuge_areas(
      biomass          = c(500, 800),
      grazing_pressure = c(0, 0.5)
    ),
    "NA"
  )
})

test_that("identify_refuge_areas sur SpatRaster", {
  r_bio <- terra::rast(nrows = 3, ncols = 3,
                       xmin = -8, xmax = -7, ymin = 31, ymax = 32,
                       crs = "EPSG:4326")
  terra::values(r_bio) <- c(200, 400, 600, 800, 1000, 500, 300, 700, 900)

  r_gp <- terra::rast(nrows = 3, ncols = 3,
                      xmin = -8, xmax = -7, ymin = 31, ymax = 32,
                      crs = "EPSG:4326")
  terra::values(r_gp) <- rep(0.5, 9)

  result <- identify_refuge_areas(r_bio, r_gp)
  expect_true(inherits(result$refuge_index,  "SpatRaster"))
  expect_true(inherits(result$priority_area, "SpatRaster"))
})

test_that("priority_area : environ 50% de TRUE", {
  result <- identify_refuge_areas(
    biomass          = 1:10,
    grazing_pressure = rep(0.5, 10)
  )
  prop_true <- mean(result$priority_area)
  expect_gte(prop_true, 0.4)
  expect_lte(prop_true, 0.6)
})

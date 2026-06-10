test_that("clean_occurrences supprime les doublons spatiaux", {
  df <- data.frame(
    species   = c("A", "A", "B"),
    longitude = c(10, 10, 20),
    latitude  = c(30, 30, 40)
  )
  sf_obj <- sf::st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)

  result <- clean_occurrences(sf_obj)
  expect_equal(nrow(result), 2)
})

test_that("clean_occurrences supprime les geometries vides", {
  df <- data.frame(species = "A", longitude = 10, latitude = 30)
  sf_ok    <- sf::st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)
  sf_empty <- sf::st_sf(
    species  = "B",
    geometry = sf::st_sfc(sf::st_point(), crs = 4326)
  )
  sf_combined <- rbind(sf_ok, sf_empty)

  result <- clean_occurrences(sf_combined)
  expect_equal(nrow(result), 1)
})

test_that("clean_occurrences erreur si input non sf", {
  expect_error(clean_occurrences(data.frame(x = 1)), "sf")
})

test_that("clean_occurrences retourne un objet sf", {
  df  <- data.frame(species = "A", longitude = 5, latitude = 35)
  sf_obj <- sf::st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)
  result <- clean_occurrences(sf_obj)
  expect_true(inherits(result, "sf"))
})

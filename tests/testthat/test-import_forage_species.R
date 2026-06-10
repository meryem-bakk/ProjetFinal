test_that("import_forage_species lit un CSV valide", {
  tmp <- tempfile(fileext = ".csv")
  write.csv(
    data.frame(
      species   = rep("Stipa tenacissima", 5),
      longitude = c(-5.3, -4.8, -6.1, -3.4, -5.7),
      latitude  = c(33.8, 34.1, 33.0, 35.2, 33.4)
    ),
    tmp, row.names = FALSE
  )
  result <- import_forage_species(csv_file = tmp)
  expect_named(result, c("sf_object", "dataframe"))
  expect_true(inherits(result$sf_object, "sf"))
  expect_equal(nrow(result$dataframe), 5)
  unlink(tmp)
})

test_that("import_forage_species supprime les doublons", {
  tmp <- tempfile(fileext = ".csv")
  write.csv(
    data.frame(
      longitude = c(-5.3, -5.3, -4.8),
      latitude  = c(33.8, 33.8, 34.1)
    ),
    tmp, row.names = FALSE
  )
  result <- import_forage_species(csv_file = tmp)
  expect_equal(nrow(result$dataframe), 2)
  unlink(tmp)
})

test_that("import_forage_species supprime coordonnees invalides", {
  tmp <- tempfile(fileext = ".csv")
  write.csv(
    data.frame(
      longitude = c(-5.3, 200, -4.8),
      latitude  = c(33.8, 34.1, 95)
    ),
    tmp, row.names = FALSE
  )
  result <- import_forage_species(csv_file = tmp)
  expect_equal(nrow(result$dataframe), 1)
  unlink(tmp)
})

test_that("import_forage_species erreur si ni species_name ni csv_file", {
  expect_error(import_forage_species(), "species_name")
})

test_that("import_forage_species erreur si CSV introuvable", {
  expect_error(
    import_forage_species(csv_file = "inexistant.csv"),
    "introuvable"
  )
})

test_that("import_forage_species erreur si colonnes manquantes", {
  tmp <- tempfile(fileext = ".csv")
  write.csv(data.frame(species = "A", x = 1), tmp, row.names = FALSE)
  expect_error(import_forage_species(csv_file = tmp), "manquantes")
  unlink(tmp)
})

test_that("import_forage_species CRS est WGS84", {
  tmp <- tempfile(fileext = ".csv")
  write.csv(
    data.frame(longitude = -5.3, latitude = 33.8),
    tmp, row.names = FALSE
  )
  result <- import_forage_species(csv_file = tmp)
  expect_equal(sf::st_crs(result$sf_object)$epsg, 4326)
  unlink(tmp)
})

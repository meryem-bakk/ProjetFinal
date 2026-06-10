test_that("import_biomass_data lit un CSV valide", {
  tmp <- tempfile(fileext = ".csv")
  write.csv(
    data.frame(plot = 1:3, biomass = c(300, 500, 800)),
    tmp, row.names = FALSE
  )
  result <- import_biomass_data(tmp)
  expect_true(is.data.frame(result))
  expect_true("biomass" %in% names(result))
  expect_equal(nrow(result), 3)
  unlink(tmp)
})

test_that("import_biomass_data supprime les NA", {
  tmp <- tempfile(fileext = ".csv")
  write.csv(
    data.frame(plot = 1:4, biomass = c(300, NA, 500, NA)),
    tmp, row.names = FALSE
  )
  result <- import_biomass_data(tmp)
  expect_equal(nrow(result), 2)
  unlink(tmp)
})

test_that("import_biomass_data warning et conversion t/ha -> kg/ha", {
  tmp <- tempfile(fileext = ".csv")
  write.csv(
    data.frame(plot = 1:3, biomass = c(0.5, 1.2, 0.8)),
    tmp, row.names = FALSE
  )
  expect_warning(result <- import_biomass_data(tmp), "10 000")
  expect_equal(result$biomass[1], 5000)
  unlink(tmp)
})

test_that("import_biomass_data erreur si fichier introuvable", {
  expect_error(import_biomass_data("fichier_inexistant.csv"), "introuvable")
})

test_that("import_biomass_data erreur si colonne biomass absente", {
  tmp <- tempfile(fileext = ".csv")
  write.csv(data.frame(plot = 1:3, poids = c(300, 500, 800)), tmp, row.names = FALSE)
  expect_error(import_biomass_data(tmp), "biomass")
  unlink(tmp)
})

test_that("import_biomass_data erreur si format non supporte", {
  tmp <- tempfile(fileext = ".txt")
  writeLines("test", tmp)
  expect_error(import_biomass_data(tmp), "non supporte")
  unlink(tmp)
})

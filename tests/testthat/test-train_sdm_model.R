# Donnees de test communes
make_test_data <- function(n = 50){
  set.seed(42)
  data.frame(
    presence = factor(sample(c(0, 1), n, replace = TRUE)),
    bio_1    = runif(n, 10, 30),
    bio_12   = runif(n, 100, 800)
  )
}

test_that("train_sdm_model retourne les elements attendus", {
  data   <- make_test_data()
  result <- train_sdm_model(data, ntree_values = c(50))

  expect_named(
    result,
    c("model", "best_ntree", "train_accuracy",
      "test_accuracy", "importance", "suitability_raster")
  )
  expect_true(inherits(result$model, "randomForest"))
  expect_equal(result$best_ntree, 50)
  expect_gte(result$train_accuracy, 0)
  expect_lte(result$train_accuracy, 1)
  expect_gte(result$test_accuracy,  0)
  expect_lte(result$test_accuracy,  1)
})

test_that("train_sdm_model erreur si variable cible absente", {
  data <- make_test_data()
  expect_error(
    train_sdm_model(data, target = "absent"),
    "introuvable"
  )
})

test_that("train_sdm_model suitability_raster NULL si pas de raster fourni", {
  data   <- make_test_data()
  result <- train_sdm_model(data, ntree_values = c(50))
  expect_null(result$suitability_raster)
})

test_that("train_sdm_model suitability_raster produit si raster fourni", {
  data <- make_test_data()

  r <- terra::rast(
    nrows = 5, ncols = 5,
    xmin = -8, xmax = -7, ymin = 31, ymax = 32,
    crs = "EPSG:4326"
  )
  r2 <- r
  terra::values(r)  <- runif(25, 10, 30)
  terra::values(r2) <- runif(25, 100, 800)
  rstack <- c(r, r2)
  names(rstack) <- c("bio_1", "bio_12")

  result <- train_sdm_model(data, predictors_raster = rstack, ntree_values = c(50))

  expect_true(inherits(result$suitability_raster, "SpatRaster"))
  expect_equal(names(result$suitability_raster), "suitability")

  vals <- terra::values(result$suitability_raster)
  expect_true(all(vals >= 0 & vals <= 1, na.rm = TRUE))
})

test_that("importance des variables est une matrice", {
  data   <- make_test_data()
  result <- train_sdm_model(data, ntree_values = c(50))
  expect_true(is.matrix(result$importance))
})

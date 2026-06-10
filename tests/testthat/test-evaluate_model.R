test_that("evaluate_model retourne AUC, Accuracy, RMSE corrects", {
  observed  <- c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0)
  predicted <- c(0.9, 0.1, 0.8, 0.7, 0.2, 0.85, 0.15, 0.3, 0.75, 0.2)

  result <- evaluate_model(observed, predicted)

  expect_true(is.data.frame(result$performance))
  expect_named(result$performance, c("AUC", "Accuracy", "RMSE"))
  expect_gte(result$performance$AUC,      0)
  expect_lte(result$performance$AUC,      1)
  expect_gte(result$performance$Accuracy, 0)
  expect_lte(result$performance$Accuracy, 1)
  expect_gte(result$performance$RMSE,     0)
})

test_that("evaluate_model erreur si longueurs differentes", {
  expect_error(
    evaluate_model(c(1, 0, 1), c(0.9, 0.1)),
    "meme longueur"
  )
})

test_that("evaluate_model erreur si observed non binaire", {
  expect_error(
    evaluate_model(c(1, 2, 3), c(0.9, 0.5, 0.1)),
    "binaire"
  )
})

test_that("evaluate_model warning si predicted hors [0,1]", {
  expect_warning(
    evaluate_model(c(1, 0, 1), c(1.5, -0.1, 0.8)),
    "hors de"
  )
})

test_that("evaluate_model retourne objet ROC", {
  result <- evaluate_model(
    c(1, 0, 1, 0),
    c(0.8, 0.2, 0.7, 0.3)
  )
  expect_true(inherits(result$roc, "roc"))
})

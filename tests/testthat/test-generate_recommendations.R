test_that("recommandations si pression elevee", {
  recs <- generate_recommendations(
    carrying_capacity = 0.8,
    grazing_pressure  = 1.5
  )
  expect_true(any(grepl("Reduire", recs)))
  expect_true(any(grepl("rotation", recs)))
})

test_that("recommandations si capacite faible", {
  recs <- generate_recommendations(
    carrying_capacity = 0.3,
    grazing_pressure  = 0.5
  )
  expect_true(any(grepl("Restaurer", recs)))
})

test_that("recommandations si zones refuges fournies", {
  recs <- generate_recommendations(
    carrying_capacity = 1.5,
    grazing_pressure  = 0.8,
    refuge_area       = c(TRUE, FALSE, TRUE)
  )
  expect_true(any(grepl("refuges", recs)))
})

test_that("message favorable si tout est OK", {
  recs <- generate_recommendations(
    carrying_capacity = 2.0,
    grazing_pressure  = 0.5
  )
  expect_true(any(grepl("durable", recs)))
})

test_that("pas de doublons dans les recommandations", {
  recs <- generate_recommendations(
    carrying_capacity = 0.3,
    grazing_pressure  = 1.8
  )
  expect_equal(length(recs), length(unique(recs)))
})

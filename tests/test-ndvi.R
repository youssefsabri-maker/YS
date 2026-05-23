test_that("calc_ndvi retourne des valeurs entre -1 et 1", {
  result <- calc_ndvi(nir = 0.8, red = 0.2)
  expect_true(result >= -1 && result <= 1)
})

test_that("calc_ndvi donne 0 quand NIR = Rouge", {
  expect_equal(calc_ndvi(nir = 0.5, red = 0.5), 0)
})

test_that("calc_ndvi retourne NA quand NIR + Rouge = 0", {
  expect_warning(result <- calc_ndvi(nir = 0, red = 0))
  expect_true(is.na(result))
})

test_that("calc_ndvi fonctionne sur des vecteurs", {
  nir <- c(0.8, 0.3, 0.6)
  red <- c(0.1, 0.2, 0.1)
  result <- calc_ndvi(nir, red)
  expect_length(result, 3)
  expect_true(all(result >= -1 & result <= 1, na.rm = TRUE))
})

test_that("classifier_ndvi retourne le bon nombre de classes", {
  vals <- c(-0.1, 0.1, 0.3, 0.5, 0.75)
  classes <- classifier_ndvi(vals)
  expect_s3_class(classes, "factor")
  expect_length(levels(classes), 5)
})

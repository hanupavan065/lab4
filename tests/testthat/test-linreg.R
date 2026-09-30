test_that("linreg gives the same coefficients as lm", {
  model <- linreg(Petal.Length ~ Species, data = iris)
  reference <- lm(Petal.Length ~ Species, data = iris)

  expect_equal(
    coef(model),
    coef(reference),
    tolerance = 1e-8
  )
})

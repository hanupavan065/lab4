
test_that("ridgereg fits a model and returns coefficients", {
  fit <- ridgereg(mpg ~ wt + hp, data = mtcars, lambda = 1)

  expect_s3_class(fit, "ridgereg")
  expect_named(coef(fit), c("(Intercept)", "wt", "hp"))
  expect_equal(length(coef(fit)), 3)
  expect_equal(length(predict(fit)), nrow(mtcars))
})

test_that("ridgereg predicts on new data", {
  fit <- ridgereg(mpg ~ wt + hp, data = mtcars, lambda = 1)

  predictions <- predict(fit, newdata = mtcars[1:5, ])

  expect_length(predictions, 5)
  expect_equal(
    predictions,
    predict(fit)[1:5],
    tolerance = 1e-8
  )
})

test_that("ridgereg rejects invalid lambda values", {
  expect_error(
    ridgereg(mpg ~ wt + hp, mtcars, lambda = -1)
  )

  expect_error(
    ridgereg(mpg ~ wt + hp, mtcars, lambda = NA_real_)
  )
})

test_that("airport delay visualization returns a plot", {
  p <- visualize_airport_delays()

  expect_s3_class(p, "ggplot")
  expect_true(nrow(p$data) > 0)
  expect_true(all(c("lon", "lat", "mean_delay") %in% names(p$data)))
})

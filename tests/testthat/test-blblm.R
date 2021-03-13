test_that("Check various aspects of the fit function",  {
  library(furrr)
  suppressWarnings(plan(multiprocess, workers = 6))
  options(future.rng.onMisuse = "ignore")
  set.seed(200)
  fit = blblm(mpg ~ hp * wt, data = mtcars, m = 10, B = 1000, par = FALSE)
  #check all the classes
  expect_identical(class(fit), "blblm")
  expect_identical(class(coef(fit)), "numeric")
  #check the parallel versions
})
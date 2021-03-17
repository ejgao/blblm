test_that("Check features of blblm",  {
  library(furrr)
  suppressWarnings(plan(multiprocess, workers = 6))
  options(future.rng.onMisuse = "ignore")
  set.seed(200)
  fit = blblm(mpg ~ hp * wt, data = mtcars, m = 3, B = 100, par = F)
  #check fit
  expect_identical(class(fit), "blblm")
  expect_identical(length(fit), 2L)
  #check sigma
  expect_identical(class(sigma(fit)), "numeric")
  expect_identical(length(sigma(fit)), 1L)
  expect_identical(sigma(fit), sigma(fit, par = T))
  #check coef
  expect_identical(class(coef(fit)), "numeric")
  expect_identical(length(coef(fit)), 4L)
  expect_identical(coef(fit), coef(fit, par = T))
  #check confint
  expect_identical(class(confint(fit)), c("matrix", "array"))
  expect_identical(length(confint(fit)), 6L)
  expect_identical(confint(fit), confint(fit, par = T))
  #check predict
  new_data = data.frame(wt=c(2.4, 2), hp=c(130, 140))
  expect_identical(class(predict(fit, new_data)), "numeric")
  expect_identical(length(predict(fit, new_data)), 2L)
  expect_identical(length(predict(fit, new_data, confidence = T)), 6L)
  expect_identical(predict(fit, new_data, par = F), predict(fit, new_data, par = T))
})

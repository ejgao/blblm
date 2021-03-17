test_that("Aspects of Logistic Regression with Blblog", {
  library(furrr)
  suppressWarnings(plan(multiprocess, workers = 6))
  options(future.rng.onMisuse = "ignore")
  set.seed(200)
  fit = blblog(Species ~ Sepal.Length, data = iris, m = 2, B = 2)
  #check fit
  expect_identical(class(fit), "blblog")
  expect_identical(length(fit), 2L)
  #check coef
  expect_identical(class(coef(fit)), "numeric")
  expect_identical(length(coef(fit)), 2L)
  expect_identical(coef(fit), coef(fit, par = T))
  #check confint
  expect_identical(class(confint(fit)), c("matrix", "array"))
  expect_identical(length(confint(fit)), 2L)
  expect_identical(confint(fit), confint(fit, par = T))
  #check predict, type = response
  newiris=iris[1:100, ]
  newfit = blblog(Species ~ Sepal.Length, data = newiris, m = 2, B=2)
  newdata = data.frame(Sepal.Length=c(5.1))
  predict_fit_par = predict(newfit, new_data = newdata, type = "response", par = T)
  predict_fit = predict(newfit, new_data = newdata, type = "response")
  expect_identical(class(predict_fit_par), class(predict_fit))
  expect_identical(length(predict_fit_par), 1L)
  expect_identical(class(predict_fit_par), "numeric")
  #check predict, type = link
  predict_fit_par = predict(newfit, new_data = newdata, type = "link", par = T)
  predict_fit = predict(newfit, new_data = newdata, type = "link")
  expect_identical(class(predict_fit_par), class(predict_fit))
  expect_identical(length(predict_fit_par), 1L)
  expect_identical(class(predict_fit_par), "numeric")

})

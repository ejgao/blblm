#' @export
blblog <- function(formula, data, m = 10, B = 5000, par = FALSE) {
  data_list <- split_data(data, m)
  if(par == FALSE){
    estimates <- map(
      data_list,
      ~ glm_each_subsample(formula = formula, data = ., n = nrow(data), B = B))
    res <- list(estimates = estimates, formula = formula)
    class(res) <- "blblog"
    invisible(res)
  }
  else{
    estimates <- future_map(
      data_list,
      ~ glm_each_subsample(formula = formula, data = ., n = nrow(data), B = B))
    res <- list(estimates = estimates, formula = formula)
    class(res) <- "blblog"
    invisible(res)
  }
}


#' split data into m parts of approximated equal sizes
split_data <- function(data, m) {
  idx <- sample.int(m, nrow(data), replace = TRUE)
  data %>% split(idx)
}


#' compute the estimates
glm_each_subsample <- function(formula, data, n, B){
  # drop the original closure of formula,
  # otherwise the formula will pick a wrong variable from the global scope.
  environment(formula) <- environment()
  m <- model.frame(formula, data)
  X <- model.matrix(formula, m)
  y <- model.response(m)
  replicate(B, glm1(X, y, n), simplify = FALSE)
}


#' compute the regression estimates for a blb dataset
glm1 <- function(X, y, n) {
  freqs <- as.vector(rmultinom(1, n, rep(1, nrow(X))))
  fit <- glm.fit(X, y, freqs, family = binomial())
  list(coef = blbcoef(fit))
}


#' compute the coefficients from fit
blbcoef_log <- function(fit) {
  coef(fit)
}


#' @method print blblog
#' @export
print.blblog<- function(x, ...) {
  cat("blblm model:", capture.output(x$formula))
  cat("\n")
}


#' @export
#' @method coef blblog
coef.blblog <- function(object, par = FALSE, ...) {
  est <- object$estimates
  map_mean(est, ~ map_cbind(., "coef") %>% rowMeans())
}


#' @export
#' @method confint blblog
confint.blblog <- function(object, parm = NULL, level = 0.95, ...) {
  if (is.null(parm)) {
    parm <- attr(terms(object$formula), "term.labels")
  }
  alpha <- 1 - level
  est <- object$estimates
  out <- map_rbind(parm, function(p) {
    map_mean(est, ~ map_dbl(., list("coef", p)) %>% quantile(c(alpha / 2, 1 - alpha / 2)))
  })
  if (is.vector(out)) {
    out <- as.matrix(t(out))
  }
  dimnames(out)[[1]] <- parm
  out
}

#' @export
#' @method predict blblog
predict.blblog <- function(object, new_data, confidence = FALSE, level = 0.95, type = "link", ...) {
  est <- object$estimates
  X <- model.matrix(reformulate(attr(terms(object$formula), "term.labels")), new_data)
  if(type == "link"){
    if (confidence) {
      map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>%
                apply(1, mean_lwr_upr, level = level) %>%
                t())
    } else {
      map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>% rowMeans())
    }
  }
  else{
    if (confidence) {
      link = map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>%
                 apply(1, mean_lwr_upr, level = level) %>%
                 t())
      prob = exp(link)/(1+exp(link))
      prob

    } else {
      link = map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>% rowMeans())
      prob = exp(link)/(1+exp(link))
      prob
    }
  }
}


#predict(mylogit, newdata = newdata1, type = "response") #probability predictions
#predict(mylogit, newdata = newdata2, type = "link”) #returns linear


mean_lwr_upr <- function(x, level = 0.95) {
  alpha <- 1 - level
  c(fit = mean(x), quantile(x, c(alpha / 2, 1 - alpha / 2)) %>% set_names(c("lwr", "upr")))
}

future_map_mean <- function(.x, .f, ...) {
  (future_map(.x, .f, ...) %>% reduce(`+`)) / length(.x)
}

map_mean <- function(.x, .f, ...) {
  (map(.x, .f, ...) %>% reduce(`+`)) / length(.x)
}

map_cbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(cbind)
}

map_rbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(rbind)
}
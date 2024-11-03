# tests/testthat/test-model_to_latex-lm.R
test_that("model_to_latex.lm generates correct LaTeX for lm model", {
  data <- data.frame(y = rnorm(100), x1 = rnorm(100), x2 = rnorm(100))
  model <- lm(y ~ x1 + x2, data = data)

  result <- model_to_latex.lm(model)
  expected <- "\\(Y = \\beta_0 + \\beta_1 X_1 + \\beta_2 X_2 + \\epsilon \\)"
  expect_equal(result, expected)
})


# tests/testthat/test-model_to_latex-glm.R
test_that("model_to_latex.glm generates correct LaTeX for binomial glm", {
  data <- data.frame(y = rbinom(100, 1, 0.5), x1 = rnorm(100), x2 = rnorm(100))
  model <- glm(y ~ x1 + x2, data = data, family = binomial)

  result <- model_to_latex.glm(model)
  expected <- "\\(\\log\\left(\\frac{Y}{1 - Y}\\right) \\sim \\beta_0 + \\beta_1 X_1 + \\beta_2 X_2\\)"
  expect_equal(result, expected)
})

test_that("model_to_latex.glm generates correct LaTeX for poisson glm", {
  data <- data.frame(y = rpois(100, lambda = 10), x1 = rnorm(100), x2 = rnorm(100))
  model <- glm(y ~ x1 + x2, data = data, family = poisson)

  result <- model_to_latex.glm(model)
  expected <- "\\(\\log(Y) \\sim \\beta_0 + \\beta_1 X_1 + \\beta_2 X_2\\)"
  expect_equal(result, expected)
})

test_that("model_to_latex.glm handles unsupported families", {
  data <- data.frame(y = rnorm(100), x1 = rnorm(100), x2 = rnorm(100))
  model <- glm(y ~ x1 + x2, data = data, family = gaussian)

  result <- model_to_latex.glm(model)
  expected <- "\\(\\log(Y) \\sim \\beta_0 + \\beta_1 X_1 + \\beta_2 X_2\\)"
  expect_equal(result, expected)
})

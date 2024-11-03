
#' String to Sentence Case
#'
#' @param text
#'
#' @return text
#' @export
#'
#' @examples
#' sentence_case('asdasdasd')
sentence_case <- function(text) {
  to_sentence_case <- function(str) {
    sentences <- unlist(strsplit(str, "(?<=[.!?])\\s+", perl = TRUE))
    # Capitalize the first letter of each sentence and make the rest lowercase
    sentences <- paste0(
      toupper(substring(sentences, 1, 1)),
      tolower(substring(sentences, 2))
    )
    # Rejoin sentences into a single string
    paste(sentences, collapse = " ")
  }

  # Apply the function to each element if text is a vector
  if (is.character(text)) {
    return(sapply(text, to_sentence_case, USE.NAMES = FALSE))
  } else {
    stop("Input must be a character string or vector.")
  }
}

#' Adding index in LaTeX
#'
#' @param text
#'
#' @return text
#' @export
#'
#' @examples
#' format_subscript('X17')
format_subscript <- function(term) {
  if (grepl("[a-zA-Z]+[0-9]+$", term)) {
    term <- gsub("([a-zA-Z]+)([0-9]+)", "\\1_\\2", term)
  }
  return(term)
}


#' Linear model method to LaTeX
#'
#' @param model
#'
#' @return string
#' @export
#'
#' @examples
model_to_latex.lm <- function(model) {
  formula <- as.formula(model$call$formula)
  terms <- attr(terms(formula), "term.labels")
  response <- all.vars(formula)[1]

  # Convert response and terms to sentence case
  response <- sentence_case(all.vars(formula)[1])
  terms <- sapply(terms, sentence_case, USE.NAMES = FALSE)

  # Add number indices to response and terms
  response <- format_subscript(response)
  terms <- sapply(terms, format_subscript, USE.NAMES = FALSE)

  # Start building the LaTeX formula string
  latex_formula <- paste0("\\(", response, " = \\beta_0")

  for (i in seq_along(terms)) {
    latex_formula <- paste0(latex_formula, " + \\beta_", i, " ", terms[i])
  }

  latex_formula <- paste0(latex_formula, " + \\epsilon \\)")

  return(latex_formula)
}


#' Generalized linear model method to LaTeX
#'
#' @param model
#'
#' @return string
#' @export
#'
#' @examples# Generalized linear model method with family-specific checks
model_to_latex.glm <- function(model) {
  # Extract the formula and terms
  formula <- as.formula(model$call$formula)
  terms <- attr(terms(formula), "term.labels")
  response <- sentence_case(all.vars(formula)[1])
  response <- format_subscript(response)

  terms <- sapply(terms, sentence_case, USE.NAMES = FALSE)
  terms <- sapply(terms, format_subscript, USE.NAMES = FALSE)

  # Extract the family and link function
  family <- model$family$family
  link <- model$family$link

  # Determine LaTeX representation based on family
  if (family == "binomial" || family == "quasibinomial") {
    latex_formula <- paste0("\\(","\\log\\left(\\frac{", response, "}{1 - ", response, "}\\right) \\sim \\beta_0")
  } else if (family == "poisson" || family == "quasipoisson") {
    latex_formula <- paste0("\\(","\\log(", response, ") \\sim \\beta_0")
  } else if(family == "gaussian" || family == "quasi"){
    latex_formula <- paste0("\\(","\\log(", response, ") \\sim \\beta_0")
  }else if(family == "gamma"){
    latex_formula <- paste0("\\(","Y^{-1} \\sim \\beta_0")
  }else if(family == "inverse.gaussian"){
    latex_formula <- paste0("\\(","Y^{-2} \\sim \\beta_0")
  }else {
    # Default to link(response) if no specific format is defined
    latex_formula <- paste0("\\(","\\", link, "(", response, ") = \\beta_0")
  }

  # Add terms to the formula
  for (i in seq_along(terms)) {
    latex_formula <- paste0(latex_formula, " + \\beta_", i, " ", terms[i])
  }

  # Add the error term (if appropriate)
  latex_formula <- paste0(latex_formula, "\\)")

  return(latex_formula)
}

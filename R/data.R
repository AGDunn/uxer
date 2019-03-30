#' Responses to a (fictitious) SUS and TAM questionnaire
#'
#' 20 participants answered standard questions from the SUS and TAM
#' questionnaire instruments.  All the questions are 1--5 scored Likert items.
#
#' @format A tibble of 20 rows and 23 variables.
#' \describe{
#'   \item{id}{ID variable for participants in study.}
#'   \item{ZZZq1 to q10}{responses to SUS questions.}
#'   \item{ZZZq11 to q16}{responses to TAM usefulness subscale.}
#'   \item{ZZZq17 to q22}{responses to TAM ease-of-use subscale.}
#' }
#' @source Fictional data made using fellowr::fake_a_questionnaire()
"small_study"

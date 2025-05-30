#' Simulated climbing data loosely based on Llewellyn et al. (2008)
#'
#' The paper can be located at: https://doi.org/10.1016/j.paid.2008.03.001
#'
#' Illustrative dataset which can be used for testing the `group_by` argument in `qli()`. First, fit the model as `climbing_mod <- lm(self_efficacy ~ grade*climbing_style, data = climbing_data)`. Then extract the QLI estimates for each climbing style separately as `qli(climbing_mod, predictor = "fitted", group_by = "climbing_style")`
#'
#' @format A tibble with 402 rows and 3 variables:
#' \describe{
#'   \item{climbing_style}{Character. Values: Bouldering, Lead Climbing}
#'   \item{grade}{Numeric. Difficulty at which climbers regularly climb. Originally was 1-11 for bouldering, 1-16 for lead climbing. The variable in the dataset is normalised between 1-15 to allow comparisons.}
#'   \item{self_efficacy}{Numeric. Based on The Climbing Self-Efficacy Scale. Range between 1-1000}
#' }
#' @source \url{https://www.sciencedirect.com/science/article/pii/S0191886908000901}
"climbing_data"

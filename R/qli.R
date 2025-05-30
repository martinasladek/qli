#' Quantify heteroscedasticity in linear models using Quantile LOWESS Intervals (QLI)
#'
#' @param mod A linear model object fitted with lm()
#' @param predictor A character vector. Name of the predictor as it appears in the dataset used for fitting `mod`. If predictor is "fitted" (default), fitted values from the `mod` are used instead.
#' @param standardise If TRUE (default) predictor values and model residuals will be standardised.
#' @param group_by Character specified a grouping variable name. If specified, QLI is produced for each group individually, using the residuals from the full model. Default is NULL.
#' @param lower_quant A quantile to use for the lower boundary of the quantile lowess interval. A numeric value between 0 and 1. Default is 0.025 (recommended).
#' @param upper_quant A quantile to use for the upper boundary of the quantile lowess interval. A numeric value between 0 and 1. Default is 0.975 (recommended).
#' @param window_prop Proportion of the total sample size use in each rolling window. A numeric value between 0 and 1. Default is 0.10 (recommended).
#' @param prop_overlap The extent to which rolling windows can overlap. A numeric value between 0 and 1. Default is 0.75 (recommended).
#'
#' @return Object of class 'lm' containing polynomial trends estimating changes in QLI.
#' @export
#'
#' @examples
#' cars_mod <- lm(dist ~ speed, data = cars)
#' qli(mod = cars_mod, predictor = "speed")
qli <- function(
    mod,
    predictor = "fitted",
    standardise = TRUE,
    group_by = NULL,
    lower_quant = .025,
    upper_quant = .975,
    window_prop = 0.10,
    prop_overlap = 0.75){

  # Obtain QLI values for the predictor - run for each group, if specified:

  qli_df_list <- qli_values(
    mod = mod,
    predictor = predictor,
    standardise = standardise,
    group_by = group_by,
    lower_quant = lower_quant,
    upper_quant = upper_quant,
    window_prop = window_prop,
    prop_overlap = prop_overlap
  )

  # Fit the model predicting the width of the interval from polynomial trends.
  # Run for each group, if specified

  lowess_mod_list <-
    purrr::map(
      .x = qli_df_list,
      .f = ~stats::lm(data = .x, lowess_wide ~ x + I(x^2) + I(x^3) + I(x^4))
    )

  # If groups are not specified, return first element of the list (for easier
  # manipulation Otherwise return the list)

  if(is.null(group_by)) {return(lowess_mod_list[[1]])}
  else{return(lowess_mod_list)}
}




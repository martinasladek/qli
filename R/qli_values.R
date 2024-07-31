#' Calculate quantile residual values for a continuous predictor or fitted values
#' in a linear model
#'
#' @param mod A linear model object fitted with lm()
#' @param predictor A character vector. Name of the predictor as it appears in the dataset used for fitting `mod`. If predictor is "fitted" (default), fitted values from the `mod` are used instead.
#' @param standardise If TRUE (default) predictor values and model residuals will be standardised.
#' @param lower_quant A quantile to use for the lower boundary of the quantile lowess interval. A numeric value between 0 and 1. Default is 0.025 (recommended).
#' @param upper_quant A quantile to use for the upper boundary of the quantile lowess interval. A numeric value between 0 and 1. Default is 0.975 (recommended).
#' @param window_prop Proportion of the total sample size use in each rolling window. A numeric value between 0 and 1. Default is 0.10 (recommended).
#' @param prop_overlap The extent to which rolling windows can overlap. A numeric value between 0 and 1. Default is 0.75 (recommended).
#'
#' @return A dataframe containing the following columns: `x` (predictor or fitted values), `q_lower_y_lowess` (residual values for the lower quantile), `q_upper_y_lowess` (residual values for the upper quantile), `lowess_wide` (width of the quantile lowess interval calculated as the difference between the upper and lower quantile values), `predictor_name` (helper column specifying which predictor was used as `x`. Useful for visualisations.)
#' @export
#'
#' @examples
#' cars_mod <- lm(dist ~ speed, data = cars)
#' qli_values(mod = cars_mod, predictor = "speed")
qli_values <- function(mod,
                       predictor = "fitted",
                       standardise = TRUE,
                       lower_quant = .025,
                       upper_quant = .975,
                       window_prop = 0.10,
                       prop_overlap = 0.75
){

  # if predictor is fitted values, extract fitted values from the model,
  # otherwise grab the predictor values from the dataset used for fitting the
  # original linear model

  if(predictor == "fitted"){
    predictor_vals = mod$fitted.values
  } else {
    predictor_vals = mod$model[[predictor]]
  }

  residual_vals = mod$residuals

  # standardise predictor and outcome values

  if(standardise == TRUE){

    predictor_vals = z(predictor_vals)
    residual_vals = z(residual_vals)

  }

  # compute lower quantile values

  q_lower <- quantile_smoother(
    y = residual_vals,
    x = predictor_vals,
    prop_overlap = prop_overlap,
    window_prop = window_prop,
    tau = lower_quant
  )

  # compute upper quantile values

  q_upper <- quantile_smoother(
    y = residual_vals,
    x = predictor_vals,
    prop_overlap = prop_overlap,
    window_prop = window_prop,
    tau = upper_quant
  )

  # Organise values in a dataset and compute interval width
  qli_df <- data.frame(
    x = q_lower$x,
    q_lower_y_lowess = q_lower$y.lowess,
    q_upper_y_lowess = q_upper$y.lowess,
    lowess_wide = q_upper$y.lowess - q_lower$y.lowess,
    predictor_name = predictor
  )

  return(qli_df)
}

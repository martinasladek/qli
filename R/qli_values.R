#' Calculate quantile residual values for a continuous predictor or fitted values
#' in a linear model
#'
#' @param mod A linear model object fitted with lm()
#' @param predictor A character. Name of the predictor as it appears in the dataset used for fitting `mod`. If predictor is "fitted" (default), fitted values from the `mod` are used instead.
#' @param standardise If TRUE (default) predictor values and model residuals will be standardised.
#' @param group_by Character specified a grouping variable name. If specified, QLI is produced for each group individually, using the residuals from the full model. Default is NULL.
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
                       group_by = NULL,
                       lower_quant = .025,
                       upper_quant = .975,
                       window_prop = 0.10,
                       prop_overlap = 0.75
){

  # Create a dataset with original variables and add residuals and fitted values

  mod_df <- mod$model
  mod_df$residuals <- mod$residuals
  mod_df$fitted <- mod$fitted.values

  # Standardise predictor and outcome values

  if(standardise == TRUE){

    mod_df[[predictor]] = z(mod_df[[predictor]])
    mod_df$residuals = z(mod_df$residuals)

  }

  # Transform into a list, so that outcomes for groups specified by group_by
  # can be iterated over. A single item list is also created if groups are not
  # specified - this is so that the code can keep the same form using (purrr)

  if(is.null(group_by)){
    mod_list = list(mod_df)
  } else {
    mod_list <- mod_df |>
      dplyr::group_split(!!rlang::sym(group_by))
  }

  # Apply quantile smoother to generate lower and upper quantile values

  q_lower_list <- purrr::map(
    .x = mod_list,
    .f = ~quantile_smoother(
      y = .x$residuals,
      x = .x[[predictor]],
      prop_overlap = prop_overlap,
      window_prop = window_prop,
      tau = lower_quant
    )
  )

  q_upper_list <- purrr::map(
    .x = mod_list,
    .f = ~quantile_smoother(
      y = .x$residuals,
      x = .x[[predictor]],
      prop_overlap = prop_overlap,
      window_prop = window_prop,
      tau = upper_quant
    )
  )

  # Collate values from lower and upper quantiles into a list of dataframe.
  # This can be then used for fitting the lowess model in the qli() function.

  qli_df_list <- purrr::map2(
    .x = q_lower_list,
    .y = q_upper_list,
    .f = ~construct_qli_df(.x, .y, predictor)
  )

  # if group_by is specified, add names to the dataframes with QLI values

  if(!is.null(group_by)){
    names(qli_df_list) <- sort(unique(mod_df[[group_by]]))
  }

  return(qli_df_list)

}

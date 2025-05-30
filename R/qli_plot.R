#' Quantify heteroscedasticity in linear models using Quantile LOWESS Intervals (QLI)
#'
#' @param mod A linear model object fitted with lm()
#' @param predictor A character. Name of the predictor as it appears in the dataset used for fitting `mod`. If predictor is "fitted" (default), fitted values from the `mod` are used instead.
#' @param standardise If TRUE (default) predictor values and model residuals will be standardised.
#' @param group_by Character specified a grouping variable name. If specified, QLI is produced for each group individually, using the residuals from the full model. Default is NULL.
#' @param plot_args List of arguments to customise the plot. Valid arguments include, `interval_fill_col` for the fill colour of the interval, `interval_outline_col` for the colour of the interval outline, and `point_col` for the colour of the points on the scatter plot. Further customisation can be achieved by adding `ggplot2` layers. Alternatively, run function `qli_values()` instead to produce a dataset with values for plotting.
#' @param lower_quant A quantile to use for the lower boundary of the quantile lowess interval. A numeric value between 0 and 1. Default is 0.025 (recommended).
#' @param upper_quant A quantile to use for the upper boundary of the quantile lowess interval. A numeric value between 0 and 1. Default is 0.975 (recommended).
#' @param window_prop Proportion of the total sample size use in each rolling window. A numeric value between 0 and 1. Default is 0.10 (recommended).
#' @param prop_overlap The extent to which rolling windows can overlap. A numeric value between 0 and 1. Default is 0.75 (recommended).
#'
#' @return A ggplot object showing the plot of residuals vs fitted (or predictor) values with overexposed QLI.
#' @export
#'
#' @examples
#' cars_mod <- lm(dist ~ speed, data = cars)
#' qli_plot(mod = cars_mod, predictor = "speed")
qli_plot <- function(
    mod,
    predictor = "fitted",
    standardise = TRUE,
    group_by = NULL,
    plot_args = list(interval_fill_col = "#1887ac", interval_outline_col = "#1887ac", point_col = "black"),
    lower_quant = .025,
    upper_quant = .975,
    window_prop = 0.10,
    prop_overlap = 0.75
){

  # Obtain quantile values for each group, if specified

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

  # Create plots either for the overall model, or for each group, if specified:

  plot_list <- purrr::map2(
    .x = mod_list,
    .y = qli_df_list,
    .f = ~ggplot2::ggplot() +
      ggplot2::geom_point(ggplot2::aes(x = .x[[predictor]], y = .x$residuals), colour = plot_args$point_col) +
      ggplot2::geom_line(ggplot2::aes(x = .y$x, y = .y$q_upper_y_lowess), colour = plot_args$interval_outline_col) +
      ggplot2::geom_line(ggplot2::aes(x = .y$x, y = .y$q_lower_y_lowess), colour = plot_args$interval_outline_col) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .y$q_lower_y_lowess, ymax = .y$q_upper_y_lowess, x = .y$x, y = NULL),
                           fill = plot_args$interval_fill_col, alpha = 0.25
      ) +
      ggplot2::labs(x = .y[["predictor_name"]][1], y = "residuals") +
      ggplot2::theme_light()
  )

  if(!is.null(group_by)){
    names(plot_list) <- sort(unique(mod_df[[group_by]]))
  }

  return(plot_list)
}




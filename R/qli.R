#' Quantify heteroscedasticity in linear models using Quantile LOWESS Intervals (QLI)
#'
#' @param mod A linear model object fitted with lm()
#' @param predictor A character vector. Name of the predictor as it appears in the dataset used for fitting `mod`. If predictor is "fitted" (default), fitted values from the `mod` are used instead.
#' @param standardise If TRUE (default) predictor values and model residuals will be standardised.
#' @param plot If TRUE, output includes a scatterplot of predictor vs residuals with the Quantile LOWESS Interval. Default is FALSE.
#' @param plot_args List of arguments to customise the plot. Valid arguments include, `interval_fill_col` for the fill colour of the interval, `interval_outline_col` for the colour of the interval outline, and `point_col` for the colour of the points on the scatter plot. Further customisation can be achieved by adding `ggplot2` layers. Alternatively, run function `qli_values()` instead to produce a dataset with values for plotting.
#' @param lower_quant A quantile to use for the lower boundary of the quantile lowess interval. A numeric value between 0 and 1. Default is 0.025 (recommended).
#' @param upper_quant A quantile to use for the upper boundary of the quantile lowess interval. A numeric value between 0 and 1. Default is 0.975 (recommended).
#' @param window_prop Proportion of the total sample size use in each rolling window. A numeric value between 0 and 1. Default is 0.10 (recommended).
#' @param prop_overlap The extent to which rolling windows can overlap. A numeric value between 0 and 1. Default is 0.75 (recommended).
#'
#' @return If plot is TRUE, returns a list with polynomial trends estimating changes in QLI width and a plot. If plot is false, returns only QLI results.
#' @export
#'
#' @examples
#' cars_mod <- lm(dist ~ speed, data = cars)
#' qli(mod = cars_mod, predictor = "speed")
qli <- function(
    mod,
    predictor = "fitted",
    standardise = TRUE,
    plot = FALSE,
    plot_args = list(interval_fill_col = "#1887ac", interval_outline_col = "#1887ac", point_col = "black"),
    lower_quant = .025,
    upper_quant = .975,
    window_prop = 0.10,
    prop_overlap = 0.75
){

  # Obtained quantile values

  qli_df <- qli_values(
    mod = mod,
    predictor = predictor,
    standardise = standardise,
    lower_quant = lower_quant,
    upper_quant = upper_quant,
    window_prop = window_prop,
    prop_overlap = prop_overlap
  )

  # Fit a model predicting interval width from linear and polynomial trends for x

  lowess_mod <- stats::lm(data = qli_df, lowess_wide ~ x + I(x^2) + I(x^3) + I(x^4))

  if(plot == FALSE){return(lowess_mod)}

  else if (plot == TRUE){

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

    qli_plot <- ggplot2::ggplot() +
      ggplot2::geom_point(ggplot2::aes(x = predictor_vals, y = residual_vals), colour = plot_args$point_col) +
      ggplot2::geom_line(ggplot2::aes(x = qli_df$x, y = qli_df$q_upper_y_lowess), colour = plot_args$interval_outline_col) +
      ggplot2::geom_line(ggplot2::aes(x = qli_df$x, y = qli_df$q_lower_y_lowess), colour = plot_args$interval_outline_col) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = qli_df$q_lower_y_lowess, ymax = qli_df$q_upper_y_lowess, x = qli_df$x, y = NULL),
                           fill = plot_args$interval_fill_col, alpha = 0.25
      ) +
      ggplot2::labs(x = qli_df[["predictor_name"]][1], y = "residuals") +
      ggplot2::theme_light()


    return(
      list(
        lowess_mod = lowess_mod,
        qli_plot = qli_plot
      )
    )

  }

}




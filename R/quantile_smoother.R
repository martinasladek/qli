#' Calculate quantile values of y across the values of x
#'
#' @param y A numeric vector representing the outcome
#' @param x A numeric vector representing the predictor
#' @param prop_overlap The extent to which rolling windows can overlap. A numeric value between 0 and 1.
#' @param window_prop  Proportion of the total sample size use in each rolling window. A numeric value between 0 and 1.
#' @param tau Quantile through which to fit the LOWESS smoother. A numeric value between 0 and 1.
#' @param window_alignment Alignment for rolling windows. The default is "center". Other options are "left" and "right".
#' @param window_function Function to apply in each rolling window. The default is `function(x) {stats::quantile(x, tau)}` which obtains the quantile values.
#'
#' @return A list with two elements. x representing the predictor values and y.lowess representing the smoothed quantile LOWESS values
#' @export
#'
#' @examples
#' quantile_smoother(x = cars$speed, y = cars$dist)
quantile_smoother	 <- function(y, x,
                               prop_overlap = 0.75,
                               window_prop = 0.10,
                               tau = .95,
                               window_alignment = c("center"),
                               window_function = function(x) {stats::quantile(x, tau)}
)
{

  sample_size <- length(y)
  window_size <- ceiling(sample_size*window_prop)

  window_distance <- window_size * (1-prop_overlap)


  # creating our new X and Y
  zoo.Y <- zoo::zoo(x = y, order.by = x)
  #zoo.X <- attributes(zoo.Y)$index

  # center align
  new.Y <- zoo::rollapply(zoo.Y,
                          width = window_size,
                          FUN = window_function,
                          by = window_distance,
                          align = "center"
  )

  new.X <- attributes(new.Y)$index
  new.Y <- as.numeric(new.Y)


  # fit lowess model
  new.Y.mod <- stats::lowess(new.Y~new.X)
  new.Y.lowess <- new.Y.mod$y

  return(
    list(
      x = new.X,
      y.lowess = new.Y.lowess
    )
  )
}

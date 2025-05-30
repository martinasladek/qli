#' A helper function for constructing a dataset with QLI values from dataframes
#' containing lower and upper quantile estimates
#'
#' @param q_lower A dataset containing output from quantile smoother for lower quantiles
#' @param q_upper A dataset containing output from quantile smoother for upper quantiles
#' @param predictor A character. Name of the predictor as it appears in the dataset used for fitting `mod`. If predictor is "fitted" (default), fitted values from the `mod` are used instead.
#'
#' @return A dataframe with QLI values which can be used to fit a model predicting the change in the width of the interval.
#' @export
#'
#' @examples
#' \dontrun{
#' construct_qli_df(q_lower, q_upper, "predictor_name")
#'}
construct_qli_df <- function(q_lower, q_upper, predictor){
  data.frame(
    x = q_lower$x,
    q_lower_y_lowess = q_lower$y.lowess,
    q_upper_y_lowess = q_upper$y.lowess,
    lowess_wide = q_upper$y.lowess - q_lower$y.lowess,
    predictor_name = predictor
  )
}

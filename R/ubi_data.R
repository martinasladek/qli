#' Universal Basic Income support data from Nettle et al. (2020)
#'
#' This dataset was obtained from  https://osf.io/vkchq/
#'
#' The full reference for the paper is:
#'
#' Nettle, D., Johnson, E. A., Johnson, M. T., & Saxe, R. (2020, June 2). Why has the COVID-19 pandemic increased support for Universal Basic Income?. https://doi.org/10.31234/osf.io/csr3u
#'
#' Changes to the dataset were made under the Creative Commons License (http://creativecommons.org/licenses/by/4.0/) attributed to the original articla and third party materials.
#'
#' The version of the data was processed to only include variables necessary for reproducing the linear model used for demonstration in https://doi.org/10.31234/osf.io/gn4mr .
#'
#' @format A tibble with 802 rows and 3 variables:
#' \describe{
#'   \item{observed_shift}{Numeric. Originally labelled delta.ubi.}
#'   \item{predicted_shift}{Numeric. Originally labelled predicted.pandemic.delta.}
#'   \item{normal_times_support}{Numeric. Originally labelled ubinormal_1.}
#' }
#' @source \url{https://www.nature.com/articles/s41599-021-00760-7}
"ubi_data"

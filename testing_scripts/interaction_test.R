library(qli)
library(ggplot2)

z <- function(x){
  (x - mean(x, na.rm = TRUE)) / stats::sd(x, na.rm = TRUE)
}



squish <- function(x, new_min = min_bound, new_max = max_bound){
  (x - min(x))/(max(x)-min(x)) * (new_max - new_min) + new_min
}

set.seed(2)

n = 201
b0 = 0
vp_val = 0.45
vp4 <- function(x1, vp_val) (5 / (1+exp(-vp_val*((x1)-1))))/2.5

climb_df <- data.frame(
  boulder_grade = rnorm(n)# |> squish(new_min = 1, new_max = 15)
)

climb_df$lead_grade <-  faux::rnorm_pre(climb_df, r = 0.71, empirical = TRUE)#|> squish(new_min = 1, new_max = 15)

climb_df$boulder_efficacy <-  (0 + 0.2*climb_df$boulder_grade + rnorm(n, sd = 1) )

climb_df$lead_efficacy <- (0.4 + 0.7*climb_df$lead_grade)  + vp4(climb_df$lead_grade, vp_val)*rnorm(n, sd = 1)

climb_df_long <- data.frame(
  climbing_style = rep(c("Bouldering", "Lead climbing"), each = n),
  grade = c(climb_df$boulder_grade, climb_df$lead_grade) |> squish(new_min = 1, new_max = 15),
  self_efficacy = c(climb_df$boulder_efficacy, climb_df$lead_efficacy) |> squish(new_min = 1, new_max = 1000) |> as.integer()
)

ggplot(data = climb_df_long, aes(x = grade, y = self_efficacy)) +
  geom_point() +
  stat_smooth(method = "lm") +
  facet_wrap(~climbing_style)

mod <- lm(self_efficacy ~ grade*climbing_style, data = climb_df_long)



# qli_values --------------------------------------------------------------

qli_values <- function(mod,
                       predictor = "fitted",
                       standardise = TRUE,
                       group_by = NULL,
                       lower_quant = .025,
                       upper_quant = .975,
                       window_prop = 0.10,
                       prop_overlap = 0.75
){

  mod_df <- mod$model
  mod_df$residuals <- mod$residuals
  mod_df$fitted <- mod$fitted.values

  # standardise predictor and outcome values

  if(standardise == TRUE){

    mod_df[[predictor]] = z(mod_df[[predictor]])
    mod_df$residuals = z(mod_df$residuals)

  }

  if(is.null(group_by)){
    mod_list = list(mod_df)
  } else {
  mod_list <- mod_df |>
    dplyr::group_split(!!sym(group_by))
  }

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


  construct_qli_df <- function(q_lower, q_upper, predictor){
    data.frame(
      x = q_lower$x,
      q_lower_y_lowess = q_lower$y.lowess,
      q_upper_y_lowess = q_upper$y.lowess,
      lowess_wide = q_upper$y.lowess - q_lower$y.lowess,
      predictor_name = predictor
    )
  }


  qli_df_list <- purrr::map2(
    .x = q_lower_list,
    .y = q_upper_list,
    .f = ~construct_qli_df(.x, .y, predictor)
  )

  if(!is.null(group_by)){
    names(qli_df_list) <- sort(unique(mod_df[[group_by]]))
  }

  return(qli_df_list)

}




# qli ---------------------------------------------------------------------

lower_quant = .025;
upper_quant = .975;
window_prop = 0.10;
prop_overlap = 0.7;
standardise = FALSE
group_by = "climbing_style"
predictor = "grade"

qli <- function(
    mod,
    predictor = "fitted",
    standardise = TRUE,
    group_by = NULL,
    lower_quant = .025,
    upper_quant = .975,
    window_prop = 0.10,
    prop_overlap = 0.75){

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

  lowess_mod_list <-
    purrr::map(
      .x = qli_df_list,
      .f = ~stats::lm(data = .x, lowess_wide ~ x + I(x^2) + I(x^3) + I(x^4))
    )

  if(length(lowess_mod_list) == 1) {return(lowess_mod_list[[1]])}
  else{return(lowess_mod_list)}
}


# qli_plot ----------------------------------------------------------------

plot_args = list(interval_fill_col = "#1887ac", interval_outline_col = "#1887ac", point_col = "black")


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


  mod_df <- mod$model
  mod_df$residuals <- mod$residuals
  mod_df$fitted <- mod$fitted.values

  # standardise predictor and outcome values

  if(standardise == TRUE){

    mod_df[[predictor]] = z(mod_df[[predictor]])
    mod_df$residuals = z(mod_df$residuals)

  }

  if(is.null(group_by)){
    mod_list = list(mod_df)
  } else {
    mod_list <- mod_df |>
      dplyr::group_split(!!sym(group_by))
  }



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

qli_plot(mod = mod, predictor = "fitted", group_by = "climbing_style", standardise = TRUE)
qli_plot(mod = mod, predictor = "fitted", group_by = "climbing_style", standardise = TRUE)$Bouldering

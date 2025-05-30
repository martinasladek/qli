# Creativity data

creativity_data <- readxl::read_excel("data-raw/creativity_data.xlsx") |>
  dplyr::transmute(
    divergent_thinking = Fluency,
    relaxation = AUCi_RSA_36,
    creativity = as.numeric(CAQ_score)
  ) |>
  tidyr::drop_na()

usethis::use_data(creativity_data)


# Universal Basic Income data

d <- read.csv("data-raw/ubi_data.csv")

# code from https://osf.io/eyq92/
d$delta.ubi=d$ubipandem_1 - d$ubinormal_1

d$delta.shirking=d$shirking_3-d$shirking_2
d$delta.unfairrich=d$unfairrich_3-d$unfairrich_2
d$delta.unfairdesert=d$unfairdesert_3-d$unfairdesert_2
d$delta.irresponsibility=d$irrespnsibility_3-d$irrespnsibility_2
d$delta.value=d$value_3-d$value_2
d$delta.efficiency=d$efficiency_3-d$efficiency_2
d$delta.expense = d$expense_3 - d$expense_2
d$delta.stress = d$stress_3 - d$stress_2
d$delta.cheat = d$cheat_3 - d$cheat_2

#not this:
baseline.unst.model=lm(ubinormal_1~ shirking_2 +
                         unfairrich_2 +
                         unfairdesert_2 +
                         irrespnsibility_2 +
                         value_2 +
                         efficiency_2 +
                         expense_2 +
                         stress_2 +
                         cheat_2, data=d)

predicted.ubi.pandemic =
  summary(baseline.unst.model)$coefficients[2, 1]* mean(d$delta.shirking, na.rm=T) +
  summary(baseline.unst.model)$coefficients[3, 1]* mean(d$delta.unfairrich, na.rm=T) +
  summary(baseline.unst.model)$coefficients[4, 1]* mean(d$delta.unfairdesert, na.rm=T) +
  summary(baseline.unst.model)$coefficients[5, 1]* mean(d$delta.irresponsibility, na.rm=T) + +
  summary(baseline.unst.model)$coefficients[6, 1]* mean(d$delta.value, na.rm=T) +
  summary(baseline.unst.model)$coefficients[7, 1]* mean(d$delta.efficiency, na.rm=T) +
  summary(baseline.unst.model)$coefficients[8, 1]* mean(d$delta.expense, na.rm=T) +
  summary(baseline.unst.model)$coefficients[9, 1]* mean(d$delta.stress, na.rm=T) +
  summary(baseline.unst.model)$coefficients[10, 1]* mean(d$delta.cheat, na.rm=T)
# Compare this to the observed mean shift
mean(d$delta.ubi)
predicted.ubi.pandemic

# Now predict individual-level shift
d$predicted.pandemic.delta=
  summary(baseline.unst.model)$coefficients[2, 1]* d$delta.shirking +
  summary(baseline.unst.model)$coefficients[3, 1]* d$delta.unfairrich +
  summary(baseline.unst.model)$coefficients[4, 1]* d$delta.unfairdesert +
  summary(baseline.unst.model)$coefficients[5, 1]* d$delta.irresponsibility +
  summary(baseline.unst.model)$coefficients[6, 1]* d$delta.value +
  summary(baseline.unst.model)$coefficients[7, 1]* d$delta.efficiency +
  summary(baseline.unst.model)$coefficients[8, 1]* d$delta.expense +
  summary(baseline.unst.model)$coefficients[9, 1]* d$delta.stress +
  summary(baseline.unst.model)$coefficients[10, 1]* d$delta.cheat

ubi_data <- d |>
  dplyr::transmute(
    observed_shift = delta.ubi, # actual shift in UBI support
    expected_shift = predicted.pandemic.delta, # shift predicted based on importance ratings
    normal_times_support = ubinormal_1
  )

usethis::use_data(ubi_data, overwrite = TRUE)



# climbing data

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

climbing_data <- climb_df_long
usethis::use_data(climbing_data, overwrite = TRUE)

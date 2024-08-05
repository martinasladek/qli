creativity_data <- readxl::read_excel("data-raw/creativity_data.xlsx") |>
  dplyr::transmute(
    divergent_thinking = Fluency,
    relaxation = AUCi_RSA_36,
    creativity = as.numeric(CAQ_score)
  ) |>
  tidyr::drop_na()

usethis::use_data(creativity_data)

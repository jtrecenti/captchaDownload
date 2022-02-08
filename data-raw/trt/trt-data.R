# path <- "data-raw/trt/img2/"
# purrr::walk(1:500, ~captcha_download_trt(path))
# classificar <- fs::dir_ls("data-raw/trt/img", regexp = "_", invert = TRUE)
# captcha::classify(classificar, rm_old = TRUE)

tail(readr::read_csv(fs::path_ext_set(path_model, ".log"), show_col_types = FALSE))


path_model <- "/home/jt/Documents/jtrecenti/captcha/data-raw/trt.pt"
modelo <- luz::luz_load(path_model)

purrr::rerun(
  .n = 1,
  captcha_oracle_trt_com_feedback("data-raw/trt/img_oracle/", modelo, 5)
)


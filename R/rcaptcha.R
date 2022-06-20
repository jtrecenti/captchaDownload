# captcha_oracle_magick <- function(path,
#                                   model = NULL,
#                                   n_letter = 4) {
#   res <- captcha::captcha_generate(
#     FALSE,
#     path,
#     n_chars = n_letter
#   )
#   f_captcha <- fs::file_temp("magick", path, ".png")
#   magick::image_write(res$image, f_captcha)
#   label_oraculo <- tolower(res$captcha)
#   if (is.null(model)) {
#     label <- captcha_label(f_captcha)
#   } else {
#     label <- captcha::decrypt(f_captcha, model)
#   }
#   acertou <- label == label_oraculo
#   lab_oracle <- paste0(label, "_", as.character(as.numeric(acertou)))
#   captcha::classify(f_captcha, lab_oracle, rm_old = TRUE)
# }

captcha_access_rcaptcha <- function(path, n_letter) {
  res <- captcha::captcha_generate(FALSE, path, n_chars = n_letter)
  f_captcha <- fs::file_temp("rcaptcha", path, ".png")
  magick::image_write(res$image, f_captcha)
  Sys.sleep(0.1)
  label_oraculo <- tolower(res$captcha)
  list(f_captcha = f_captcha, ans = label_oraculo)
}

captcha_test_rcaptcha <- function(obj, label) {
  !is.na(label) && label == obj$ans
}

captcha_generate_mnist <- function(path,
                                   path_images = "data-raw/mnist/img_individual/",
                                   n_letter = 4) {
  fs::dir_create(path)
  labels <- sample(0:9, n_letter, replace = TRUE)
  label <- paste(labels, collapse = "")
  img <- fs::dir_ls(path_images)[labels+1] %>%
    purrr::map_chr(~sample(fs::dir_ls(.x), 1)) %>%
    magick::image_read() %>%
    magick::image_append()
  f_captcha <- paste0(fs::file_temp("mnist", path), "_", label, ".png")
  magick::image_write(img, f_captcha)
}

captcha_oracle_mnist <- function(path,
                                 model = NULL,
                                 path_images = "data-raw/mnist/img_individual/",
                                 n_letter = 4) {

  labels <- sample(0:9, n_letter, replace = TRUE)
  label_oraculo <- paste(labels, collapse = "")
  img <- fs::dir_ls(path_images)[labels+1] %>%
    purrr::map_chr(~sample(fs::dir_ls(.x), 1)) %>%
    magick::image_read() %>%
    magick::image_append()

  f_captcha <- fs::file_temp("mnist", path, ".png")
  magick::image_write(img, f_captcha)

  if (is.null(model)) {
    label <- captcha_label(f_captcha)
  } else {
    label <- captcha::decrypt(f_captcha, model)
  }

  acertou <- label == label_oraculo
  lab_oracle <- paste0(label, "_", as.character(as.numeric(acertou)))
  captcha::classify(f_captcha, lab_oracle, rm_old = TRUE)

}

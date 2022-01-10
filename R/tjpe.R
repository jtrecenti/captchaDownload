captcha_download_tjpe <- function(path) {
  u_captcha <- "https://srv01.tjpe.jus.br/consultaprocessualunificadaservico/api/captcha"
  f_captcha <- fs::file_temp(tmp_dir = path, ext = ".png", pattern = "tjpe")
  if (!file.exists(f_captcha)) {
    captcha <- httr::GET(u_captcha, httr::config(ssl_verifypeer = FALSE))
    captcha %>%
      httr::content() %>%
      purrr::pluck("image") %>%
      stringr::str_remove("data:image/png;base64,") %>%
      base64enc::base64decode() %>%
      writeBin(f_captcha)
  }
  f_captcha
}


captcha_oracle_tjpe <- function(path, model = NULL) {

  # path <- "data-raw/tjpe"
  # model <- NULL

  ssl <- httr::config(ssl_verifypeer = FALSE)
  id <- "00034728720148171030"
  u_inicial <- "https://srv01.tjpe.jus.br/consultaprocessualunificada/"
  httr::handle_reset(u_inicial)
  r_inicial <- httr::GET(u_inicial, ssl)

  u_captcha <- "https://srv01.tjpe.jus.br/consultaprocessualunificadaservico/api/captcha"
  f_captcha <- fs::file_temp(tmp_dir = path, ext = ".png", pattern = "tjpe")
  captcha <- httr::GET(u_captcha, ssl)
  captcha %>%
    httr::content() %>%
    purrr::pluck("image") %>%
    stringr::str_remove("data:image/png;base64,") %>%
    base64enc::base64decode() %>%
    writeBin(f_captcha)

  if (is.null(model)) {
    label <- captcha_label(f_captcha)
  } else {
    label <- captcha::decrypt(f_captcha, model)
  }

  ## teste aceita varios chutes
  # label_bkp <- label
  # label <- "12345"
  # label <- label_bkp

  u_consulta <- "https://srv01.tjpe.jus.br/consultaprocessualunificadaservico/api/processo"
  r_consulta <- httr::POST(
    u_consulta,
    body = list(npu = id),
    httr::add_headers("captcha" = label),
    encode = "json",
    ssl
  )

  acertou <- r_consulta$status_code == 200
  lab_oracle <- paste0(label, "_", as.character(as.numeric(acertou)))
  captcha::classify(f_captcha, lab_oracle, rm_old = TRUE)

}

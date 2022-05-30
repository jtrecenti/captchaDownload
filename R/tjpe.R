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


captcha_oracle_tjpe <- function(path, model = NULL, try_again = FALSE) {

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

  ntry <- 0
  while (try_again && !acertou && ntry < 10) {
    ntry <- ntry + 1
    usethis::ui_todo("Wrong answer ({label}) ({ntry})!")
    label <- captcha_label(f_captcha)
    u_consulta <- "https://srv01.tjpe.jus.br/consultaprocessualunificadaservico/api/processo"
    r_consulta <- httr::POST(
      u_consulta,
      body = list(npu = id),
      httr::add_headers("captcha" = label),
      encode = "json",
      ssl
    )
    acertou <- r_consulta$status_code == 200
  }

  lab_oracle <- paste0(label, "_", as.character(as.numeric(acertou)))
  captcha::classify(f_captcha, lab_oracle, rm_old = TRUE)

}

captcha_tjpe_test <- function(label) {
  ssl <- httr::config(ssl_verifypeer = FALSE)
  id <- "00034728720148171030"
  u_consulta <- "https://srv01.tjpe.jus.br/consultaprocessualunificadaservico/api/processo"
  r_consulta <- httr::POST(
    u_consulta,
    body = list(npu = id),
    httr::add_headers("captcha" = label),
    encode = "json",
    ssl
  )
  acertou <- r_consulta$status_code == 200
  acertou
}

captcha_oracle_tjpe_com_feedback <- function(path, model = NULL, max_ntry = 10) {

  # path <- "data-raw/trt/"
  # model <- NULL

  fs::dir_create(path)

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


  ntry <- 1

  if (is.null(model)) {
    label <- captcha_label(f_captcha)
  } else {
    label <- captcha_candidates(f_captcha, model, n = max_ntry,
                                cut_value = log(.001))
  }
  # browser()

  acertou <- captcha_tjpe_test(label[1])
  if (acertou) {
    usethis::ui_done("Acertou!!!")
    label <- label[ntry]
  } else {
    max_ntry_model <- min(max_ntry, length(label))
    usethis::ui_info("Temos {max_ntry_model} candidatos...")
  }

  while (!acertou && ntry <= max_ntry_model && !is.null(model)) {
    usethis::ui_info("Errou! O chute foi: {label[ntry]}")
    ntry <- ntry + 1
    acertou <- captcha_tjpe_test(label[ntry])
    if (acertou) {
      usethis::ui_done("Acertou!!!")
      label <- label[ntry]
    }
  }

  # if tried {max_ntry} times and the model still did not find it
  ntry <- 0
  # while (!acertou && ntry < max_ntry) {
  #   ntry <- ntry + 1
  #   label <- captcha_label(f_captcha)
  #   acertou <- captcha_tjpe_test(label)
  # }
  label <- dplyr::last(label)

  lab_oracle <- paste0(label, "_", as.character(as.numeric(acertou)))
  captcha::classify(f_captcha, lab_oracle, rm_old = TRUE)
}


captcha_access_tjpe <- function(path) {
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

  list(f_captcha = f_captcha)
}

captcha_test_tjpe <- function(obj, label) {
  ssl <- httr::config(ssl_verifypeer = FALSE)
  id <- "00034728720148171030"
  u_consulta <- "https://srv01.tjpe.jus.br/consultaprocessualunificadaservico/api/processo"
  r_consulta <- httr::POST(
    u_consulta,
    body = list(npu = id),
    httr::add_headers("captcha" = label),
    encode = "json",
    ssl
  )
  acertou <- r_consulta$status_code == 200
  acertou
}

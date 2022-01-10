captcha_download_trt <- function(path) {
  fs::dir_create(path)
  u_captcha <- "https://pje-consulta.trt3.jus.br/pje-consulta-api/api/captcha?idProcesso=2104879"
  f <- fs::file_temp(tmp_dir = path, ext = ".jpeg", pattern = "trt")
  if (!file.exists(f)) {
    captcha <- httr::GET(u_captcha)
    captcha %>%
      httr::content() %>%
      purrr::pluck("imagem") %>%
      base64enc::base64decode() %>%
      writeBin(f)
  }
  f
}

captcha_oracle_trt <- function(path, model = NULL) {

  # path <- "data-raw/trt/"
  # model <- NULL

  fs::dir_create(path)
  u_captcha <- "https://pje-consulta.trt3.jus.br/pje-consulta-api/api/captcha?idProcesso=2104879"
  f_captcha <- fs::file_temp(tmp_dir = path, ext = ".jpeg", pattern = "trt")

  if (!file.exists(f_captcha)) {
    captcha <- httr::GET(u_captcha)
    captcha %>%
      httr::content() %>%
      purrr::pluck("imagem") %>%
      base64enc::base64decode() %>%
      writeBin(f_captcha)

    token <- captcha %>%
      httr::content() %>%
      purrr::pluck("tokenDesafio")
  }

  u_consulta <- "https://pje-consulta.trt3.jus.br/pje-consulta-api/api/processos/2104879"

  if (is.null(model)) {
    label <- captcha_label(f_captcha)
  } else {
    label <- captcha::decrypt(f_captcha, model)
  }

  r_consulta <- httr::GET(
    u_consulta,
    query = list(tokenDesafio = token, resposta = label),
    httr::add_headers(`X-Grau-Instancia` = 1)
  )

  acertou <- r_consulta %>%
    httr::content() %>%
    purrr::pluck("numero") %>%
    is.null() %>%
    magrittr::not()

  lab_oracle <- paste0(label, "_", as.character(as.numeric(acertou)))
  captcha::classify(f_captcha, lab_oracle, rm_old = TRUE)


}

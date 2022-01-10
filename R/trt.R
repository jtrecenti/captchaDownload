captcha_download_trt <- function(path) {
  fs::dir_create(path)
  u_captcha <- "https://pje.trt1.jus.br/pje-consulta-api/api/captcha?idProcesso=749661"
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

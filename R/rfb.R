captcha_download_rfb <- function(path) {
  u_captcha <- "http://servicos.receita.fazenda.gov.br/Servicos/cnpjreva/captcha/gerarCaptcha.asp"
  captcha_download_generic(u_captcha, nm = "rfb", ext = ".png", path = path)
}

captcha_access_rfb <- function(path) {
  u_sonoro <- "http://servicos.receita.fazenda.gov.br/Servicos/cnpjreva/Cnpjreva_Solicitacao_CS.asp"
  httr::handle_reset(u_sonoro)
  r0 <- httr::GET(u_sonoro)
  u_captcha <- "http://servicos.receita.fazenda.gov.br/Servicos/cnpjreva/captcha/gerarCaptcha.asp"
  f_captcha <- fs::file_temp(tmp_dir = path, ext = ".png", pattern = "rfb")
  r_captcha <- httr::GET(u_captcha, httr::write_disk(f_captcha))
  Sys.sleep(runif(1) + 3)
  list(f_captcha = f_captcha)
}

captcha_test_rfb <- function(obj, label) {
  u_rfb <- "http://servicos.receita.fazenda.gov.br/Servicos/cnpjreva/valida.asp"
  body <- list(
    "origem" = "comprovante",
    "cnpj" = "13.612.840/0001-57",
    "txtTexto_captcha_serpro_gov_br" = label,
    "search_type" = "cnpj"
  )
  r <- httr::POST(u_rfb, body = body, encode = "form")
  acertou <- r |>
    xml2::read_html() |>
    xml2::xml_text() |>
    stringr::str_detect("JURIMETRIA")
  acertou
}

captcha_download_rfb <- function(path) {
  u_captcha <- "http://servicos.receita.fazenda.gov.br/Servicos/cnpjreva/captcha/gerarCaptcha.asp"
  captcha_download_generic(u_captcha, nm = "rfb", ext = ".png", path = path)
}

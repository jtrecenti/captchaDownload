captcha_download_cadesp <- function(path) {
  fs::dir_create(path)
  u0 <- "https://www.cadesp.fazenda.sp.gov.br/(S(ilwiswheqli3rzms145teohc))/Pages/Cadastro/Consultas/ConsultaPublica/ConsultaPublica.aspx"
  u_captcha <- "https://www.cadesp.fazenda.sp.gov.br/(S(ilwiswheqli3rzms145teohc))/imagemDinamica.aspx"
  f <- fs::file_temp(tmp_dir = path, ext = ".jpeg", pattern = "cadesp")
  if (!file.exists(f)) {
    r0 <- httr::GET(u0)
    httr::GET(u_captcha, httr::write_disk(f, TRUE))
  }
  f
}

captcha_oracle_cadesp <- function(path, model = NULL) {

  fs::dir_create(path)

  u_prefix <- "https://www.cadesp.fazenda.sp.gov.br"
  u0 <- stringr::str_glue("{u_prefix}/Pages/Cadastro/Consultas/ConsultaPublica/ConsultaPublica.aspx")

  # download initial page
  r0 <- httr::GET(u0)
  html <- xml2::read_html(r0)
  # download inicial credentials
  vs <- html %>%
    xml2::xml_find_first("//*[@id='__VIEWSTATE']") %>%
    xml2::xml_attr("value")

  ev <- html %>%
    xml2::xml_find_first("//*[@id='__EVENTVALIDATION']") %>%
    xml2::xml_attr("value")

  evg <- html %>%
    xml2::xml_find_first("//*[@id='__VIEWSTATEGENERATOR']") %>%
    xml2::xml_attr("value")

  session_id <- stringr::str_extract(r0$url, "\\([^)]+\\)\\)")

  # download captcha
  u_captcha_suffix <- "imagemDinamica.aspx"
  u_captcha <- stringr::str_glue("{u_prefix}/{session_id}/{u_captcha_suffix}")
  f <- fs::file_temp(tmp_dir = path, ext = ".jpeg", pattern = "cadesp")
  r_captcha <- httr::GET(u_captcha, httr::write_disk(f, TRUE))

  # classify captcha (manually or automatically)
  if (!is.null(model)) {
    label <- captcha::decrypt(f, model)
  } else {
    label <- captcha_label(f)
  }

  # validation request
  u_validacao_suffix <- "Pages/Cadastro/Consultas/ConsultaPublica/ConsultaPublica.aspx"
  u_validacao <- stringr::str_glue("{u_prefix}/{session_id}/{u_validacao_suffix}")

  codigo_controle <- "beb3633a-7d63-4b32-b896-55814dae2da1"
  cnpj <- "47.508.411/0001-56" # CNPJ do GPA

  body_validacao <- list(
    # "ctl00_conteudoPaginaPlaceHolder_filtroTabContainer_ClientState" = '{"ActiveTabIndex":1,"TabState":[true,true]}',
    "__EVENTTARGET" = "",
    "__EVENTARGUMENT" = "",
    "__LASTFOCUS" = "",
    "__VIEWSTATE" = vs,
    "__VIEWSTATEGENERATOR" = evg,
    "__EVENTVALIDATION" = ev,
    "ctl00$conteudoPaginaPlaceHolder$filtroTabContainer$filtroEmitirCertidaoTabPanel$tipoFiltroDropDownList" = "0",
    "ctl00$conteudoPaginaPlaceHolder$filtroTabContainer$filtroEmitirCertidaoTabPanel$valorFiltroTextBox" = "",
    "g-recaptcha-response" = "",
    "ctl00$conteudoPaginaPlaceHolder$filtroTabContainer$filtroConsultarCertidaoTabPanel$nrCnpjConsultarCertidaoTextBox" = cnpj,
    "ctl00$conteudoPaginaPlaceHolder$filtroTabContainer$filtroConsultarCertidaoTabPanel$nrCodigoValidadorTextBox" = codigo_controle,
    "ctl00$conteudoPaginaPlaceHolder$filtroTabContainer$filtroConsultarCertidaoTabPanel$imagemDinamicaConsultarCertidaoTextBox" = label,
    "ctl00$conteudoPaginaPlaceHolder$filtroTabContainer$filtroConsultarCertidaoTabPanel$consultarCertidaoButton" = "Consultar"
  )

  r_validacao <- httr::POST(u_validacao, body = body_validacao, encode = "form")

  ## for debugguing purposes
  # r_validacao <- httr::POST(
  #   u_validacao, body = body_validacao, encode = "form",
  #   httr::write_disk("data-raw/test.html", TRUE)
  # )
  # httr::BROWSE("data-raw/test.html")

  valido <- r_validacao %>%
    xml2::read_html() %>%
    xml2::xml_text() %>%
    stringr::str_detect("foi emitido em")

  lab_oracle <- paste0(label, "_", as.character(as.numeric(valido)))
  captcha::classify(f, lab_oracle, rm_old = TRUE)

}

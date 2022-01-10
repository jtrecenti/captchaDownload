captcha_download_jucesp <- function(path) {
  u <- "https://www.jucesponline.sp.gov.br/Pre_Visualiza.aspx?idproduto=&nire=35222827792"
  r0 <- httr::GET(u)
  guid <- stringr::str_extract(httr::content(r0, "text"), "(?<=guid=)[^\"]+")
  u_captcha <- paste0("https://www.jucesponline.sp.gov.br/CaptchaImage.aspx?guid=", guid)
  f_captcha <- fs::file_temp(
    pattern = "jucesp",
    tmp_dir = path,
    ext = ".jpeg"
  )
  r_captcha <- httr::GET(u_captcha, httr::write_disk(f_captcha, TRUE))
  f_captcha
}


captcha_oracle_jucesp <- function(path, model = NULL) {

  u <- "https://www.jucesponline.sp.gov.br/Pre_Visualiza.aspx?idproduto=&nire=35222827792"
  r0 <- httr::GET(u)
  h <- xml2::read_html(r0)
  vs <- scrapr::html_viewstate(h)
  ev <- scrapr::html_eventval(h)
  guid <- stringr::str_extract(httr::content(r0, "text"), "(?<=guid=)[^\"]+")
  u_captcha <- paste0("https://www.jucesponline.sp.gov.br/CaptchaImage.aspx?guid=", guid)

  f_captcha <- fs::file_temp(
    pattern = "jucesp",
    tmp_dir = path,
    ext = ".jpeg"
  )

  r_captcha <- httr::GET(u_captcha, httr::write_disk(f_captcha, TRUE))

  if (is.null(model)) {
    label <- captcha_label(f_captcha)
  } else {
    label <- captcha::decrypt(f_captcha, model)
  }


  parm <- list(
    "ctl00$ajaxMaster" = "ctl00$cphContent$ajaxForm|ctl00$cphContent$frmPreVisualiza$btEntrar",
    "ctl00$frmLogin$txtLogin" = "",
    "ctl00$frmLogin$tweLogin_ClientState" = "",
    "ctl00$frmLogin$txtSenha" = "",
    "ctl00$frmLogin$tweSenha_ClientState" = "",
    "ctl00$cphContent$frmPreVisualiza$CaptchaControl1" = label,
    "__EVENTTARGET" = "",
    "__EVENTARGUMENT" = "",
    "__VIEWSTATE" = vs,
    "__VIEWSTATEGENERATOR" = "BE1E90E5",
    "__EVENTVALIDATION" = ev,
    "ctl00$cphContent$frmPreVisualiza$btEntrar" = "Continuar"
  )

  r_final <- httr::POST(u, body = parm)

  acertou <- r_final %>%
    xml2::read_html() %>%
    xml2::xml_find_first(".//*[@id='dados']") %>%
    xml2::xml_text() %>%
    stringr::str_squish() %>%
    stringr::str_detect("TERRANOVA CONSULTORIA") %>%
    isTRUE()

  lab_oracle <- paste0(label, "_", as.character(as.numeric(acertou)))
  captcha::classify(f_captcha, lab_oracle, rm_old = TRUE)
}

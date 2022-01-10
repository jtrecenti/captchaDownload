captcha_download_tjes <- function(path) {
  u_captcha <- "http://aplicativos.tjes.jus.br/consultaunificada/faces/javax.faces.resource/captcha?ln=oam.custom.captcha&captchaSessionKeyName=captchaPalavra"
  captcha_download_generic(u_captcha, "tjes", ".png", path)
}

captcha_oracle_tjes <- function(path, model = NULL) {

  # path <- "data-raw/tests/"
  # model <- NULL

  id <- "00086512720208080000"
  base <- "http://aplicativos.tjes.jus.br/consultaunificada/faces/"
  httr::handle_reset(base)
  resp <- httr::GET(paste0(base, "pages/pesquisaSimplificada.xhtml"))

  view_state <- resp %>%
    xml2::read_html() %>%
    xml2::xml_find_first("//*[@id='javax.faces.ViewState']") %>%
    xml2::xml_attr("value")
  form <- list(
    "searchMode" = "1",
    "txtPesquisaSimplificada" = id,
    "btnRealizarPesquisaSimplificada" = "",
    "j_id_4o" = "",
    "j_id_4p" = "0",
    "j_id_4q" = "0",
    "j_id_4r" = "",
    "j_id_4s" = "0",
    "j_id_4t" = "0",
    "j_id_4u" = "",
    "txtTodasPalavras" = "",
    "j_id_5v" = "",
    "j_id_62" = "",
    "j_id_69" = "",
    "j_id_6_SUBMIT" = "1",
    "autoScroll" = "",
    "javax.faces.ViewState" = view_state
  )

  f_captcha <- fs::file_temp(
    pattern = "tjes",
    tmp_dir = path,
    ext = ".png"
  )
  params <- list(
    ln = "oam.custom.captcha",
    captchaSessionKeyName = "captchaPalavra",
    dummyParameter = as.character(round(as.numeric(Sys.time()) * 1000, 0))
  )
  httr::GET(
    paste0(base, "javax.faces.resource/captcha"),
    query = params, httr::write_disk(f_captcha, TRUE)
  )

  if (is.null(model)) {
    label <- captcha_label(f_captcha)
  } else {
    label <- captcha::decrypt(f_captcha, model)
  }

  ## teste aceita varios chutes
  # label_bkp <- label
  # label <- "12345"
  # label <- label_bkp


  # 0 is an extra label
  form$campoCaptcha <- stringr::str_remove(label, "0$")

  resp <- httr::POST(
    paste0(base, "pages/pesquisaSimplificada.xhtml"),
    body = form,
    encode = "form"
  )

  tem_captcha <- resp %>%
    xml2::read_html() %>%
    xml2::xml_find_all("//*/img") %>%
    xml2::xml_attr("src") %>%
    stringr::str_detect("captcha") %>%
    base::any()

  acertou <- !tem_captcha

  lab_oracle <- paste0(label, "_", as.character(as.numeric(acertou)))
  captcha::classify(f_captcha, lab_oracle, rm_old = TRUE)

}


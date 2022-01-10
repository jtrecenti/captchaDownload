captcha_download_trf5 <- function(path) {
  # fs::dir_create(path)
  # u_captcha <- "https://pje.trf5.jus.br/pjeconsulta/seam/resource/captcha"
  # f <- fs::file_temp(tmp_dir = path, ext = ".jpeg", pattern = "trf5")
  # if (!file.exists(f)) {
  #   httr::GET(u_captcha, httr::write_disk(f, TRUE))
  # }
  # f

  u_captcha <- "https://pje.trf5.jus.br/pjeconsulta/seam/resource/captcha"
  captcha_download_generic(u_captcha, nm = "trf5", path = path)
}

captcha_oracle_trf5 <- function(path, model = NULL) {
  u <- "https://pje.trf5.jus.br/pjeconsulta/ConsultaPublica/listView.seam"
  r0 <- httr::GET(u)
  j_id <- r0 %>%
    xml2::read_html() %>%
    xml2::xml_find_first("//*[@id='javax.faces.ViewState']") %>%
    xml2::xml_attr("value")

  f_captcha <- fs::file_temp(tmp_dir = path, ext = ".jpeg", pattern = "trf5")
  captcha <- httr::GET(
    "https://pje.trf5.jus.br/pjeconsulta/seam/resource/captcha",
    httr::write_disk(f_captcha, TRUE)
  )

  # classify captcha (manually or automatically)
  if (!is.null(model)) {
    label <- captcha::decrypt(f_captcha, model)
  } else {
    label <- captcha_label(f_captcha)
  }

  body <- list(
    "AJAXREQUEST" = "_viewRoot",
    "consultaPublicaForm:Processo:jurisdicaoSecaoDecoration:jurisdicaoSecao" = "org.jboss.seam.ui.NoSelectionConverter.noSelectionValue",
    "consultaPublicaForm:Processo:ProcessoDecoration:Processo" = "0003081-44.2013.4.05.8400",
    "consultaPublicaForm:Processo:j_id119:numeroProcessoPesqsuisaOriginario" = "",
    "consultaPublicaForm:nomeParte:nomeParteDecoration:nomeParte" = "",
    "consultaPublicaForm:nomeParteAdvogado:nomeParteAdvogadoDecoration:nomeParteAdvogadoDecoration:nomeParteAdvogado" = "",
    "consultaPublicaForm:classeJudicial:idDecorateclasseJudicial:classeJudicial" = "",
    "consultaPublicaForm:classeJudicial:idDecorateclasseJudicial:j_id207_selection" = "",
    "consultaPublicaForm:numeroCPFCNPJ:numeroCPFCNPJRadioCPFCNPJ:numeroCPFCNPJCNPJ" = "",
    "consultaPublicaForm:numeroOABParte:numeroOABParteDecoration:numeroOABParteEstadoCombo" = "org.jboss.seam.ui.NoSelectionConverter.noSelectionValue",
    "consultaPublicaForm:numeroOABParte:numeroOABParteDecoration:numeroOABParte" = "",
    "consultaPublicaForm:numeroOABParte:numeroOABParteDecoration:j_id258" = "",
    "consultaPublicaForm:captcha:j_id268:verifyCaptcha" = label,
    "consultaPublicaForm" = "consultaPublicaForm",
    "autoScroll" = "",
    "javax.faces.ViewState" = j_id,
    "consultaPublicaForm:pesq" = "consultaPublicaForm:pesq"
  )

  r <- httr::POST(u, body = body, encode = "form")
  acertou <- r %>%
    xml2::read_html() %>%
    xml2::xml_find_first("//span[contains(@class, 'error')]") %>%
    xml2::xml_text() %>%
    is.na()

  lab_oracle <- paste0(label, "_", as.character(as.numeric(acertou)))
  captcha::classify(f_captcha, lab_oracle, rm_old = TRUE)
}

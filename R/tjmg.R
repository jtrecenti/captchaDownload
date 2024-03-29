captcha_download_tjmg <- function(path) {
  u_captcha <- "http://www4.tjmg.jus.br/juridico/sf/captcha.svl"
  captcha_download_generic(u_captcha, nm = "tjmg", ext = ".jpeg", path = path)
}

# captcha_oracle_tjmg <- function(path, model = NULL) {
#
#   # path <- "data-raw/tests/"
#   # model <- NULL
#
#   fs::dir_create(path)
#   # case to be tested
#   id <- " 50018808320168130699"
#   u <- "https://www4.tjmg.jus.br/juridico/sf/proc_resultado.jsp?"
#   httr::handle_reset(u)
#   q <- list(
#     comrCodigo = stringr::str_sub(id, -4, -1),
#     numero = "1",
#     listaProcessos = id,
#     btn_pesquisar = "Pesquisar"
#   )
#   r0 <- httr::GET(u, query = q)
#   captcha <- tjmg_has_captcha(r0)
#
#   f_captcha <- fs::file_temp(tmp_dir = path, ext = ".jpeg", pattern = "tjmg")
#   httr::GET(
#     paste0("https://www4.tjmg.jus.br/juridico/sf/", captcha),
#     httr::write_disk(f_captcha, overwrite = TRUE)
#   )
#
#   # classify captcha (manually or automatically)
#   if (!is.null(model)) {
#     label <- captcha::decrypt(f_captcha, model)
#   } else {
#     label <- captcha_label(f_captcha)
#   }
#
#   ## teste aceita varios chutes
#   # label_bkp <- label
#   # label <- "123422"
#   # label <- label_bkp
#
#
#   payload <- list(
#     "callCount" = "1",
#     "nextReverseAjaxIndex" = "0",
#     "c0-scriptName" = "ValidacaoCaptchaAction",
#     "c0-methodName" = "isCaptchaValid",
#     "c0-id" = "0",
#     "c0-param0" = paste0("string:", label),
#     "batchId" = "0",
#     "instanceId" = "0",
#     "page" = "",
#     "scriptSessionId" = ""
#   )
#   u_captcha <- "https://www4.tjmg.jus.br/juridico/sf/dwr/call/plaincall/ValidacaoCaptchaAction.isCaptchaValid.dwr"
#   r_captcha <- httr::POST(u_captcha, body = payload, encode = "form")
#
#   acertou <- httr::content(r_captcha, "text", encoding = "UTF-8") |>
#     stringr::str_detect(",true\\)")
#
#   lab_oracle <- paste0(label, "_", as.character(as.numeric(acertou)))
#   captcha::classify(f_captcha, lab_oracle, rm_old = TRUE)
#
# }

# tjmg_has_captcha <- function(r) {
#   r |>
#     httr::content("text", encoding = "latin1") |>
#     xml2::read_html() |>
#     xml2::xml_find_first("//*[@id='captcha_image']") |>
#     xml2::xml_attr("src")
# }

captcha_access_tjmg <- function(path) {
  id <- "00666632620118130480"
  u <- "https://www4.tjmg.jus.br/juridico/sf/proc_resultado.jsp?"
  ssl <- httr::config(ssl_verifypeer = FALSE)
  f_captcha <- fs::file_temp(tmp_dir = path, ext = ".jpeg", pattern = "tjmg")
  httr::handle_reset(u)
  q <- list(
    comrCodigo = stringr::str_sub(id, -4, -1),
    numero = "1",
    listaProcessos = id,
    btn_pesquisar = "Pesquisar"
  )
  r0 <- httr::GET(u, query = q, ssl)

  captcha <- r0 |>
    httr::content("text", encoding = "latin1") |>
    xml2::read_html() |>
    xml2::xml_find_first("//*[@id='captcha_image']") |>
    xml2::xml_attr("src")

  if (is.na(captcha)) {
    usethis::ui_stop("Captcha não esta disponível")
  }
  # TODO: ainda não deu para testar se o captcha ficará disponível

  httr::GET(
    paste0("https://www4.tjmg.jus.br/juridico/sf/", captcha),
    httr::write_disk(f_captcha, overwrite = TRUE),
    ssl
  )

  list(f_captcha = f_captcha)

}

captcha_test_tjmg <- function(obj, label) {

  ssl <- httr::config(ssl_verifypeer = FALSE)
  payload <- list(
    "callCount" = "1",
    "nextReverseAjaxIndex" = "0",
    "c0-scriptName" = "ValidacaoCaptchaAction",
    "c0-methodName" = "isCaptchaValid",
    "c0-id" = "0",
    "c0-param0" = paste0("string:", label),
    "batchId" = "0",
    "instanceId" = "0",
    "page" = "",
    "scriptSessionId" = ""
  )
  u_captcha <- "https://www4.tjmg.jus.br/juridico/sf/dwr/call/plaincall/ValidacaoCaptchaAction.isCaptchaValid.dwr"
  r_captcha <- httr::POST(u_captcha, body = payload, encode = "form", ssl)

  acertou <- httr::content(r_captcha, "text", encoding = "UTF-8") |>
    stringr::str_detect(",true\\)")

  acertou

}

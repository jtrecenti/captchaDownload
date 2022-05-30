captcha_download_esaj <- function(path) {
  u_captcha <- "http://esaj.tjba.jus.br/cpopg/imagemCaptcha.do"
  captcha_download_generic(u_captcha, nm = "esaj", ext = ".png", path = path)
}

# captcha_oracle_esaj <- function(path, model = NULL) {
#
#   # path <- "data-raw/tests/"
#   # model <- NULL
#
#   fs::dir_create(path)
#
#   f_captcha <- fs::file_temp(pattern = "esaj", tmp_dir = path, ext = ".png")
#
#   u_search <- "http://esaj.tjba.jus.br/cpopg/search.do"
#   httr::handle_reset(u_search)
#
#   id <- "05042786720178050004"
#   ts <- stringr::str_replace_all(lubridate::now("Brazil/East"), "[^0-9]", "")
#   httr::POST(
#     "http://esaj.tjba.jus.br/cpopg/imagemCaptcha.do",
#     body = list(timestamp = ts, uuidCaptcha = "", conversationId = ""),
#     config = httr::config(ssl_verifypeer = FALSE),
#     httr::write_disk(f_captcha, TRUE)
#   )
#
#   if (is.null(model)) {
#     label <- captcha_label(f_captcha)
#   } else {
#     label <- captcha::decrypt(f_captcha, model)
#   }
#
#   ## teste aceita varios chutes
#   # label_bkp <- label
#   # label <- "12345"
#   # label <- label_bkp
#
#   query <- list(
#     "dadosConsulta.localPesquisa.cdLocal" = "-1",
#     "cbPesquisa" = "NUMPROC",
#     "dadosConsulta.tipoNuProcesso" = "UNIFICADO",
#     "numeroDigitoAnoUnificado" = stringr::str_sub(id, 1, 13),
#     "foroNumeroUnificado" = stringr::str_sub(id, -4, -1),
#     "dadosConsulta.valorConsultaNuUnificado" = id,
#     "dadosConsulta.valorConsulta" = "",
#     "vlCaptcha" = label
#   )
#
#   result <- httr::GET(
#     u_search,
#     query = query,
#     httr::config(ssl_verifypeer = FALSE)
#   )
#
#   acertou <- result %>%
#     xml2::read_html() %>%
#     xml2::xml_find_all("//*[@id='tablePartesPrincipais']") %>%
#     length() %>%
#     magrittr::is_greater_than(0)
#
#   lab_oracle <- paste0(label, "_", as.character(as.numeric(acertou)))
#   captcha::classify(f_captcha, lab_oracle, rm_old = TRUE)
#
# }
#
# captcha_esaj_test <- function(label) {
#
#   id <- "05042786720178050004"
#   u_search <- "http://esaj.tjba.jus.br/cpopg/search.do"
#
#   query <- list(
#     "dadosConsulta.localPesquisa.cdLocal" = "-1",
#     "cbPesquisa" = "NUMPROC",
#     "dadosConsulta.tipoNuProcesso" = "UNIFICADO",
#     "numeroDigitoAnoUnificado" = stringr::str_sub(id, 1, 13),
#     "foroNumeroUnificado" = stringr::str_sub(id, -4, -1),
#     "dadosConsulta.valorConsultaNuUnificado" = id,
#     "dadosConsulta.valorConsulta" = "",
#     "vlCaptcha" = label
#   )
#
#   result <- httr::GET(
#     u_search,
#     query = query,
#     httr::config(ssl_verifypeer = FALSE)
#   )
#
#   acertou <- result %>%
#     xml2::read_html() %>%
#     xml2::xml_find_all("//*[@id='tablePartesPrincipais']") %>%
#     length() %>%
#     magrittr::is_greater_than(0)
#
#   acertou
# }
#
#
# captcha_oracle_esaj_com_feedback <- function(path, model = NULL, max_ntry = 10, manual = TRUE) {
#
#   # path <- "data-raw/trt/"
#   # model <- NULL
#
#   fs::dir_create(path)
#
#   f_captcha <- fs::file_temp(pattern = "esaj", tmp_dir = path, ext = ".png")
#
#   u_search <- "http://esaj.tjba.jus.br/cpopg/search.do"
#   httr::handle_reset(u_search)
#
#   id <- "05042786720178050004"
#   ts <- stringr::str_replace_all(lubridate::now("Brazil/East"), "[^0-9]", "")
#   httr::POST(
#     "http://esaj.tjba.jus.br/cpopg/imagemCaptcha.do",
#     body = list(timestamp = ts, uuidCaptcha = "", conversationId = ""),
#     config = httr::config(ssl_verifypeer = FALSE),
#     httr::write_disk(f_captcha, TRUE)
#   )
#
#   ntry <- 1
#
#   if (is.null(model)) {
#     label <- captcha_label(f_captcha)
#   } else {
#     label <- captcha_candidates(f_captcha, model, n = max_ntry)
#   }
#   # browser()
#
#   f_log <- paste0(
#     dirname(path),
#     "/logs/",
#     fs::path_ext_set(basename(f_captcha), ".log")
#   )
#   fs::dir_create(dirname(f_log))
#
#   acertou <- captcha_esaj_test(label[1])
#   da_log <- tibble::tibble(
#     ntry = ntry,
#     label = label[ntry],
#     type = "auto",
#     result = acertou
#   )
#   if (acertou) {
#     usethis::ui_done("Acertou!!!")
#     label <- label[ntry]
#   } else {
#     max_ntry_model <- min(max_ntry, length(label))
#     usethis::ui_info("Temos {max_ntry_model} candidatos...")
#   }
#
#   while (!acertou && ntry < max_ntry_model && !is.null(model)) {
#     usethis::ui_info("Errou! O chute foi: {label[ntry]}")
#     ntry <- ntry + 1
#     acertou <- captcha_esaj_test(label[ntry])
#     da_log <- tibble::add_row(
#       da_log,
#       ntry = ntry,
#       label = label[ntry],
#       type = "auto",
#       result = acertou
#     )
#     if (acertou) {
#       usethis::ui_done("Acertou!!!")
#       label <- label[ntry]
#     }
#   }
#
#   if (!acertou && !manual) {
#     label <- label[ntry]
#   }
#
#   # if tried {max_ntry} times and the model still did not find it
#   ntry <- 0
#   while (!acertou && ntry < max_ntry && manual) {
#     ntry <- ntry + 1
#     label <- captcha_label(f_captcha)
#     acertou <- captcha_esaj_test(label)
#     da_log <- tibble::add_row(
#       da_log,
#       label = label,
#       type = "manual",
#       result = acertou
#     )
#   }
#
#   lab_oracle <- paste0(label, "_", as.character(as.numeric(acertou)))
#   captcha::classify(f_captcha, lab_oracle, rm_old = TRUE)
#   readr::write_csv(da_log, f_log)
#
# }

captcha_access_esaj <- function(path) {
  f_captcha <- fs::file_temp(pattern = "esaj", tmp_dir = path, ext = ".png")
  u_search <- "http://esaj.tjba.jus.br/cpopg/search.do"
  httr::handle_reset(u_search)
  id <- "05042786720178050004"
  ts <- stringr::str_replace_all(lubridate::now("Brazil/East"), "[^0-9]", "")
  httr::POST(
    "http://esaj.tjba.jus.br/cpopg/imagemCaptcha.do",
    body = list(timestamp = ts, uuidCaptcha = "", conversationId = ""),
    config = httr::config(ssl_verifypeer = FALSE),
    httr::write_disk(f_captcha, TRUE)
  )
  list(f_captcha = f_captcha)
}

captcha_test_esaj <- function(obj, label) {
  id <- "05042786720178050004"
  u_search <- "http://esaj.tjba.jus.br/cpopg/search.do"

  query <- list(
    "dadosConsulta.localPesquisa.cdLocal" = "-1",
    "cbPesquisa" = "NUMPROC",
    "dadosConsulta.tipoNuProcesso" = "UNIFICADO",
    "numeroDigitoAnoUnificado" = stringr::str_sub(id, 1, 13),
    "foroNumeroUnificado" = stringr::str_sub(id, -4, -1),
    "dadosConsulta.valorConsultaNuUnificado" = id,
    "dadosConsulta.valorConsulta" = "",
    "vlCaptcha" = label
  )

  result <- httr::GET(
    u_search,
    query = query,
    httr::config(ssl_verifypeer = FALSE)
  )

  acertou <- result %>%
    xml2::read_html() %>%
    xml2::xml_find_all("//*[@id='tablePartesPrincipais']") %>%
    length() %>%
    magrittr::is_greater_than(0)

  acertou
}


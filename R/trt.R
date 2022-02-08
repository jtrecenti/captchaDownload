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

captcha_trt_test <- function(u_consulta, token, label) {
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
  acertou
}

captcha_oracle_trt_com_feedback <- function(path, model = NULL, max_ntry = 10) {

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

  acertou <- captcha_trt_test(u_consulta, token, label)
  ntry <- 0
  historico_chutes <- character(0)

  while (!acertou && ntry < max_ntry && !is.null(model)) {
    ntry <- ntry + 1
    historico_chutes <- c(historico_chutes, label)
    usethis::ui_info("Errou! O chute foi: {label}")

    # calcula os logits
    model$model$eval()
    transformed <- model$model$transform(f_captcha)
    logits <- as.matrix(model$model(transformed)[1,..])

    # heuristics to update logits
    while (label %in% historico_chutes) {
      # pega a ordem dos captchas. O chute é a primeira linha
      logit_order <- apply(logits, 1, order, decreasing = TRUE)
      first_logit <- logits[logit_order[1,]]
      second_logit <- logits[logit_order[2,]]
      # metodo da menor distancia entre primeiro e segundo chutes
      to_change <- which.min(first_logit - second_logit)
      # modifica os logits
      logits[to_change, logit_order[1,to_change]] <- -1e3
      ind <- apply(logits, 1, which.max)
      label <- paste(model$model$vocab[ind], collapse = "")
    }
    acertou <- captcha_trt_test(u_consulta, token, label)
    if (acertou) usethis::ui_done("Acertou!!!")
  }


  # if tried {max_ntry} times and the model still did not find it
  ntry <- 0
  while (!acertou && ntry < max_ntry) {
    usethis::ui_info("Errou! O chute foi: {label}")
    ntry <- ntry + 1
    label <- captcha_label(f_captcha)
    acertou <- captcha_trt_test(u_consulta, token, label)
  }

  lab_oracle <- paste0(label, "_", as.character(as.numeric(acertou)))
  captcha::classify(f_captcha, lab_oracle, rm_old = TRUE)
}

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

#' O algoritmo funcionaria assim. Primeiro, ele pega qual é a letra que
#' tem a menor diferença entre o primeiro e o segundo candidatos.
#' Depois, ele "troca" a posição do primeiro canditado com o segundo.
#' Se esse teste já foi realizado, ele verifica qual letra seria considerada,
#' usando o mesmo critério que o anterior (menor diferença entre o primeiro
#' e o segundo candidatos). Dessa vez, no entanto, ele teria de considerar
#' o terceiro candidatos.
#'
#' Vamos pensar em outra heurística: menor probabilidade.
#' Nessa heurística, selecionamos a letra com menor probabilidade
#' e pegamos o segundo candidato dessa letra.


#' Calculates n captcha candidate labels
#'
#' Using all captcha combinations from some cut value
#'
#' @param f_captcha captcha file
#' @param model model that generates logits
#' @param cut_value so that we make combinations from real values
#' @param n maximum number of candidates
#'
#' @export
captcha_candidates <- function(f_captcha, model, cut_value = log(.01), n) {

  # from captcha::decrypt
  model$model$eval()
  transformed <- model$model$transform(f_captcha)

  # calculate log-probability
  probs <- as.matrix(torch::nnf_log_softmax(model$model(transformed)[1,..], 2))

  comb_index <- apply(probs > cut_value, 1, which)
  comb <- purrr::map(purrr::cross(comb_index), purrr::flatten_int)
  comb_matrix <- do.call(rbind, comb)
  candidates <- apply(
    comb_matrix,
    MARGIN = 1,
    FUN = function(x) paste(model$model$vocab[x], collapse = "")
  )
  # calculates the log-likelihood of that candidate
  lkl_candidate <- apply(
    comb_matrix,
    MARGIN = 1,
    FUN = function(x) sum(sapply(seq_along(x), function(z) probs[z,x[z]]))
  )
  candidates <- candidates[order(lkl_candidate, decreasing = TRUE)]
  head(candidates, n)
}

#' Classifica captchas com feedback do oraculo
#'
#' @param path path
#' @param model model
#' @param max_ntry max tries
#' @param manual whether to annotate manually when model fails
#'
#' @export
captcha_oracle_trt_com_feedback <- function(path, model = NULL, max_ntry = 10, manual = TRUE) {

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
  ntry <- 1

  if (is.null(model)) {
    label <- captcha_label(f_captcha)
  } else {
    label <- captcha_candidates(f_captcha, model, n = max_ntry)
  }
  # browser()

  acertou <- captcha_trt_test(u_consulta, token, label[1])
  if (acertou) {
    usethis::ui_done("Acertou!!!")
    label <- label[ntry]
  } else {
    max_ntry_model <- min(max_ntry, length(label))
    usethis::ui_info("Temos {max_ntry_model} candidatos...")
  }

  while (!acertou && ntry <= max_ntry_model && !is.null(model)) {
    usethis::ui_info("Errou! O chute foi: {label[ntry]}")
    ntry <- ntry + 1
    acertou <- captcha_trt_test(u_consulta, token, label[ntry])
    if (acertou) {
      usethis::ui_done("Acertou!!!")
      label <- label[ntry]
    }
  }

  # if tried {max_ntry} times and the model still did not find it
  ntry <- 0
  while (!acertou && ntry < max_ntry && manual) {
    ntry <- ntry + 1
    label <- captcha_label(f_captcha)
    acertou <- captcha_trt_test(u_consulta, token, label)
  }

  lab_oracle <- paste0(label, "_", as.character(as.numeric(acertou)))
  captcha::classify(f_captcha, lab_oracle, rm_old = TRUE)
}

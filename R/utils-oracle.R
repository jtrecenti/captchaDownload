#' Função que baixa um captcha e classifica usando oráculo
#'
#' Essa função é muito relevante para construção dos resultados, pois
#' é responsável por fazer as classificações usando o oráculo dos sites
#'
#' @param path caminho em que os arquivos serão salvos
#' @param model modelo para predizer a label de uma imagem
#' @param max_ntry quantidade máxima de chutes
#' @param manual caso o máximo de tentativas seja alcançado, abrir o prompt
#'   para classificar manualmente?
#' @param captcha_access função que baixa um captcha e retorna dados da
#'   sessão para validar o captcha
#' @param captcha_test função que testa se um captcha está correto
#'   a partir de uma label específica
#'
#' @export
captcha_oracle <- function(path, model = NULL, max_ntry = 10, manual = TRUE,
                           captcha_access, captcha_test) {

  # browser()
  fs::dir_create(path)
  obj <- captcha_access(path)
  f_captcha <- obj$f_captcha
  ntry <- 1
  label <- captcha_candidates(f_captcha, model, n = max_ntry)

  f_log <- paste0(dirname(path), "/logs/", fs::path_ext_set(basename(f_captcha), ".log"))
  fs::dir_create(dirname(f_log))
  acertou <- captcha_test(obj, label[1])

  da_log <- tibble::tibble(
    ntry = ntry,
    label = label[ntry],
    type = "auto",
    result = acertou
  )
  if (acertou) {
    usethis::ui_done("Acertou!!!")
    label <- label[ntry]
  } else {
    max_ntry_model <- min(max_ntry, length(label))
    usethis::ui_info("Temos {max_ntry_model} candidatos...")
  }

  while (!acertou && ntry < max_ntry_model && !is.null(model)) {
    usethis::ui_info("Errou! O chute foi: {label[ntry]}")
    ntry <- ntry + 1
    acertou <- captcha_test(obj, label[ntry])
    da_log <- tibble::add_row(
      da_log,
      ntry = ntry,
      label = label[ntry],
      type = "auto",
      result = acertou
    )
    if (acertou) {
      usethis::ui_done("Acertou!!!")
      label <- label[ntry]
    }
  }

  if (!acertou && !manual) {
    label <- label[ntry]
  }

  # if tried {max_ntry} times and the model still did not find it
  ntry <- 0
  while (!acertou && ntry < max_ntry && manual) {
    ntry <- ntry + 1
    label <- captcha_label(f_captcha)
    acertou <- captcha_test(obj, label)
    da_log <- tibble::add_row(
      da_log,
      label = label,
      type = "manual",
      result = acertou
    )
  }

  if (acertou == 0) {
    usethis::ui_oops("Errado depois de todas as tentativas...")
  }

  lab_oracle <- paste0(label, "_", as.character(as.numeric(acertou)))
  captcha::captcha_annotate(f_captcha, lab_oracle, rm_old = TRUE)
  readr::write_csv(da_log, f_log)

}




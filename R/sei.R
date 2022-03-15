sei_all_numbers <- function() {
  c(
    48:57, #0:9
    65:90, #A-Z
    97:122 #a-z
  )
}

sei_all_chars <- function() {
  c(0:9, LETTERS, letters)
}

captcha_download_sei <- function(path, x, y) {
  f <- fs::file_temp(ext = ".png")
  u <- "https://sei.economia.gov.br/infra_js/infra_gerar_captcha.php"
  u <- glue::glue("{u}?codetorandom={x}-{y}")
  captchaDownload:::captcha_download_generic(
    link = u,
    nm = "sei",
    path = "~/Downloads/sei",
    ext = ".png",
    httr::config(ssl_verifypeer = FALSE)
  )
}

num_to_char <- function(x) {
  nums <- sei_all_numbers()
  char <- sei_all_chars()
  charx <- character(length(x))
  for(xi in seq_along(x)) {
    if (length(char[nums == x[xi]]) > 0) {
      charx[xi] <- char[nums == x[xi]]
    } else {
      charx[xi] <- NA_character_
    }
  }
  charx
}

captcha_classify_sei <- function(x, y) {
  l1 <- num_to_char(x)
  l2 <- num_to_char(y)
  l3 <- num_to_char(ceiling((x + y - 48 * 2) / 2 + 48))
  l4 <- num_to_char(floor((x + y - 48 * 2) / 2 + 48))
  if (is.na(l3)) l3 <- ifelse(x > y, l2, l1)
  if (is.na(l4)) l4 <- ifelse(x > y, l1, l2)
  paste(c(l1, l2, l3, l4), collapse = "")
}

#' Download SEI
#'
#' @param path path to folder where file will be saved
#' @param classify optional. Automatically classify captcha.
#'
#' @export
captcha_download_sei <- function(path, classify = FALSE) {
  ssl <- httr::config(ssl_verifypeer = FALSE)
  u <- "https://sei.economia.gov.br/sei/modulos/pesquisa/md_pesq_processo_pesquisar.php?acao_externa=protocolo_pesquisar&acao_origem_externa=protocolo_pesquisar&id_orgao_acesso_externo=0"
  r <- httr::GET(u, ssl)
  u_captcha_endpoint <- r %>%
    xml2::read_html() %>%
    xml2::xml_find_all("//img[contains(@src,'captcha')]") %>%
    xml2::xml_attr("src")
  u_captcha <- paste0("https://sei.economia.gov.br", u_captcha_endpoint)
  f <- captcha_download_generic(u_captcha, "sei", ".png", path, ssl)
  if (classify) {
    xy <- u_captcha_endpoint %>%
      urltools::param_get("codetorandom") %>%
      stringr::str_split("-") %>%
      unlist() %>%
      as.numeric()
    ans <- captcha_classify_sei(xy[1], xy[2])
    f <- captcha::classify(f, ans, rm_old = TRUE)
  }
  f
}

captcha_sei_test <- function(label, ans) {
  !is.na(label) && label == ans
}

captcha_oracle_sei_com_feedback <- function(path, model = NULL, max_ntry = 10) {

  ssl <- httr::config(ssl_verifypeer = FALSE)
  u <- "https://sei.economia.gov.br/sei/modulos/pesquisa/md_pesq_processo_pesquisar.php?acao_externa=protocolo_pesquisar&acao_origem_externa=protocolo_pesquisar&id_orgao_acesso_externo=0"
  r <- httr::GET(u, ssl)
  u_captcha_endpoint <- r %>%
    xml2::read_html() %>%
    xml2::xml_find_all("//img[contains(@src,'captcha')]") %>%
    xml2::xml_attr("src")
  u_captcha <- paste0("https://sei.economia.gov.br", u_captcha_endpoint)
  f_captcha <- captcha_download_generic(u_captcha, "sei", ".png", path, ssl)
  xy <- u_captcha_endpoint %>%
    urltools::param_get("codetorandom") %>%
    stringr::str_split("-") %>%
    unlist() %>%
    as.numeric()
  ans <- captcha_classify_sei(xy[1], xy[2])

  ntry <- 1

  if (is.null(model)) {
    label <- ans
  } else {
    label <- captcha_candidates(f_captcha, model, n = max_ntry)
  }
  # browser()

  acertou <- captcha_sei_test(label[1], ans)
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
    acertou <- captcha_sei_test(label[ntry], ans)
    if (acertou) {
      usethis::ui_done("Acertou!!!")
      label <- label[ntry]
    }
  }

  # if tried {max_ntry} times and the model still did not find it
  ntry <- 0
  while (!acertou && ntry < max_ntry) {
    ntry <- ntry + 1
    label <- ans
    acertou <- captcha_sei_test(label, ans)
  }
  lab_oracle <- paste0(label, "_", as.character(as.numeric(acertou)))
  captcha::classify(f_captcha, lab_oracle, rm_old = TRUE)
}

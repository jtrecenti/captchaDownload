captcha_download_tjrs <- function(path) {
  u_captcha <- "https://www.tjrs.jus.br/site_php/consulta/human_check/humancheck_showcode.php"
  captcha_download_generic(u_captcha, nm = "tjrs", ext = ".jpeg", path = path)
}


captcha_oracle_tjrs <- function(path, model = NULL) {

  u_principal <- "http://www.tjrs.jus.br/busca/?tb=proc"
  u_verify <- "https://www.tjrs.jus.br/site_php/consulta/verifica_codigo_novo.php"
  u_captcha <- "https://www.tjrs.jus.br/site_php/consulta/human_check/humancheck_showcode.php"

  query <- list(
    "nome_comarca" = "Uruguaiana",
    "versao" = "",
    "versao_fonetica" = "2",
    "tipo" = "1",
    "id_comarca" = "uruguaiana",
    "intervalo_movimentacao" = "0",
    "N1_var2" = "1",
    "id_comarca1" = "uruguaiana",
    "num_processo_mask" = "0010143-19.2016.8.21.0037",
    "num_processo" = "00101431920168210037",
    "numCNJ" = "S",
    "id_comarca2" = "700",
    "uf_oab" = "RS",
    "num_oab" = "",
    "foro" = "0",
    "N1_var2_1" = "1",
    "intervalo_movimentacao_1" = "15",
    "ordem_consulta" = "1",
    "N1_var" = "",
    "id_comarca3" = "todas",
    "nome_parte" = "",
    "intervalo_movimentacao_2" = "0",
    "N1_var2_2" = "1",
    "code" = ""
  )

  httr::handle_reset(u_principal)
  r0 <- httr::GET(u_principal)

  f_captcha <- fs::file_temp(
    pattern = "tjrs",
    tmp_dir = path,
    ext = ".jpeg"
  )

  r_captcha <- httr::GET(
    paste0(u_captcha, "?", trunc(runif(1) * 1e6)),
    httr::write_disk(f_captcha, overwrite = TRUE)
  )

  if (is.null(model)) {
    label <- captcha_label(f_captcha)
  } else {
    label <- captcha::decrypt(f_captcha, model)
  }

  query$code <- label
  r <- httr::GET(u_verify, query = query)
  re_pass <- "Código de Verificação"

  acertou <- !stringr::str_detect(httr::content(r, "text"), re_pass)
  lab_oracle <- paste0(label, "_", as.character(as.numeric(acertou)))
  captcha::classify(f_captcha, lab_oracle, rm_old = TRUE)

}

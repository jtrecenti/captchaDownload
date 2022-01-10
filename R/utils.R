gerador_numero_processo <- function(justica = "8", tribunal = "26") {
  abjutils::sample_cnj(
    1,
    "0001",
    anos = "2019",
    orgao = justica,
    tr = tribunal,
    return_df = FALSE
  )
}


captcha_label <- function(cap) {
  cap_ <- captcha::read_captcha(cap)
  captcha:::plot.captcha(cap_)
  ans <- readline("Answer: ")
  ans
}


captcha_download_generic <- function(link, nm = "captcha", ext = "", path, ...) {
  fs::dir_create(path)
  f <- fs::file_temp(tmp_dir = path, ext = ext, pattern = nm)
  httr::handle_reset(link)
  r_captcha <- httr::GET(link, httr::write_disk(f, TRUE), ...)
  ## for testing purposes
  # r_captcha <- httr::GET(link, httr::write_disk(f, TRUE))
  if (ext == "") {
    ct <- r_captcha$headers[["content-type"]]
    ext <- ifelse(is.null(ct), ext, stringr::str_c(".", basename(ct)))
    f_rename <- fs::path_ext_set(f, ext)
    fs::file_move(f, f_rename)
    f <- f_rename
  }
  f
}

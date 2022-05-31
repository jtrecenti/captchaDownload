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
  # browser()
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

test_that("oracle trt works", {

  model <- captcha::captcha_load_model("/Users/julio/Downloads/trt_99.pt")

  tictoc::tic()
  res <- captcha_oracle(
    "data-raw/trt",
    model = model,
    max_ntry = 10,
    manual = FALSE,
    captcha_access = captcha_access_trt,
    captcha_test = captcha_test_trt
  )
  tictoc::toc()

  expect_s3_class(res, "tbl_df")

})

test_that("oracle jucesp works", {

  model <- captcha::captcha_load_model("/Users/julio/Downloads/model_24(1).pt")

  tictoc::tic()
  res <- captcha_oracle(
    "data-raw/jucesp/testthat",
    model = model,
    max_ntry = 1,
    manual = FALSE,
    captcha_access = captcha_access_jucesp,
    captcha_test = captcha_test_jucesp
  )
  tictoc::toc()


  expect_s3_class(res, "tbl_df")

})

test_that("oracle tjmg works", {

  model <- captcha::captcha_load_model("/Users/julio/Downloads/model_23.pt")

  res <- captcha_oracle(
    "data-raw/tjmg/testthat",
    model = model,
    max_ntry = 10,
    manual = FALSE,
    captcha_access = captcha_access_tjmg,
    captcha_test = captcha_test_tjmg
  )

  expect_s3_class(res, "tbl_df")

})

test_that("oracle tjrs works", {

  model <- captcha::captcha_load_model("/Users/julio/Downloads/model_24.pt")

  tictoc::tic()
  res <- captcha_oracle(
    "data-raw/tjrs/testthat",
    model = model,
    max_ntry = 10,
    manual = FALSE,
    captcha_access = captcha_access_tjrs,
    captcha_test = captcha_test_tjrs
  )
  tictoc::toc()

  expect_s3_class(res, "tbl_df")

})


test_that("oracle tjpe works", {

  model <- captcha::captcha_load_model("/Users/julio/Downloads/model_20.pt")

  tictoc::tic()
  res <- captcha_oracle(
    "data-raw/tjpe/testthat",
    model = model,
    max_ntry = 10,
    manual = FALSE,
    captcha_access = captcha_access_tjpe,
    captcha_test = captcha_test_tjpe
  )
  tictoc::toc()

  expect_s3_class(res, "tbl_df")

})

test_that("oracle cadesp works", {

  model <- captcha::captcha_load_model("/Users/julio/Downloads/model_27.pt")

  tictoc::tic()
  res <- captcha_oracle(
    "data-raw/cadesp/testthat",
    model = model,
    max_ntry = 10,
    manual = FALSE,
    captcha_access = captcha_access_cadesp,
    captcha_test = captcha_test_cadesp
  )
  tictoc::toc()

  expect_s3_class(res, "tbl_df")

})

test_that("oracle esaj works", {

  model <- captcha::captcha_load_model("/Users/julio/Downloads/model_05.pt")

  tictoc::tic()
  res <- captcha_oracle(
    "data-raw/esaj/testthat",
    model = model,
    max_ntry = 10,
    manual = FALSE,
    captcha_access = captcha_access_esaj,
    captcha_test = captcha_test_esaj
  )
  tictoc::toc()

  expect_s3_class(res, "tbl_df")

})

test_that("oracle trf5 works", {

  model <- captcha::captcha_load_model("/Users/julio/Downloads/model_09.pt")

  tictoc::tic()
  res <- captcha_oracle(
    "data-raw/trf5/testthat",
    model = model,
    max_ntry = 10,
    manual = FALSE,
    captcha_access = captcha_access_trf5,
    captcha_test = captcha_test_trf5
  )
  tictoc::toc()

  expect_s3_class(res, "tbl_df")

})


test_that("oracle sei works", {

  model <- captcha::captcha_load_model("/Users/julio/Downloads/model_17.pt")

  tictoc::tic()
  res <- captcha_oracle(
    "data-raw/sei/testthat",
    model = model,
    max_ntry = 10,
    manual = FALSE,
    captcha_access = captcha_access_sei,
    captcha_test = captcha_test_sei
  )
  tictoc::toc()

  expect_s3_class(res, "tbl_df")

})

test_that("oracle rcaptcha works", {

  model <- captcha::captcha_load_model("/Users/julio/Downloads/model_05(1).pt")

  tictoc::tic()
  res <- captcha_oracle(
    "data-raw/rcaptcha/testthat",
    model = model,
    max_ntry = 10,
    manual = FALSE,
    captcha_access = purrr::partial(captcha_access_rcaptcha, n_letter = 6),
    captcha_test = captcha_test_rcaptcha
  )
  tictoc::toc()

  expect_s3_class(res, "tbl_df")

})


test_that("oracle rfb works", {

  model <- captcha::captcha_load_model("/Users/julio/Downloads/model_26.pt")

  tictoc::tic()
  res <- captcha_oracle(
    "data-raw/rfb/testthat",
    model = model,
    max_ntry = 10,
    manual = FALSE,
    captcha_access = captcha_access_rfb,
    captcha_test = captcha_test_rfb
  )
  tictoc::toc()

  expect_s3_class(res, "tbl_df")

})

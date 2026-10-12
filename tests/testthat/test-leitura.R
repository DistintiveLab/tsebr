test_that("tse_read le apenas as colunas pedidas", {
  f <- file.path(tempdir(), "mini_leitura.csv")
  writeLines(c("A;B;C", "1;x;2,5", "2;y;3,5"), f, useBytes = TRUE)
  d <- tsebr::tse_read(f, colunas = c("A", "C"))
  expect_identical(names(d), c("A", "C"))
  expect_identical(nrow(d), 2L)
  expect_equal(d$C[1], 2.5)
})

test_that("tse_read ignora colunas pedidas que nao existem", {
  f <- file.path(tempdir(), "mini_leitura2.csv")
  writeLines(c("A;B", "1;x"), f, useBytes = TRUE)
  d <- tsebr::tse_read(f, colunas = c("A", "Z"))
  expect_identical(names(d), "A")
})

test_that("tse_read sem colunas e com max_rows seguem como antes", {
  f <- file.path(tempdir(), "mini_leitura3.csv")
  writeLines(c("A;B;C", "1;x;2", "2;y;3", "3;z;4"), f, useBytes = TRUE)
  expect_identical(names(tsebr::tse_read(f)), c("A", "B", "C"))
  d <- tsebr::tse_read(f, max_rows = 2L, colunas = c("B", "C"))
  expect_identical(names(d), c("B", "C"))
  expect_identical(nrow(d), 2L)
})

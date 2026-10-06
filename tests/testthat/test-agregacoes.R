test_that("sigla de UF vira codigo IBGE de 2 digitos", {
  expect_identical(unname(tsebr:::.uf_para_ibge("DF")), "53")
  expect_identical(unname(tsebr:::.uf_para_ibge("sp")), "35")
  expect_true(is.na(tsebr:::.uf_para_ibge("XX")))
})

test_that("agregacao por mapa soma por municipio e ignora sem mapa", {
  dados <- data.frame(
    cod_municipio_tse = c("9101", "9101", "9102"),
    votos = c(10, 5, 7))
  mapa <- data.frame(cod_municipio_tse = c("9101", "9102"),
                     ibge = c(5300108, 5300109))
  agg <- tsebr:::.agregar_por_mapa(dados, mapa, "votos")
  expect_setequal(agg$ibge, c(5300108, 5300109))
  expect_identical(agg$valor[agg$ibge == 5300108], 15)
  expect_identical(agg$valor[agg$ibge == 5300109], 7)
})

test_that("saida municipal tem contrato long local/periodo/valor", {
  dados <- data.frame(cod_municipio_tse = "9101", votos = 3)
  mapa <- data.frame(cod_municipio_tse = "9101", ibge = 5300108)
  out <- tsebr:::.agregar_por_mapa(dados, mapa, "votos") |>
    dplyr::rename(local = ibge) |>
    dplyr::mutate(periodo = as.Date("2022-12-31"))
  expect_named(out[1, ], c("local", "valor", "periodo"))
  expect_s3_class(out$periodo, "Date")
})

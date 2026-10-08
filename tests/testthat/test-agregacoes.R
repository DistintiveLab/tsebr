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

test_that("mapa do tse_municipios (geoloc_id) e aceito sem ibge", {
  dados <- data.frame(cod_municipio_tse = c(9101, 9101), votos = c(10, 5))
  mapa <- data.frame(cod_municipio_tse = "9101", geoloc_id = 5300108,
                     empate = FALSE)
  agg <- tsebr:::.agregar_por_mapa(dados, mapa, "votos")
  expect_identical(agg$valor[agg$ibge == 5300108], 15)
})

test_that("mapa com empate e descartado; sem mapa nao junta nada", {
  dados <- data.frame(cod_municipio_tse = c("9101", "9102"), votos = c(3, 4))
  mapa <- data.frame(cod_municipio_tse = c("9101", "9102"),
                     ibge = c(5300108, 5300109), empate = c(FALSE, TRUE))
  agg <- tsebr:::.agregar_por_mapa(dados, mapa, "votos")
  expect_setequal(agg$ibge, 5300108)
  expect_identical(agg$valor[1], 3)
})

test_that("grupos extras (periodo) sao mantidos na chave da saida", {
  dados <- data.frame(
    cod_municipio_tse = "9101",
    periodo = as.Date(c("2022-12-31", "2018-12-31")),
    votos = c(10, 5))
  mapa <- data.frame(cod_municipio_tse = "9101", ibge = 5300108)
  agg <- tsebr:::.agregar_por_mapa(dados, mapa, "votos",
                                   grupos = c("ibge", "periodo"))
  expect_identical(nrow(agg), 2L)
  expect_setequal(agg$valor, c(10, 5))
})

test_that("valor ausente e mapa sem colunas falham com erro claro", {
  dados <- data.frame(cod_municipio_tse = "9101", votos = 1)
  expect_error(tsebr:::.agregar_por_mapa(dados, data.frame(a = 1), "votos"),
               "mapa sem as colunas")
  expect_error(tsebr:::.agregar_por_mapa(dados[, "cod_municipio_tse",
                                               drop = FALSE],
                                         data.frame(cod_municipio_tse = "9101",
                                                    ibge = 1), "votos"),
               "coluna de valor")
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

test_that("URL da CDN muda de layout conforme o ano", {
  expect_identical(
    .tse_cdn_url(2022, "votacao_secao", "votacao_secao_2022_DF.zip"),
    paste0("https://cdn.tse.jus.br/estatistica/sead/odsele/",
           "votacao_secao/votacao_secao_2022_DF.zip"))
  expect_identical(
    .tse_cdn_url(2026, "perfil_eleitor_secao",
                 "perfil_eleitor_secao_2026_DF.zip"),
    paste0("https://cdn.tse.jus.br/estatistica/sead/eleicoes/",
           "eleicoes2026/perfil_eleitor_secao/",
           "perfil_eleitor_secao_2026_DF.zip"))
  expect_match(.tse_cdn_hash_url("https://x/y.zip"), "\\.sha512$")
})

test_that("conformar renomeia por padrao sem colisao", {
  d <- data.frame(SG_UF = "DF", NR_SECAO = 1, QT_ELEITORES = 10)
  out <- conformar(d, tse_layouts()$perfil_eleitor_secao)
  expect_identical(names(out), c("uf", "secao", "QT_ELEITORES"))
})

test_that("layouts cobrem as familias planejadas", {
  expect_setequal(
    names(tse_layouts()),
    c("votacao_secao", "detalhe_votacao_secao", "perfil_eleitor_secao",
      "eleitorado_local_votacao", "consulta_cand",
      "receitas_candidato", "despesas_candidato"))
})

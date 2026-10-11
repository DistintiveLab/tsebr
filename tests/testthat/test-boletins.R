recursos_fake <- tibble::tibble(
  name = c(
    "AC - Boletim de Urna - Primeiro turno - 05.10.2026",
    "AC - Hash SHA2 - Primeiro turno - 05.10.2026",
    "PR - Boletim de Urna - Primeiro turno - 05.10.2026",
    "PR - Hash SHA2 - Primeiro turno - 05.10.2026",
    "RN - Boletim de Urna - Primeiro turno - 05.10.2026",
    "RO - Boletim de Urna - Primeiro turno - 05.10.2026",
    "SP - Boletim de Urna - Primeiro turno - 05.10.2026"),
  url = c("cdn/bweb_1t_AC_x.zip", "cdn/bweb_1t_AC_x.zip.sha512",
          "cdn/bweb_1t_PR_x.zip", "cdn/bweb_1t_PR_x.zip.sha512",
          "cdn/bweb_1t_RN_x.zip", "cdn/bweb_1t_RO_x.zip",
          "cdn/bweb_1t_SP_x.zip"))

test_that(".recurso_bweb escolhe o recurso certo por sigla delimitada", {
  for (uf in c("AC", "PR", "RN", "RO", "SP")) {
    rec <- tsebr:::.recurso_bweb(recursos_fake, uf)
    expect_identical(nrow(rec), 1L, label = uf)
    expect_true(grepl(sprintf("_1t_%s_", uf), rec$url[1]), label = uf)
  }
})

test_that(".recurso_bweb nao confunde 'PRimeiro', 'Urn(a)' e 'Primei(ro)'", {
  ## nomes reais do CKAN em 10/2026: sigla solta casa todos os 56
  ## recursos e url[1] devolve AC para PR/RN/RO
  ac <- tsebr:::.recurso_bweb(recursos_fake, "PR")
  expect_false(grepl("_1t_AC_", ac$url[1]))
  expect_false(nrow(tsebr:::.recurso_bweb(recursos_fake, "ZZ")) > 0)
})

test_that(".recurso_bweb aceita nome de arquivo no estilo do CDN", {
  recs <- tibble::tibble(
    name = c("bweb_1t_AC_051020261403.zip", "bweb_1t_PR_051020261403.zip"),
    url = c("cdn/bweb_1t_AC.zip", "cdn/bweb_1t_PR.zip"))
  expect_identical(nrow(tsebr:::.recurso_bweb(recs, "PR")), 1L)
  expect_true(grepl("_1t_PR", tsebr:::.recurso_bweb(recs, "PR")$url[1]))
  expect_identical(nrow(tsebr:::.recurso_bweb(recs, "AC")), 1L)
})

#' Resultados eleitorais de 2026 via boletim de urna WEB (bweb)
#'
#' O dataset `resultados-2026-boletim-de-urna` do CKAN/CDN publica
#' o boletim de urna WEB (bweb) como CSV por UF — votos por
#' candidato em cada secao, com aptos, comparecimento e abstencoes.
#' Este modulo baixa o CSV, agrega por municipio e devolve no
#' formato longo do DW beep.
#'
#' O TSE bloqueia IPs que passam de ~100 req/s (limite 10 min);
#' aqui e um unico download por UF (sem rate limit).
#'
#' @param ano Ano eleitoral (2026).
#' @param uf Sigla da UF ou "all" (baixa 27 UFs sequencialmente).
#' @param dest_dir Diretorio de cache.
#' @return `data.frame` com votos por candidato por municipio.
#' @examples
#' \dontrun{
#' r <- tse_boletins(2026, uf = "DF")
#' }
#' @export
tse_boletins <- \(ano = 2026, uf, dest_dir = NULL) {
  uf <- toupper(uf)

  ## URL do bweb: primeiro turno, timestamp do CDN muda por UF —
  ## descobrir via CKAN (resultados-2026-boletim-de-urna)
  ds <- tse_search(q = "", fq = sprintf('name:resultados-%d-boletim-de-urna', ano))
  if (!nrow(ds)) {
    stop("tse_boletins: dataset resultados-", ano,
         "-boletim-de-urna nao encontrado no CKAN")
  }
  recs <- tse_show(ds$name[1], padrao = sprintf("_1t_%s_", uf))
  if (!nrow(recs)) {
    ## fallback: padrao sem o turno
    recs <- tse_show(ds$name[1], padrao = uf)
    recs <- recs[!grepl("sha512", recs$url), ]
  }
  if (!nrow(recs)) {
    stop("tse_boletins: sem bweb para ", uf, " em ", ds$name[1])
  }
  url_zip <- recs$url[1]

  ## download + leitura (um CSV nacional por UF dentro do zip)
  zip <- tse_download(url_zip, dest_dir)
  dados <- tse_read(zip)

  ## renomeia colunas-chave
  renomear <- c(
    "ANO_ELEICAO" = "ano", "SG_UF" = "uf",
    "CD_MUNICIPIO" = "cod_municipio_tse", "NM_MUNICIPIO" = "municipio",
    "NR_ZONA" = "zona", "NR_SECAO" = "secao",
    "DS_CARGO_PERGUNTA" = "cargo",
    "NR_VOTAVEL" = "nr_votavel", "NM_VOTAVEL" = "nm_votavel",
    "QT_VOTOS" = "votos",
    "QT_APTOS" = "aptos", "QT_COMPARECIMENTO" = "comparecimento",
    "QT_ABSTENCOES" = "abstencoes")
  for (de in names(renomear)) {
    if (de %in% names(dados)) names(dados)[names(dados) == de] <- renomear[[de]]
  }

  ## adiciona periodo
  dados$periodo <- as.Date(paste0(ano, "-10-04"))
  dados
}

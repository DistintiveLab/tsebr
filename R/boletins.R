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
  recs <- .recurso_bweb(tse_show(ds$name[1]), uf)
  if (!nrow(recs)) {
    stop("tse_boletins: sem bweb para ", uf, " em ", ds$name[1])
  }
  url_zip <- recs$url[1]

  ## download + leitura (um CSV nacional por UF dentro do zip);
  ## so as colunas-chave: o bweb de SP tem 8+ milhoes de linhas e a
  ## leitura integral estoura a memoria do host
  zip <- tse_download(url_zip, dest_dir)
  renomear <- c(
    "ANO_ELEICAO" = "ano", "SG_UF" = "uf",
    "CD_MUNICIPIO" = "cod_municipio_tse", "NM_MUNICIPIO" = "municipio",
    "NR_ZONA" = "zona", "NR_SECAO" = "secao", "NR_TURNO" = "turno",
    "DS_CARGO_PERGUNTA" = "cargo",
    "NR_VOTAVEL" = "nr_votavel", "NM_VOTAVEL" = "nm_votavel",
    "QT_VOTOS" = "votos",
    "QT_APTOS" = "aptos", "QT_COMPARECIMENTO" = "comparecimento",
    "QT_ABSTENCOES" = "abstencoes")
  dados <- tse_read(zip, colunas = names(renomear))
  for (de in names(renomear)) {
    if (de %in% names(dados)) names(dados)[names(dados) == de] <- renomear[[de]]
  }

  ## adiciona periodo
  dados$periodo <- as.Date(paste0(ano, "-10-04"))

  ## o estilo do nome do recurso no CKAN pode mudar de novo; o dado,
  ## nao: aborta antes de devolver se a UF lida nao e a pedida (um
  ## casamento errado ja gravou dados de AC no lugar de PR/RN/RO)
  if ("uf" %in% names(dados)) {
    ufs_lidas <- unique(toupper(dados$uf))
    if (!identical(ufs_lidas, uf)) {
      stop("tse_boletins: o recurso escolhido (", basename(url_zip),
           ") trouxe uf = ", paste(ufs_lidas, collapse = ", "),
           ", nao ", uf, " — casamento de recurso errado no CKAN")
    }
  }
  dados
}

#' Escolhe o recurso do bweb de uma UF entre os recursos do dataset
#'
#' O nome do recurso no CKAN muda de estilo entre ciclos: titulo
#' (`"AC - Boletim de Urna - Primeiro turno - 05.10.2026"`) ou nome
#' de arquivo (`"bweb_1t_AC_051020261403.zip"`). Um grep solto da
#' sigla é armadilha: `PR` casa "**PR**imeiro turno", `RN` casa
#' "U**rn**a" e `RO` casa "Primei**ro**" — e devolve o recurso da
#' primeira UF do dataset (AC) para todas elas. A sigla só vale como
#' pedaço delimitado por não-letra.
#'
#' @param recs Recursos de `tse_show()` (name, url).
#' @param uf Sigla da UF (maiúscula).
#' @return Subconjunto de `recs` da UF pedida (0 linhas se não achar).
#' @keywords internal
.recurso_bweb <- \(recs, uf) {
  recs <- recs[!grepl("sha512", recs$url), ]
  padrao <- sprintf("(^|[^A-Z])%s([^A-Z]|$)", uf)
  hit <- recs[grepl(padrao, recs$name, ignore.case = TRUE), ]
  if (nrow(hit) > 1) {
    bol <- hit[grepl("boletim|bweb", hit$name, ignore.case = TRUE), ]
    if (nrow(bol)) hit <- bol
  }
  hit
}

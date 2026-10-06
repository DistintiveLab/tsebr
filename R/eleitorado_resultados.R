#' Votacao por secao eleitoral (resultados totalizados)
#'
#' Baixa e le `votacao_secao_{ano}_{uf}.zip` (votos nominais por
#' secao) ou `detalhe_votacao_secao_{ano}_{uf}.zip` (aptos,
#' comparecimento, abstencoes, nulos e brancos por secao).
#'
#' **Eleicoes 2026:** o dataset `resultados-2026` ainda nao
#' estava publicado no CKAN em 06/10/2026 (`package_show` ->
#' 404; verificavel com `tse_search(fq = 'tags:"Ano 2026"')`).
#' Pelo padrao 2022/2024 a funcao ja emite as URLs esperadas e
#' passa a funcionar na publicacao; enquanto isso, resultados
#' por urna existem nos boletins de `resultados.tse.jus.br`
#' (endpoint fora do CKAN, planejado para `tse_boletins()`).
#'
#' @param ano Ano eleitoral (federais: 1998..2024 confirmados;
#'   2026 quando publicado; municipais: pares).
#' @param uf Sigla da UF (obrigatorio; a votacao por secao nao
#'   tem arquivo nacional) ou "all" para baixar as 27 UFs
#'   sequencialmente (volumes de varios GB).
#' @param detalhe FALSE (default) devolve a votacao nominal;
#'   TRUE devolve o detalhe (aptos/abstencoes/nulos/brancos).
#' @param dest_dir Diretorio de cache; ver `tse_download()`.
#' @return `data.frame` conformado pelo layout
#'   `votacao_secao`/`detalhe_votacao_secao` de `tse_layouts()`.
#' @examples
#' \dontrun{
#' vot <- tse_resultados_secao(2022, uf = "DF")
#' det <- tse_resultados_secao(2022, uf = "DF", detalhe = TRUE)
#' }
#' @export
tse_resultados_secao <- \(ano, uf, detalhe = FALSE, dest_dir = NULL) {
  assunto <- if (detalhe) "detalhe_votacao_secao" else "votacao_secao"
  ufs <- .tse_ufs(uf)
  puxar <- \(sg) {
    arquivo <- sprintf("%s_%s_%s.zip", assunto, ano, sg)
    zip <- tse_download(.tse_cdn_url(ano, assunto, arquivo), dest_dir)
    conformar(tse_read(zip), tse_layouts()[[assunto]])
  }
  if (length(ufs) == 1L) return(puxar(ufs))
  data.table::rbindlist(lapply(ufs, puxar), fill = TRUE) |>
    tibble::as_tibble()
}

#' Perfil do eleitorado por secao eleitoral
#'
#' Baixa e le `perfil_eleitor_secao_{ano}_{uf}.zip`: eleitores
#' por secao com faixa etaria, escolaridade, genero etc.
#' Sufixo `"Inválida"` em faixa etaria existe e pode ser
#' descartado com um filtro simples.
#'
#' @param ano Ano eleitoral.
#' @param uf Sigla da UF ou "all".
#' @param dest_dir Diretorio de cache.
#' @return `data.frame` conformado pelo layout
#'   `perfil_eleitor_secao`.
#' @examples
#' \dontrun{
#' perf <- tse_perfis_secao(2026, uf = "DF")
#' }
#' @export
tse_perfis_secao <- \(ano, uf, dest_dir = NULL) {
  assunto <- "perfil_eleitor_secao"
  ufs <- .tse_ufs(uf)
  puxar <- \(sg) {
    arquivo <- sprintf("%s_%s_%s.zip", assunto, ano, sg)
    zip <- tse_download(.tse_cdn_url(ano, assunto, arquivo), dest_dir)
    conformar(tse_read(zip), tse_layouts()[[assunto]])
  }
  if (length(ufs) == 1L) return(puxar(ufs))
  data.table::rbindlist(lapply(ufs, puxar), fill = TRUE) |>
    tibble::as_tibble()
}

#' Eleitorado por local de votacao
#'
#' Baixa e le `eleitorado_local_votacao_{ano}.zip` (arquivo
#' nacional unico): secoes com local, endereco, bairro TSE e
#' coordenadas oficiais (`lat`/`lon`) - insumo direto para
#' cruzamentos territoriais por spatial join.
#'
#' @param ano Ano eleitoral.
#' @param uf Filtro opcional de sigla apos a leitura.
#' @param dest_dir Diretorio de cache.
#' @return `data.frame` conformado pelo layout
#'   `eleitorado_local_votacao`.
#' @examples
#' \dontrun{
#' lv <- tse_locais_votacao(2026, uf = "DF")
#' }
#' @export
tse_locais_votacao <- \(ano, uf = NULL, dest_dir = NULL) {
  assunto <- "eleitorado_local_votacao"
  arquivo <- sprintf("%s_%s.zip", assunto, ano)
  zip <- tse_download(.tse_cdn_url(ano, assunto, arquivo), dest_dir)
  dados <- conformar(tse_read(zip), tse_layouts()[[assunto]])
  if (!is.null(uf)) dados <- dados[dados$uf == toupper(uf), ]
  dados
}

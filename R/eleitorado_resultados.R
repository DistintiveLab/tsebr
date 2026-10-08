tse_resultados_secao <- \(ano, uf, detalhe = FALSE, dest_dir = NULL) {
  assunto <- if (detalhe) "detalhe_votacao_secao" else "votacao_secao"
  if (detalhe) {
    ## detalhe_votacao_secao: zip NACIONAL (um arquivo com todas as UFs)
    arquivo <- sprintf("%s_%s.zip", assunto, ano)
    zip <- tse_download(.tse_cdn_url(ano, assunto, arquivo), dest_dir)
    dados <- conformar(tse_read(zip), tse_layouts()[[assunto]])
    ufs <- .tse_ufs(uf)
    if (length(ufs) < 5L) dados <- dados[toupper(dados$uf) %in% ufs, ]
  } else {
    ## votacao_secao: zip POR UF (um arquivo por estado)
    ufs <- setdiff(.tse_ufs(uf), "ZZ")
    puxar <- \(sg) {
      arquivo <- sprintf("%s_%s_%s.zip", assunto, ano, sg)
      zip <- tse_download(.tse_cdn_url(ano, assunto, arquivo), dest_dir)
      conformar(tse_read(zip), tse_layouts()[[assunto]])
    }
    dados <- if (length(ufs) == 1L) puxar(ufs) else
      data.table::rbindlist(lapply(ufs, puxar), fill = TRUE)
  }
  dados
}

#' Votacao por secao do presidente (zip BR nacional, 2018+)
#'
#' A partir de 2018 o TSE publica a votacao por secao para
#' PRESIDENTE em um zip nacional proprio
#' (`votacao_secao_{ano}_BR.zip`); os zips por UF trazem apenas os
#' cargos estaduais. Le o arquivo uma unica vez e filtra as UFs.
#' @keywords internal
tse_resultados_secao_br <- \(ano, ufs, dest_dir = NULL) {
  arquivo <- sprintf("votacao_secao_%s_BR.zip", ano)
  zip <- tse_download(.tse_cdn_url(ano, "votacao_secao", arquivo), dest_dir)
  dados <- conformar(tse_read(zip), tse_layouts()$votacao_secao)
  dados[toupper(dados$uf) %in% ufs, , drop = FALSE]
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
  if (!is.null(uf)) {
    manter <- dados$uf == toupper(uf)
    dados <- dados[manter, , drop = FALSE]
  }
  dados
}

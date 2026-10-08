#' Resultados eleitorais de 2026 via resultados.tse.jus.br
#'
#' O dataset `resultados-2026` ainda não foi publicado no CKAN.
#' Enquanto isso, o TSE serve boletins de urna e resultados
#' agregados via `resultados.tse.jus.br` — o mesmo endpoint usado
#' pelo projeto ondedapraconversar.
#'
#' @param ano Ano eleitoral (2026).
#' @param uf Sigla da UF.
#' @param dest_dir Diretorio de cache.
#' @return `data.frame` com votos por candidato/município.
#' @examples
#' \dontrun{
#' r <- tse_boletins(2026, uf = "DF")
#' }
#' @export
tse_boletins <- \(ano = 2026, uf, dest_dir = NULL) {
  base <- "https://resultados.tse.jus.br/oficial"
  uf <- toupper(uf)

  ## 1) config: mapeia a eleicao para o codigo do pleito
  cfg_url <- paste0(base, "/comum/config/ele-c.json")
  cfg <- tryCatch(jsonlite::fromJSON(cfg_url), error = \(e) NULL)
  if (is.null(cfg)) {
    stop("tse_boletins: sem acesso a ", cfg_url)
  }
  pleitos <- cfg$pleitos |>
    (\(x) do.call(rbind, lapply(names(x), \(k)
      data.frame(pleito = k, descricao = x[[k]]))))()
  pleito_row <- pleitos[grepl(as.character(ano), pleitos$descricao), ]
  if (!nrow(pleito_row)) {
    stop("tse_boletins: pleito ", ano, " nao encontrado no config")
  }

  ## 2) lista de municipios da UF
  mun_url <- paste0(base, "/ele", ano, "/config/mun-e", ano, "/municipios.json")
  muns <- tryCatch(jsonlite::fromJSON(mun_url), error = \(e) NULL)
  if (is.null(muns)) {
    muns <- tryCatch(jsonlite::fromJSON(paste0(base, "/ele", ano,
      "/config/mun/municipios.json")), error = \(e) NULL)
  }
  if (is.null(muns)) {
    stop("tse_boletins: sem lista de municipios para ", ano)
  }
  muns_df <- if (is.data.frame(muns)) muns else
    do.call(rbind, lapply(muns, \(m) data.frame(
      codigo = m$codigo, nome = m$nome)))

  ## 3) para cada municipio, baixa o JSON de resultados
  resultados <- lapply(seq_len(nrow(muns_df)), \(i) {
    mun_cod <- muns_df$codigo[i]
    mun_nome <- muns_df$nome[i]
    for (code in pleito_row$pleito) {
      url <- paste0(base, "/ele", ano, "/", code,
                    "/dados/", uf, "/", mun_cod, "/",
                    "p000", code, "-", uf, "-m", mun_cod, ".json")
      r <- tryCatch(jsonlite::fromJSON(url), error = \(e) NULL)
      if (!is.null(r)) {
        cand <- r$cand
        if (!is.null(cand) && nrow(cand) > 0) {
          return(data.frame(
            ano = ano, uf = uf, cod_municipio = mun_cod,
            municipio = mun_nome,
            nr_candidato = as.character(cand$n),
            nome = as.character(cand$nm),
            partido = as.character(cand$cc),
            votos = as.numeric(cand$vap),
            stringsAsFactors = FALSE))
        }
      }
    }
    NULL
  })
  resultado <- data.table::rbindlist(
    Filter(Negate(is.null), resultados), fill = TRUE)
  if (!nrow(resultado)) {
    stop("tse_boletins: nenhum resultado baixado para ", uf, " ", ano)
  }
  resultado
}

#' Resultados 2026 agregados por município (via boletins)
#'
#' Wrapper de conveniencia: chama [tse_boletins()] e devolve
#' no formato longo (local, periodo, valor) para o DW beep.
#'
#' @inheritParams tse_boletins
#' @param mapa `data.frame` com cod_municipio_tse e ibge (7d);
#'   sem ele, devolve os codigos TSE.
#' @export
tse_resultados_2026_municipio <- \(uf, mapa = NULL, dest_dir = NULL) {
  dados <- tse_boletins(2026, uf = uf, dest_dir = dest_dir)
  if (is.null(mapa)) {
    return(dados |> dplyr::mutate(periodo = as.Date("2026-10-31")))
  }
  dados |>
    dplyr::left_join(mapa, by = c("cod_municipio" = "cod_municipio_tse")) |>
    dplyr::filter(!is.na(ibge)) |>
    dplyr::mutate(local = as.numeric(ibge),
                  periodo = as.Date("2026-10-31"),
                  valor = votos) |>
    dplyr::select(local, periodo, valor, uf, municipio,
                  nr_candidato, nome, partido)
}

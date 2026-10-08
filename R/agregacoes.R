#' Converte sigla de UF em codigo IBGE de 2 digitos
#' @keywords internal
.uf_para_ibge <- \(uf) {
  tabela <- c(AC = "12", AL = "27", AM = "13", AP = "16", BA = "29",
              CE = "23", DF = "53", ES = "32", GO = "52", MA = "21",
              MG = "31", MS = "50", MT = "51", PA = "15", PB = "25",
              PE = "26", PI = "22", PR = "41", RJ = "33", RN = "24",
              RO = "11", RR = "14", RS = "43", SC = "42", SE = "28",
              SP = "35", TO = "17")
  tabela[toupper(uf)]
}

#' Agrega linhas por municipio usando o mapa TSE x IBGE
#'
#' Helper puro (testavel offline): junta `dados` ao `mapa`
#' (cod_municipio_tse -> ibge 7 digitos), agrupa pelo codigo IBGE
#' e soma `valor`.
#' @keywords internal
.agregar_por_mapa <- \(dados, mapa, valor) {
  dados$.valor <- as.numeric(dados[[valor]])
  saida <- dplyr::left_join(
    dados,
    mapa[, c("cod_municipio_tse", "ibge")] |>
      dplyr::mutate(cod_municipio_tse = as.character(cod_municipio_tse)),
    by = c(cod_municipio_tse = "cod_municipio_tse"))
  saida |>
    dplyr::group_by(ibge) |>
    dplyr::summarise(valor = sum(.valor, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::filter(!is.na(ibge))
}

#' Votos nominais por municipio
#'
#' Agrega `votacao_secao` ao nivel municipal (codigo IBGE 7
#' digitos) usando `mapa` (de `tse_municipios()`). Sem
#' `nr_votavel`, devolve o total de votos nominais do cargo por
#' municipio; com `nr_votavel`, apenas aquele votavel.
#'
#' @param ano Ano eleitoral.
#' @param uf Sigla da UF.
#' @param cargo Regex de cargo (ex.: "PRESIDENTE"), opcional.
#' @param nr_votavel Numero do candidato/partido, opcional.
#' @param mapa `data.frame` com cod_municipio_tse e ibge
#'   (7 digitos); sem ele, `local` sai como codigo TSE (nao
#'   encaixa no DW beep).
#' @param dest_dir Diretorio de cache do download.
#' @return `data.frame` long: local, periodo, valor (+ uf, cargo).
#' @examples
#' \dontrun{
#' mapa <- tse_municipios(2022, con = con_dw, uf = "DF")
#' tse_resultados_municipio(2022, "DF", cargo = "PRESIDENTE",
#'                          nr_votavel = 13, mapa = mapa)
#' }
#' @export
tse_resultados_municipio <- \(ano = NULL, uf, cargo = NULL, nr_votavel = NULL,
                              mapa = NULL, dest_dir = NULL) {
  anos <- if (is.null(ano)) tse_anos_disponiveis("todas") else
    sort(unique(as.integer(ano)))
  dados <- data.table::rbindlist(lapply(anos, \(a)
    tse_resultados_secao(a, uf, detalhe = FALSE, dest_dir = dest_dir)),
    fill = TRUE)
  if (is.null(mapa)) mapa <- .mapa_municipios_interno(anos, uf)
  if (!is.null(cargo) && "cargo" %in% names(dados)) {
    dados <- dados[grepl(cargo, dados$cargo, ignore.case = TRUE), ]
  }
  if (!is.null(nr_votavel)) {
    dados <- dados[dados$nr_votavel == as.character(nr_votavel), ]
  }
  if (is.null(mapa)) {
    saida <- dados |>
      dplyr::group_by(cod_municipio_tse) |>
      dplyr::summarise(valor = sum(as.numeric(votos), na.rm = TRUE),
                       .groups = "drop") |>
      dplyr::mutate(local = as.numeric(cod_municipio_tse))
  } else {
    agg <- .agregar_por_mapa(dados, mapa, "votos")
    saida <- agg |> dplyr::rename(local = ibge)
  }
  saida |>
    dplyr::mutate(periodo = as.Date(paste0(ano, "-12-31")),
                  uf = toupper(uf)) |>
    dplyr::select(local, periodo, valor, uf, dplyr::everything())
}

#' Totais de secao por municipio (aptos, abstencoes, nulos, brancos)
#'
#' Agrega `detalhe_votacao_secao` ao nivel municipal. `metrica`
#' e uma das colunas conformadas: aptos, comparecimento,
#' abstencoes, votos_nulos ou votos_brancos.
#'
#' @inheritParams tse_resultados_municipio
#' @param metrica Coluna do detalhe a agregar.
#' @return `data.frame` long: local, periodo, valor, uf, metrica.
#' @examples
#' \dontrun{
#' tse_detalhe_municipio(2022, "DF", metrica = "abstencoes",
#'                       mapa = mapa)
#' }
#' @export
tse_detalhe_municipio <- \(ano = NULL, uf, metrica = c("aptos", "comparecimento",
                                                "abstencoes", "votos_nulos",
                                                "votos_brancos"),
                           mapa = NULL, dest_dir = NULL) {
  metrica <- match.arg(metrica)
  anos <- if (is.null(ano)) tse_anos_disponiveis("todas") else
    sort(unique(as.integer(ano)))
  dados <- data.table::rbindlist(lapply(anos, \(a)
    tse_resultados_secao(a, uf, detalhe = TRUE, dest_dir = dest_dir)),
    fill = TRUE)
  if (is.null(mapa)) mapa <- .mapa_municipios_interno(anos, uf)
  if (!metrica %in% names(dados)) {
    stop("tsebr: metrica '", metrica, "' ausente no detalhe ", ano,
         " (colunas: ", paste(names(dados), collapse = ", "), ")")
  }
  if (is.null(mapa)) {
    saida <- dados |>
      dplyr::group_by(cod_municipio_tse) |>
      dplyr::summarise(valor = sum(as.numeric(.data[[metrica]]),
                                   na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(local = as.numeric(cod_municipio_tse))
  } else {
    saida <- .agregar_por_mapa(dados, mapa, metrica) |>
      dplyr::rename(local = ibge)
  }
  saida |>
    dplyr::mutate(periodo = as.Date(paste0(ano, "-12-31")),
                  uf = toupper(uf), metrica = metrica) |>
    dplyr::select(local, periodo, valor, uf, metrica)
}

#' Totais de prestacao de contas por UF
#'
#' Soma receitas (ou despesas) por UF e devolve `local` como
#' codigo IBGE de 2 digitos (encaixa no nivel UF do DW beep).
#' Nao discrimina candidato: para series por candidato, use
#' `tse_prestacao()` e trate fora do DW.
#'
#' @param ano Ano eleitoral.
#' @param tipo "receitas" ou "despesas".
#' @param dest_dir Diretorio de cache do download.
#' @return `data.frame` long: local (IBGE 2d), periodo, valor,
#'   uf, tipo.
#' @examples
#' \dontrun{
#' tse_prestacao_uf(2026, tipo = "receitas")
#' }
#' @export
tse_prestacao_uf <- \(ano = NULL, tipo = c("receitas", "despesas"),
                      dest_dir = NULL) {
  tipo <- match.arg(tipo)
  anos <- if (is.null(ano)) tse_anos_disponiveis("todas") else
    sort(unique(as.integer(ano)))
  dados <- data.table::rbindlist(lapply(anos, \(a)
    tse_prestacao(a, tipo = tipo, dest_dir = dest_dir)), fill = TRUE)
  dados |>
    dplyr::group_by(uf) |>
    dplyr::summarise(valor = sum(as.numeric(valor), na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::mutate(local = as.numeric(.uf_para_ibge(uf)),
                  periodo = as.Date(paste0(ano, "-12-31")),
                  tipo = tipo) |>
    dplyr::filter(!is.na(local)) |>
    dplyr::select(local, periodo, valor, uf, tipo)
}

#' Mapa TSE x IBGE interno (conexao padrao via env vars)
#' @keywords internal
.mapa_municipios_interno <- \(anos, uf) {
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        user = Sys.getenv("user", "beep"),
                        password = Sys.getenv("password", "aEd1#man@gR"),
                        host = Sys.getenv("host", "127.0.0.1"),
                        dbname = Sys.getenv("dbname", "beepdb"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  ano_max <- max(anos, na.rm = TRUE)
  tse_municipios(ano_max, con = con, uf = uf)
}

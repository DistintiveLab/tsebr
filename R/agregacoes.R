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
#' (cod_municipio_tse -> ibge 7 digitos), agrupa por `grupos`
#' (default: so ibge; passe extras como periodo para manter a
#' chave do DW) e soma `valor`. Aceita mapa no formato de
#' [tse_municipios()] (coluna `geoloc_id` no lugar de `ibge`);
#' municipios marcados com `empate = TRUE` sao descartados do mapa
#' (correspondencia ambigua por nome) e linhas sem correspondencia
#' saem do resultado.
#' @keywords internal
.agregar_por_mapa <- \(dados, mapa, valor, grupos = "ibge") {
  if (!valor %in% names(dados)) {
    stop("tsebr: coluna de valor '", valor, "' ausente nos dados ",
         "(colunas: ", paste(names(dados), collapse = ", "), ")")
  }
  if (!"ibge" %in% names(mapa) && "geoloc_id" %in% names(mapa)) {
    mapa$ibge <- as.numeric(mapa$geoloc_id)
  }
  faltando <- setdiff(c("cod_municipio_tse", "ibge"), names(mapa))
  if (length(faltando)) {
    stop("tsebr: mapa sem as colunas ", paste(faltando, collapse = ", "),
         " — use a saida de tse_municipios() ou um data.frame com ",
         "cod_municipio_tse e ibge")
  }
  if ("empate" %in% names(mapa)) {
    mapa <- mapa[is.na(mapa$empate) | !mapa$empate, , drop = FALSE]
  }
  valores <- as.numeric(dados[[valor]])
  dados <- dplyr::mutate(
    dados,
    .valor = valores,
    cod_municipio_tse = as.character(cod_municipio_tse))
  mapa <- dplyr::mutate(
    mapa,
    cod_municipio_tse = as.character(cod_municipio_tse),
    ibge = as.numeric(ibge))
  saida <- dplyr::left_join(
    dados,
    mapa[, c("cod_municipio_tse", "ibge")],
    by = c(cod_municipio_tse = "cod_municipio_tse"))
  saida |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grupos))) |>
    dplyr::summarise(valor = sum(.valor, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::filter(!is.na(ibge))
}

#' Votos por municipio, turno, cargo e votavel
#'
#' Agrega `votacao_secao` por municipio TSE x turno x cargo x
#' votavel, processando **um ano e uma UF por vez**: o conjunto
#' nacional por secao nao cabe em memoria (foi a causa de sessoes
#' do R mortas em cargas "all"). Os codigos de municipio sao os do
#' TSE (`cod_municipio_tse`); para encaixar no DW beep, use
#' [tse_resultados_municipio()], que junta o mapa IBGE.
#'
#' @param ano Ano eleitoral ou vetor de anos; NULL = todas as
#'   eleicoes disponiveis.
#' @param uf Sigla da UF ou "all".
#' @param cargo Regex de cargo (ex.: "PRESIDENTE"), opcional.
#' @param nr_votavel Numero do candidato/partido, opcional.
#' @param dest_dir Diretorio de cache do download.
#' @return `data.frame`: ano, periodo, uf, cod_municipio_tse,
#'   municipio, turno, cargo, nr_votavel, nm_votavel, votos.
#' @examples
#' \dontrun{
#' v <- tse_votacao_municipio(2022, "DF")
#' }
#' @export
tse_votacao_municipio <- \(ano = NULL, uf, cargo = NULL, nr_votavel = NULL,
                            dest_dir = NULL) {
  anos <- if (is.null(ano)) tse_anos_disponiveis("todas") else
    sort(unique(as.integer(ano)))
  ufs <- setdiff(.tse_ufs(uf), "ZZ")
  chaves <- c("uf", "cod_municipio_tse", "municipio", "turno",
              "cargo", "nr_votavel", "nm_votavel")
  agregar <- \(d, a) {
    if (!"votos" %in% names(d)) {
      stop("tsebr: coluna 'votos' ausente em votacao_secao ", a,
           " — verifique o layout do ciclo (tse_layouts()$votacao_secao)")
    }
    ## filtros avaliados FORA do d[...]: dentro do [.data.table
    ## um simbolo como `cargo` resolve primeiro como COLUNA
    if (!is.null(cargo) && "cargo" %in% names(d)) {
      manter <- grepl(cargo, d$cargo, ignore.case = TRUE)
      d <- d[manter, , drop = FALSE]
    }
    if (!is.null(nr_votavel) && "nr_votavel" %in% names(d)) {
      manter <- d$nr_votavel == as.character(nr_votavel)
      d <- d[manter, , drop = FALSE]
    }
    mantidas <- intersect(chaves, names(d))
    d |>
      dplyr::select(dplyr::all_of(c(mantidas, "votos"))) |>
      dplyr::mutate(votos = as.numeric(votos),
                    cargo = if ("cargo" %in% names(d)) toupper(cargo)) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(mantidas))) |>
      dplyr::summarise(votos = sum(votos, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(ano = as.integer(a),
                    periodo = as.Date(paste0(a, "-12-31")))
  }
  puxar_uf <- \(a, sg) {
    message("tsebr: votacao_secao ", a, " ", sg)
    d <- tse_resultados_secao(a, sg, detalhe = FALSE, dest_dir = dest_dir)
    agregar(d, a)
  }
  ## presidente (2018+): zip BR nacional, lido uma vez por ano. A
  ## falha aqui e fatal (sem o BR o presidente some da serie)
  puxar_br <- \(a) {
    message("tsebr: votacao_secao ", a, " BR (presidente)")
    d <- tryCatch(tse_resultados_secao_br(a, ufs, dest_dir),
                  error = \(e) {
                    stop("tsebr: falha ao ler o zip BR (presidente) de ", a,
                         " — sem ele a votacao para presidente nao entra: ",
                         conditionMessage(e))
                  })
    agregar(d, a)
  }
  chunks <- Filter(Negate(is.null), unlist(
    lapply(anos, \(a) c(lapply(ufs, \(sg) puxar_uf(a, sg)), list(puxar_br(a)))),
    recursive = FALSE))
  data.table::rbindlist(chunks, fill = TRUE) |>
    tibble::as_tibble()
}

#' Votos nominais por municipio
#'
#' Agrega `votacao_secao` ao nivel municipal (codigo IBGE 7
#' digitos) usando `mapa` (de `tse_municipios()`). Sem
#' `nr_votavel`, devolve o total de votos nominais do cargo por
#' municipio; com `nr_votavel`, apenas aquele votavel. A leitura e
#' uma UF/ano por vez (ver [tse_votacao_municipio()]).
#'
#' @param ano Ano eleitoral ou vetor de anos; NULL = todas.
#' @param uf Sigla da UF.
#' @param cargo Regex de cargo (ex.: "PRESIDENTE"), opcional.
#' @param nr_votavel Numero do candidato/partido, opcional.
#' @param mapa `data.frame` com cod_municipio_tse e ibge
#'   (7 digitos); sem ele, `local` sai como codigo TSE (nao
#'   encaixa no DW beep).
#' @param dest_dir Diretorio de cache do download.
#' @return `data.frame` long: local, periodo, valor, uf.
#' @examples
#' \dontrun{
#' tse_resultados_municipio(2022, "DF", cargo = "PRESIDENTE",
#'                          nr_votavel = 13)
#' }
#' @export
tse_resultados_municipio <- \(ano = NULL, uf, cargo = NULL, nr_votavel = NULL,
                              mapa = NULL, dest_dir = NULL) {
  votos <- tse_votacao_municipio(ano = ano, uf = uf, cargo = cargo,
                                 nr_votavel = nr_votavel, dest_dir = dest_dir)
  if (is.null(mapa)) mapa <- .mapa_municipios_interno(sort(unique(votos$ano)), uf)
  .agregar_por_mapa(votos, mapa, "votos",
                    grupos = c("ibge", "periodo")) |>
    dplyr::rename(local = ibge) |>
    dplyr::mutate(uf = toupper(uf)) |>
    dplyr::select(local, periodo, valor, uf)
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
#' tse_detalhe_municipio(2022, "DF", metrica = "abstencoes")
#' }
#' @export
tse_detalhe_municipio <- \(ano = NULL, uf, metrica = c("aptos", "comparecimento",
                                                "abstencoes", "votos_nulos",
                                                "votos_brancos"),
                           mapa = NULL, dest_dir = NULL) {
  metrica <- match.arg(metrica)
  anos <- if (is.null(ano)) tse_anos_disponiveis("todas") else
    sort(unique(as.integer(ano)))
  puxar <- \(a) {
    message("tsebr: detalhe_votacao_secao ", a)
    d <- tse_resultados_secao(a, uf, detalhe = TRUE, dest_dir = dest_dir)
    if (!metrica %in% names(d)) {
      stop("tsebr: metrica '", metrica, "' ausente no detalhe ", a,
           " (colunas: ", paste(names(d), collapse = ", "), ")")
    }
    mantidas <- intersect(c("uf", "cod_municipio_tse"), names(d))
    agg <- d |>
      dplyr::select(dplyr::all_of(c(mantidas, metrica))) |>
      dplyr::mutate(valor = as.numeric(.data[[metrica]]),
                    .keep = "unused") |>
      dplyr::group_by(dplyr::across(dplyr::all_of(mantidas))) |>
      dplyr::summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(ano = as.integer(a),
                    periodo = as.Date(paste0(a, "-12-31")))
    agg
  }
  dados <- data.table::rbindlist(lapply(anos, puxar), fill = TRUE)
  if (is.null(mapa)) mapa <- .mapa_municipios_interno(sort(unique(dados$ano)), uf)
  .agregar_por_mapa(dados, mapa, "valor",
                    grupos = c("ibge", "periodo")) |>
    dplyr::rename(local = ibge) |>
    dplyr::mutate(uf = toupper(uf), metrica = metrica) |>
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
  puxar <- \(a) {
    d <- tse_prestacao(a, tipo = tipo, dest_dir = dest_dir)
    d$ano <- as.integer(a)
    d
  }
  dados <- data.table::rbindlist(lapply(anos, puxar), fill = TRUE)
  dados |>
    dplyr::group_by(uf, ano) |>
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

#' Mapa TSE x IBGE pela conexao padrao do DW (env vars)
#'
#' Encapsula [.mapa_municipios_interno()]: abre a conexao pelo
#' `.Renviron` (`user`/`password`/`host`/`dbname` do banco com a
#' tabela `local` do beep) e devolve o mapa
#' `cod_municipio_tse -> geoloc_id (IBGE 7d)` para o ano mais
#' recente pedido. Uso tipico: alimentar o `mapa` de consultas
#' rapidas do tsesqlr no painel beep.
#'
#' @param ano Ano eleitoral ou vetor (usa o maximo como referencia).
#' @param uf Sigla da UF ou "all".
#' @return `data.frame` com cod_municipio_tse, municipio, uf,
#'   geoloc_id e flag empate.
#' @examples
#' \dontrun{
#' mapa <- tse_mapa_municipios(2026, "DF")
#' }
#' @export
tse_mapa_municipios <- \(ano, uf) {
  anos <- sort(unique(as.integer(ano)))
  .mapa_municipios_interno(anos, uf)
}

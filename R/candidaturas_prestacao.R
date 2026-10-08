#' Lista de siglas de UF do TSE
#'
#' O TSE publica perfis por secao em 27 UFs mais o codigo "ZZ"
#' (eleitor no exterior).
#'
#' @param uf Sigla, vetor de siglas ou "all" (27 UFs + ZZ).
#' @return Vetor de siglas validado.
#' @keywords internal
.tse_ufs <- \(uf) {
  ufs <- c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO",
           "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR",
           "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO", "ZZ")
  if (identical(uf, "all")) return(ufs)
  uf <- toupper(uf)
  invalida <- setdiff(uf, ufs)
  if (length(invalida)) {
    stop("tsebr: UF invalida: ", paste(invalida, collapse = ", "))
  }
  uf
}

#' Candidaturas (consulta_cand)
#'
#' Baixa e le `consulta_cand_{ano}_{uf}.zip` (um zip por UF; o
#' recorte nacional e o conjunto das 27 UFs).
#'
#' @param ano Ano eleitoral.
#' @param uf Sigla da UF ou "all".
#' @param cargo Regex opcional de cargo (ex.: "PRESIDENTE",
#'   "GOVERNADOR", "DEPUTADO FEDERAL", case-insensitive).
#' @param dest_dir Diretorio de cache.
#' @return `data.frame` conformado pelo layout `consulta_cand`.
#' @examples
#' \dontrun{
#' cand <- tse_candidaturas(2026, uf = "DF", cargo = "GOVERNADOR")
#' }
#' @export
tse_candidaturas <- \(ano = NULL, uf, cargo = NULL, dest_dir = NULL) {
  assunto <- "consulta_cand"
  ufs <- .tse_ufs(uf)
  anos <- if (is.null(ano)) tse_anos_disponiveis("todas") else
    sort(unique(as.integer(ano)))
  puxar <- \(a) {
    arquivo <- sprintf("%s_%s.zip", assunto, a)
    zip <- tse_download(.tse_cdn_url(a, assunto, arquivo), dest_dir)
    conformar(tse_read(zip), tse_layouts()[[assunto]])
  }
  dados <- data.table::rbindlist(lapply(anos, puxar), fill = TRUE)
  if (!is.null(cargo)) {
    dados <- dados[grepl(cargo, dados$cargo, ignore.case = TRUE), ]
  }
  dados
}

#' Prestacao de contas eleitorais (receitas/despesas de candidato)
#'
#' Resolve a URL atual do zip no CKAN (`prestacao-de-contas-
#' eleitorais-{ano}`) e baixa/le o arquivo pedido. As URLs da
#' prestacao mudam de caminho na CDN a cada publicacao; passar
#' pelo CKAN evita hardcode.
#'
#' @param ano Ano eleitoral.
#' @param tipo `"receitas"` ou `"despesas"` (candidatos;
#'   orgaos partidarios exigem os datasets `*orgaos_partidarios*`).
#' @param uf Filtro opcional de sigla apos a leitura.
#' @param dest_dir Diretorio de cache.
#' @return `data.frame` conformado pelos layouts
#'   `receitas_candidato`/`despesas_candidato`.
#' @examples
#' \dontrun{
#' rec <- tse_prestacao(2026, tipo = "receitas", uf = "DF")
#' }
#' @export
tse_prestacao <- \(ano, tipo = c("receitas", "despesas"),
                   uf = NULL, dest_dir = NULL) {
  tipo <- match.arg(tipo)
  padroes <- c(
    receitas = sprintf("^receita_(candidato|documento)_%s(_[^_]+)?\\.zip$", ano),
    despesas = sprintf("^despesa_(candidato|documento)_%s(_[^_]+)?\\.zip$", ano))
  dataset <- tse_search(
    q = "", fq = sprintf('name:prestacao-de-contas-eleitorais-%s', ano))
  if (!nrow(dataset)) {
    stop("tsebr: dataset de prestacao de contas ", ano,
         " nao encontrado no CKAN")
  }
  recursos <- tse_show(dataset$name[1], padrao = padroes[[tipo]])
  if (!nrow(recursos)) {
    stop("tsebr: recurso ", tipo, " ", ano, " nao encontrado em ",
         dataset$name[1])
  }
  # prefere o nacional; cai no primeiro match (fatias por UF)
  pref <- grepl("_BR\\.zip$", recursos$url) |
    !grepl("_[A-Z]{2}_", recursos$url)
  alvo <- if (any(pref)) recursos$url[which(pref)[1]] else recursos$url[1]
  zip <- tse_download(alvo, dest_dir)
  layout <- tse_layouts()[[paste0(tipo, "_candidato")]]
  dados <- conformar(tse_read(zip), layout)
  if (!is.null(uf)) dados <- dados[dados$uf == toupper(uf), ]
  dados
}

#' Correspondencia municipios TSE x IBGE
#'
#' Os codigos de municipio do TSE (CD_MUNICIPIO/SG_UE) NAO sao
#' os do IBGE. Extrai a tabela de correspondencia a partir de
#' `consulta_cand` (codigo TSE + nome + UF) e cruza com a base
#' municipal do IBGE por nome normalizado + UF. Nao resolve
#' empates automaticamente: nomes repetidos na mesma UF saem
#' marcados para revisao.
#'
#' @param ano Ano eleitoral de referencia da candidaturas.
#' @param con Conexao DBI com banco que tenha a tabela municipal
#'   do IBGE (schema beep: `local` com `geoloc_id` 7 digitos e
#'   `local_name`); sem con, devolve apenas o lado TSE.
#' @param uf Sigla para reduzir volume.
#' @return `data.frame` com cod_municipio_tse, municipio, uf e,
#'   quando con informado, geoloc_id (IBGE) e flag `empate`.
#' @examples
#' \dontrun{
#' m <- tse_municipios(2026, uf = "DF")
#' }
#' @export
tse_municipios <- \(ano, con = NULL, uf = NULL) {
  cand <- tse_candidaturas(ano, uf = uf %||% "all")
  lado_tse <- dplyr::distinct(
    cand,
    cod_municipio_tse = as.character(cod_municipio_tse),
    municipio = as.character(municipio),
    uf = as.character(uf))
  if (is.null(con)) return(lado_tse)
  ibge <- DBI::dbGetQuery(con, paste(
    "SELECT geoloc_id, local_name FROM local",
    "WHERE length(geoloc_id::text) = 7",
    "AND (local_id < 5571 OR local_id > 7087)"))
  norm <- \(x) toupper(iconv(
    gsub("\\s+", " ", trimws(x)), to = "ASCII//TRANSLIT"))
  lado_tse$nome_norm <- norm(lado_tse$municipio)
  ibge$nome_norm <- norm(ibge$local_name)
  ibge$uf <- substr(as.character(ibge$geoloc_id), 1, 2)
  ibge$uf <- dplyr::recode(
    ibge$uf, "11" = "RO", "12" = "AC", "13" = "AM", "14" = "RR",
    "15" = "PA", "16" = "AP", "17" = "TO", "21" = "MA", "22" = "PI",
    "23" = "CE", "24" = "RN", "25" = "PB", "26" = "PE", "27" = "AL",
    "28" = "SE", "29" = "BA", "31" = "MG", "32" = "ES", "33" = "RJ",
    "35" = "SP", "41" = "PR", "42" = "SC", "43" = "RS", "50" = "MS",
    "51" = "MT", "52" = "GO", "53" = "DF")
  m <- dplyr::left_join(lado_tse, ibge, by = c("nome_norm", "uf"))
  n_por_chave <- dplyr::count(dplyr::distinct(m, nome_norm, uf, geoloc_id),
                              nome_norm, uf)
  m$empate <- m$nome_norm %in% n_por_chave$nome_norm[n_por_chave$n > 1] |
    duplicated(m[, c("nome_norm", "uf")])
  m[, c("cod_municipio_tse", "municipio", "uf",
        "geoloc_id", "empate")]
}

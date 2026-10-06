#' Cliente de baixo nivel da API CKAN do TSE
#'
#' Chama a API v3 do Portal de Dados Abertos do TSE
#' (`dadosabertos.tse.jus.br`). A pagina HTML do portal bloqueia
#' clientes automatizados (HTTP 403), mas a API CKAN e aberta.
#'
#' @param action Acao da API CKAN, sem o prefixo
#'   `api/3/action/` (ex.: `"package_search"`, `"package_show"`).
#' @param params Lista nomeada de parametros de consulta.
#' @param base_url URL base do portal CKAN.
#' @return Conteudo decodificado do campo `result` da resposta.
#' @examples
#' \dontrun{
#' res <- tse_ckan("package_show", list(id = "resultados-2026"))
#' }
#' @export
tse_ckan <- \(action, params = list(), base_url = "https://dadosabertos.tse.jus.br") {
  req <- httr2::request(base_url) |>
    httr2::req_url_path_append("api", "3", "action", action) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_user_agent("tsebr (https://github.com/DistintiveLab/tsebr)") |>
    httr2::req_timeout(60)
  resp <- tryCatch(
    httr2::req_perform(req),
    error = \(e) stop("tse_ckan: falha na chamada ", action, ": ",
                      conditionMessage(e))
  )
  body <- jsonlite::fromJSON(
    httr2::resp_body_string(resp),
    simplifyVector = FALSE)
  if (!isTRUE(body$success)) {
    stop("tse_ckan: API retornou erro para ", action,
         ": ", paste(body$error$message %||% "", collapse = "; "))
  }
  body$result
}

# operador de coalescencia para campos ausentes do JSON
`%||%` <- \(a, b) if (is.null(a)) b else a

#' Busca datasets no portal CKAN do TSE
#'
#' Envolve `package_search`. Prefira `q`/`fq` com a tag de ano
#' (`tags:"Ano 2026"`): a `package_list` do portal ja esteve com
#' cache desatualizado em 10/2026, omitindo datasets recem-criados.
#'
#' @param q Consulta livre (sintaxe Solr).
#' @param fq Filtro Solr (ex.: `tags:"Ano 2026"`).
#' @param rows Quantidade de resultados.
#' @return `data.frame` com name, title, notes, num_resources e
#'   metadata_modified dos datasets encontrados.
#' @examples
#' \dontrun{
#' tse_search(fq = 'tags:"Ano 2026"')
#' }
#' @export
tse_search <- \(q = "", fq = NULL, rows = 20L) {
  params <- list(q = q, rows = as.integer(rows))
  if (!is.null(fq)) params$fq <- fq
  res <- tse_ckan("package_search", params)
  pacotes <- res$results
  if (!length(pacotes)) return(tibble::tibble(name = character(0)))
  tibble::tibble(
    name = vapply(pacotes, \(p) p$name %||% NA_character_, character(1)),
    title = vapply(pacotes, \(p) p$title %||% NA_character_, character(1)),
    num_resources = vapply(pacotes, \(p) as.integer(p$num_resources %||% 0),
                           integer(1)),
    metadata_modified = vapply(
      pacotes, \(p) p$metadata_modified %||% NA_character_, character(1)))
}

#' Detalha um dataset do portal CKAN do TSE
#'
#' Envolve `package_show` e devolve os recursos (arquivos) do
#' dataset com nome e URL da CDN.
#'
#' @param id Nome (`name`) ou UUID do dataset
#'   (ex.: `"resultados-2026"`, `"eleitorado-2026"`).
#' @param padrao Regex opcional para filtrar recursos pelo nome
#'   (ex.: `"DF"`).
#' @return `data.frame` com name, url e tamanho dos recursos.
#' @examples
#' \dontrun{
#' tse_show("eleitorado-2026", padrao = "DF")
#' }
#' @export
tse_show <- \(id, padrao = NULL) {
  res <- tse_ckan("package_show", list(id = id))
  recs <- res$resources
  if (!length(recs)) return(tibble::tibble(name = character(0)))
  out <- tibble::tibble(
    name = vapply(recs, \(r) r$name %||% NA_character_, character(1)),
    url = vapply(recs, \(r) r$url %||% NA_character_, character(1)),
    size = vapply(recs, \(r) as.numeric(r$size %||% NA), numeric(1)))
  if (!is.null(padrao)) out <- out[grepl(padrao, out$name, ignore.case = TRUE), ]
  out
}

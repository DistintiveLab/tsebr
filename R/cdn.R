#' Monta a URL de um arquivo na CDN do TSE
#'
#' Os arquivos do portal de dados abertos sao servidos pela CDN
#' `cdn.tse.jus.br/estatistica/sead/`. O layout mudou entre
#' ciclos: ate 2024 os zips ficam sob `odsele/` (padrao usado
#' tambem pelo electionsBR); a partir de 2026, sob
#' `eleicoes/eleicoes{ano}/{assunto}/`.
#'
#' @param ano Ano eleitoral (numeric/character).
#' @param assunto Diretorio do assunto na CDN
#'   (ex.: `"votacao_secao"`, `"perfil_eleitor_secao"`,
#'   `"eleitorado_local_votacao"`, `"logsgedai"`, `"correspesp"`).
#' @param arquivo Nome do arquivo (ex.:
#'   `"votacao_secao_2022_DF.zip"`).
#' @return URL completa.
#' @keywords internal
.tse_cdn_url <- \(ano, assunto, arquivo) {
  ano <- as.integer(ano)
  raiz <- "https://cdn.tse.jus.br/estatistica/sead"
  if (ano >= 2026L) {
    paste(raiz, "eleicoes", paste0("eleicoes", ano), assunto, arquivo, sep = "/")
  } else {
    paste(raiz, "odsele", assunto, arquivo, sep = "/")
  }
}

#' Caminho do arquivo de hash SHA-512 publicado em par com um zip
#' @keywords internal
.tse_cdn_hash_url <- \(url_zip) paste0(url_zip, ".sha512")

#' Baixa e verifica um arquivo da CDN do TSE
#'
#' Baixa um zip da CDN do TSE para o diretorio de destino,
#' verificando (opcional, default) o hash SHA-512 publicado em
#' par pelo portal. A CDN nao exige autenticacao; o header
#' Referer do portal melhora a aceitacao de alguns caminhos.
#'
#' @param url_zip URL do zip na CDN (use `tse_show()` para
#'   resolver a partir do CKAN).
#' @param dest_dir Diretorio de destino; default e um cache do
#'   usuario (opcao `tsebr.cache_dir`).
#' @param verifica_hash Verificar o `.sha512` publicado? Sem
#'   verificacao se o par nao existir.
#' @param sobrescrever Rebaixar se o arquivo ja existir?
#' @return Caminho do arquivo baixado, invisivel.
#' @examples
#' \dontrun{
#' tse_download(.tse_cdn_url(2026, "eleitorado_local_votacao",
#'                           "eleitorado_local_votacao_2026.zip"))
#' }
#' @export
tse_download <- \(url_zip, dest_dir = NULL, verifica_hash = TRUE,
                  sobrescrever = FALSE) {
  if (is.null(dest_dir)) {
    dest_dir <- getOption("tsebr.cache_dir",
                          file.path(tools::R_user_dir("tsebr", "cache")))
  }
  dir.create(dest_dir, showWarnings = FALSE, recursive = TRUE)
  destino <- file.path(dest_dir, basename(url_zip))
  ## file.size > 0: download malogrado pode deixar arquivo vazio
  if (file.exists(destino) && file.size(destino) > 0 && !sobrescrever) {
    return(invisible(normalizePath(destino)))
  }
  tryCatch(
    utils::download.file(
      url_zip, destino, quiet = TRUE, mode = "wb", method = "libcurl",
      headers = c(
        Referer = "https://dadosabertos.tse.jus.br/",
        "User-Agent" = "tsebr (https://github.com/DistintiveLab/tsebr)")),
    error = \(e) stop("tse_download: falha ao baixar ", url_zip, ": ",
                      conditionMessage(e)))
  if (!file.exists(destino) || file.size(destino) == 0) {
    stop("tse_download: download vazio de ", url_zip)
  }
  if (verifica_hash) {
    hash_remoto <- tryCatch({
      arq <- tempfile(fileext = ".sha512")
      utils::download.file(.tse_cdn_hash_url(url_zip), arq, quiet = TRUE,
                           mode = "wb", method = "libcurl")
      trimws(readLines(arq, warn = FALSE)[1])
    }, error = \(e) NULL)
    if (!is.null(hash_remoto) && nzchar(hash_remoto)) {
      hash_local <- digest::digest(file = destino, algo = "sha512",
                                   serialize = FALSE)
      ref <- tolower(strsplit(trimws(hash_remoto), "\\s+")[[1]][1])
      if (!identical(tolower(hash_local), ref)) {
        stop("tse_download: hash SHA-512 divergente para ", url_zip)
      }
    }
  }
  invisible(normalizePath(destino))
}

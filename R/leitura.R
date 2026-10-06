#' Leitura dos CSVs eleitorais do TSE
#'
#' Os zips do TSE trazem um CSV por UF (ou nacional) em
#' **Latin-1/Windows-1252**, separador `;` e, em campos de
#' rendimento, decimal `,`. Nomes de colunas mudam entre anos:
#' a conformacao semantica deve ser feita por posicao/dicionario
#' proprio (ver `conformar()`), nao pelo nome cru.
#'
#' @param arquivo Caminho de um `.zip` (com um CSV dentro) ou de
#'   um `.csv` direto; ver `tse_download()`.
#' @param max_rows Limite de linhas para amostragem (NULL = tudo).
#' @return `data.table` com o conteudo bruto (colunas como no
#'   arquivo, nomes originais em caixa alta).
#' @examples
#' \dontrun{
#' d <- tse_read(tse_download(.tse_cdn_url(
#'   2026, "perfil_eleitor_secao", "perfil_eleitor_secao_2026_DF.zip")))
#' }
#' @export
tse_read <- \(arquivo, max_rows = NULL) {
  arquivo <- normalizePath(arquivo, mustWork = TRUE)
  if (tolower(tools::file_ext(arquivo)) == "zip") {
    pasta <- tempfile()
    dir.create(pasta)
    try(utils::unzip(arquivo, exdir = pasta), silent = TRUE)
    csvs <- list.files(pasta, pattern = "\\.(csv|txt)$",
                       ignore.case = TRUE, full.names = TRUE)
    if (!length(csvs)) {
      stop("tse_read: nenhum CSV dentro de ", basename(arquivo))
    }
    arquivo <- csvs[1]
  }
  dados <- data.table::fread(
    arquivo, encoding = "Latin-1", dec = ",", sep = "auto",
    showProgress = FALSE, nrows = max_rows %||% -1L)
  dados
}

#' Conforma colunas TSE por regex semanticos
#'
#' Renomeia colunas conforme padroes semanticos (nao por ordem
#' fixa, ainda): cada regra e um vetor `c(padrao = "novo_nome")`.
#' Dicionarios por familia de arquivo ficam em `tse_layouts()`.
#' Nomes mudam entre anos; revisar os padroes contra o
#' `leia-me` de cada ciclo.
#'
#' @param dados `data.table`/`data.frame` retornado por
#'   `tse_read()`.
#' @param layout Vetor nomeado `c(padrao_regex = "novo_nome")`.
#' @return `data.frame` com colunas renomeadas (as casadas).
#' @export
conformar <- \(dados, layout) {
  for (padrao in names(layout)) {
    alvo <- grep(padrao, names(dados), ignore.case = TRUE)
    if (length(alvo) == 1) {
      names(dados)[alvo] <- layout[[padrao]]
    }
  }
  dados
}

#' Dicionarios de conformacao por familia de arquivo
#'
#' Layouts semanticos iniciais (v1): nomes comuns a 2022/2024.
#' Nao cobre renomeacoes pontuais de cada ciclo; tratar caso a
#' caso contra o leia-me do zip.
#'
#' @return Lista nomeada por familia
#'   (`votacao_secao`, `detalhe_votacao_secao`,
#'   `perfil_eleitor_secao`, `eleitorado_local_votacao`,
#'   `consulta_cand`, `receitas_candidato`, `despesas_candidato`).
#' @export
tse_layouts <- \() {
  list(
    votacao_secao = c(
      "^SG_UF$" = "uf", "^CD_MUNICIPIO$" = "cod_municipio_tse",
      "^NM_MUNICIPIO$" = "municipio", "^NR_ZONA$" = "zona",
      "^NR_SECAO$" = "secao", "^NR_LOCAL_VOTACAO$" = "cod_local",
      "^NR_VOTAVEL$" = "nr_votavel", "^SQ_CANDIDATO$" = "sq_candidato",
      "^QT_VOTOS$" = "votos"),
    detalhe_votacao_secao = c(
      "^SG_UF$" = "uf", "^CD_MUNICIPIO$" = "cod_municipio_tse",
      "^NR_ZONA$" = "zona", "^NR_SECAO$" = "secao",
      "^QT_APTOS$" = "aptos", "^QT_COMPARECIMENTO$" = "comparecimento",
      "^QT_ABSTENCOES$" = "abstencoes",
      "^QT_VOTOS_NULOS$" = "votos_nulos",
      "^QT_VOTOS_BRANCOS$" = "votos_brancos"),
    perfil_eleitor_secao = c(
      "^SG_UF$" = "uf", "^CD_MUNICIPIO$" = "cod_municipio_tse",
      "^NR_ZONA$" = "zona", "^NR_SECAO$" = "secao",
      "^DS_FAIXA_ETARIA$" = "faixa_etaria", "^DS_GRAU_ESCOLARIDADE$" =
        "escolaridade", "^DS_GENERO$" = "genero",
      "^QT_ELEITORES_PERFIL$" = "eleitores",
      "^QT_ELEITORES_BIOMETRIA$" = "eleitores_biometria"),
    eleitorado_local_votacao = c(
      "^SG_UF$" = "uf", "^CD_MUNICIPIO$" = "cod_municipio_tse",
      "^NM_MUNICIPIO$" = "municipio", "^NR_ZONA$" = "zona",
      "^NR_SECAO$" = "secao", "^NM_LOCAL_VOTACAO$" = "local_votacao",
      "^DS_ENDERECO$" = "endereco", "^NM_BAIRRO$" = "bairro",
      "^NR_LATITUDE$" = "lat", "^NR_LONGITUDE$" = "lon",
      "^QT_ELEITOR_SECAO$" = "eleitores_secao"),
    consulta_cand = c(
      "^SG_UF$" = "uf", "^CD_MUNICIPIO$" = "cod_municipio_tse",
      "^SQ_CANDIDATO$" = "sq_candidato", "^NM_CANDIDATO$" = "nome",
      "^NM_URNA_CANDIDATO$" = "nome_urna", "^NR_CANDIDATO$" = "nr_candidato",
      "^SG_PARTIDO$" = "partido", "^DS_CARGO$" = "cargo",
      "^DS_SIT_TOT_TURNO$" = "situacao"),
    receitas_candidato = c(
      "^SG_UF$" = "uf", "^SQ_CANDIDATO$" = "sq_candidato",
      "^VR_RECEITA$" = "valor", "^DT_RECEITA$" = "data",
      "^NM_DOADOR_RFB$" = "doador", "^DS_ORIGEM_RECEITA$" = "origem"),
    despesas_candidato = c(
      "^SG_UF$" = "uf", "^SQ_CANDIDATO$" = "sq_candidato",
      "^VR_DESPESA_CONTRATADA$" = "valor",
      "^DT_DESPESA$" = "data",
      "^NM_FORNECEDOR_RFB$" = "fornecedor",
      "^DS_TIPO_DESPESA$" = "tipo"))
}

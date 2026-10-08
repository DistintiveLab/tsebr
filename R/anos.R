#' Anos eleitorais disponíveis por família
#'
#' Federais: 1998..2026 (de 4 em 4). Municipais: 1996..2024.
#' `"todas"` devolve a união ordenada (todo ano par do intervalo).
#'
#' @param tipo "federal", "municipal" ou "todas"
#' @export
tse_anos_disponiveis <- \(tipo = c("federal", "municipal", "todas")) {
  tipo <- match.arg(tipo)
  federais <- seq(1998, 2026, by = 4)
  municipais <- seq(1996, 2024, by = 4)
  switch(tipo,
         federal = federais,
         municipal = municipais,
         todas = sort(unique(c(federais, municipais))))
}

test_that(".norm_nome coloca TSE e IBGE na mesma grafia", {
  norm <- tsebr:::.norm_nome
  expect_identical(norm("OLHO D ÁGUA"), norm("Olho d'água"))
  expect_identical(norm("HERVAL D OESTE"), norm("Herval D'oeste"))
  expect_identical(norm("SANT ANA DO LIVRAMENTO"), norm("Sant'ana do Livramento"))
  expect_identical(norm("SEM PEIXE"), norm("Sem-Peixe"))
  expect_identical(norm("OLHOS D ÁGUA"), norm("Olhos-D'água"))
  expect_identical(norm("GOVERNADOR DIX-SEPT ROSADO"),
                   norm("Governador dix-Sept Rosado"))
})

test_that(".norm_nome normaliza acentos, colapsa espacos e decora", {
  norm <- tsebr:::.norm_nome
  expect_identical(norm("  GLÓRIA   D OESTE "), "GLORIA D OESTE")
  expect_identical(norm("Olho d'água Do Piauí"), "OLHO D AGUA DO PIAUI")
  expect_identical(norm("São João do Pau D'alho"), "SAO JOAO DO PAU D ALHO")
})

test_that("grafias equivalentes casam TSE cru com IBGE cru", {
  ## recode da grafia TSE crua normalizada precisa chegar exatamente
  ## na grafia IBGE crua normalizada
  norm <- tsebr:::.norm_nome
  eq <- tsebr:::.equivalentes_municipios()
  pares <- list(
    c("DISTRITO FEDERAL", "Brasília"),
    c("EMBU", "Embu das Artes"),
    c("MOJI MIRIM", "Mogi Mirim"),
    c("PARATI", "Paraty"),
    c("ALVORADA DO OESTE", "Alvorada D'oeste"),
    c("AMPARO DE SÃO FRANCISCO", "Amparo do São Francisco"),
    c("AREZ", "Arês"),
    c("ASSÚ", "Açu"),
    c("BARÃO DE MONTE ALTO", "Barão do Monte Alto"),
    c("BOA SAÚDE", "Januário Cicco"),
    c("CAMACÃ", "Camacan"),
    c("DONA EUSÉBIA", "Dona Euzébia"),
    c("ELDORADO DOS CARAJÁS", "Eldorado do Carajás"),
    c("ESPIGÃO DO OESTE", "Espigão D'oeste"),
    c("SANTA ISABEL DO PARÁ", "Santa Izabel do Pará"),
    c("SANTO ANTÔNIO DO LEVERGER", "Santo Antônio de Leverger"),
    c("SÃO LUÍS DO PARAITINGA", "São Luiz do Paraitinga"),
    c("SÃO THOMÉ DAS LETRAS", "São Tomé das Letras"))
  for (par in pares) {
    chave <- norm(par[1])
    expect_identical(unname(eq[chave]), norm(par[2]),
                     info = paste(par, collapse = " -> "))
  }
})

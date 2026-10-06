# AGENTS.md — tsebr

Cliente R para o Portal de Dados Abertos do TSE (CKAN + CDN).
Sucessor espiritual do electionsBR (CRAN 0.5.0, nov/2024, sem
cobertura 2026 e sem prestação de contas) — ver tabela de
antecessores no README. Consumidor principal: painel beep
(aba "Adicionar dados"). Textos e comentários em pt-BR.

## Comandos

```r
devtools::document()   # NAMESPACE + man/ (roxygen2, markdown)
devtools::test()       # testthat, edição 3
devtools::check()
```

## Contratos e gotchas (verificados em 06/10/2026)

- **API CKAN é aberta; o HTML do portal dá 403 para bots** —
  sempre `dadosabertos.tse.jus.br/api/3/action/...`.
  NÃO confiar em `package_list` (cache desatualizado em
  10/2026): descobrir datasets via `package_search` com
  `fq=tags:"Ano 2026"` ou `package_show?id=<nome>`.
- **CDN**: `cdn.tse.jus.br/estatistica/sead/`. Layout muda por
  ciclo: até 2024 em `odsele/`; 2026 em
  `eleicoes/eleicoes2026/<assunto>/`. Cada zip tem par
  `.sha512` — `tse_download()` verifica por padrão.
- **CSVs do TSE**: Latin-1, `;`, decimal `,` em campos de
  rendimento (`data.table::fread(encoding="Latin-1", dec=",")`).
- **Nomes de colunas mudam entre anos**: conformar por dicionário
  semântico (`tse_layouts()` + `conformar()`), nunca pelo nome
  cru; revisar contra o `leia-me.pdf` de cada ciclo.
- **Códigos TSE ≠ IBGE** para município: `tse_municipios()` cruza
  por nome normalizado + UF e marca `empate` para revisão; nunca
  cruzar pelo nome cru.
- **`resultados-2026` não publicado** em 06/10/2026
  (`package_show` → 404). Monitorar com `tse_search(fq =
  'tags:"Ano 2026"')`. Padrão esperado 2026 (igual 2022/2024):
  `detalhe_votacao_secao_2026.zip` nacional e/ou
  `votacao_secao_2026_{UF}.zip`.
- **Boletins de urna** (quando implementar `tse_boletins()`):
  endpoint fora do CKAN (`resultados.tse.jus.br/arquivo-urna/`),
  rate limit ≤ 85 req/s (bloqueio ~10 min acima de ~100);
  parser ASN.1/DER referência: projeto `ondedapraconversar`
  (sem LICENSE — usar apenas como referência de protocolo).
- Identificadores internos usam prefixo `.` (`.tse_cdn_url`,
  `.tse_ufs`): R não aceita identificadores começando com `_`.
- Estilo: pipe nativo `|>` e lambdas `\()`; roxygen com markdown;
  lazy helpers in `R/zzz.R` só setam opções (nada de conexões no
  `.onLoad` — diferente do beep, aqui tudo é stateless).

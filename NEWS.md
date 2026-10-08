# tsebr 0.0.2.9000

## Séries multi-anos (time-series)

- `tse_resultados_municipio()`, `tse_detalhe_municipio()`,
  `tse_prestacao_uf()` e `tse_candidaturas()` aceitam `ano = NULL`
  (default: **todas as eleições disponíveis**, 1996..2026) ou um
  vetor de anos — devolvendo a série completa empilhada com
  `periodo` por ano, pronta para o DW do beep.
- `tse_anos_disponiveis("federal"|"municipal"|"todas")` expõe os
  anos por família.


# tsebr 0.0.1.9000

- Novas agregações para o painel beep: `tse_resultados_municipio()`,
  `tse_detalhe_municipio()` (seção -> município via mapa TSE x IBGE)
  e `tse_prestacao_uf()` (totais por UF).
- `tse_municipios()` ganha `con` para resolução direta no DW beep.

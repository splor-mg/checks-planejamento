# Verificar quantidade de ações entre as bases

É esperado que haja a mesma quantidade de ações nas bases de ações e de
localizadores, ambas com a mesma descrição para cada ação.

## Usage

``` r
check_count_acoes(
  acoes_planejamento,
  localizadores_todos_planejamento,
  output = FALSE,
  stop_on_failure = FALSE,
  json_outfile = NULL,
  log_level = "ERROR",
  msg_template = NULL
)
```

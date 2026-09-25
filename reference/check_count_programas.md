# Verificar quantidade de programas entre as bases

É esperado que haja a mesma quantidade de programas nas bases de
programas, ações e de localizadores, todas com a mesma descrição para
cada programa.

## Usage

``` r
check_count_programas(
  programas_planejamento,
  acoes_planejamento,
  localizadores_todos_planejamento,
  output = FALSE,
  stop_on_failure = FALSE,
  json_outfile = NULL,
  log_level = "ERROR",
  msg_template = NULL
)
```

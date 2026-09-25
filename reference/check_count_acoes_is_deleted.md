# Verificar quantidade de ações excluídos entre as bases

Se, no processo de exclusão de ações que foram incluídos e excluídos
durante o mesmo ciclo (momento R), o mesmo não tiver sido realizado no
SIGPLAN, esse teste vai apontar problemas.

## Usage

``` r
check_count_acoes_is_deleted(
  acoes_planejamento,
  localizadores_todos_planejamento,
  output = FALSE,
  stop_on_failure = FALSE,
  json_outfile = NULL,
  log_level = "ERROR",
  msg_template = NULL
)
```

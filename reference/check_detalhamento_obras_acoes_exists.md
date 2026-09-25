# Verifica exclusão do detalhamento de obras de ações excluídas

Espera-se que todas as ações registradas como deletadas não tenham
valores registrados na base de detalhamento de obras.

## Usage

``` r
check_detalhamento_obras_acoes_exists(
  base_detalhamento_obras,
  acoes_planejamento,
  stop_on_failure = FALSE,
  output = FALSE,
  json_outfile = NULL,
  log_level = "ERROR",
  msg_template = NULL
)
```

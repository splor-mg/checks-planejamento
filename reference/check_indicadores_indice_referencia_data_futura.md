# Verifica se indicadores não possuem índice de referência com data futura

A data futura ocorre quando dt_apuracao \>= updated_at

## Usage

``` r
check_indicadores_indice_referencia_data_futura(
  indicadores_planejamento,
  output = FALSE,
  stop_on_failure = FALSE,
  json_outfile = NULL,
  log_level = "ERROR",
  msg_template = NULL
)
```

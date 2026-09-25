# Verifica se indicadores com previsões apuradas possuem previsões

Se a previsão do indicador não está em apuração, ele não pode estar
vazio.

## Usage

``` r
check_indicadores_previsoes_exists(
  indicadores_planejamento,
  output = FALSE,
  stop_on_failure = FALSE,
  json_outfile = NULL,
  log_level = "ERROR",
  msg_template = NULL
)
```

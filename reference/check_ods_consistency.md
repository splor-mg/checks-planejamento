# Verifica se existem programas com ODS 18 conjugado com outro ODS

Se a opção "ODS 18 - Não Possui ODS" foi selecionada, não faz sentido
que outro ODS também seja selecionado

## Usage

``` r
check_ods_consistency(
  programas_planejamento,
  output = FALSE,
  stop_on_failure = FALSE,
  json_outfile = NULL,
  log_level = "ERROR",
  msg_template = NULL
)
```

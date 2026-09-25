# Verifica se o número da obra siad possui valores duplicados por ação

Espera-se que haja um, e somente uma, obra (nº de obra) por ação em cada
unidade orçamentária.

## Usage

``` r
check_detalhamento_obras_numero_siad_duplicated(
  base_detalhamento_obras,
  stop_on_failure = FALSE,
  output = FALSE,
  json_outfile = NULL,
  log_level = "ERROR",
  msg_template = NULL
)
```

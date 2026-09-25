# Total do orçamento fiscal e investimento SIGPLAN vs SISOR

Verificar se o valor total do orçamento (orçamento fiscal e orçamento de
investimento das empresas controladas) coincide com a projeção do PPAG
para o ano seguinte

## Usage

``` r
check_valores_qdd_plurianual_invest(
  base_qdd_plurianual_invest,
  acoes_planejamento,
  stop_on_failure = FALSE,
  output = FALSE,
  json_outfile = NULL,
  log_level = "ERROR",
  msg_template = NULL
)
```

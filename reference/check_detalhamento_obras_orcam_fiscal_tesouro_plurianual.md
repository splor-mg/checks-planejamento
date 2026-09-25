# Consistência entre entre detalhamento de obras e QDD (Fiscal, Investimento, Plurianuais)

Detalhamento de Obras igual a QDD Fiscal - elemento de despesa 51 \| QDD
Invest - Cat. 4610 Detalhamento de Obras plurianual igual a Menor ou
Igual ao GND 44 - QDD FISCAL\*

## Usage

``` r
check_detalhamento_obras_orcam_fiscal_tesouro_plurianual(
  base_qdd_plurianual,
  base_detalhamento_obras,
  stop_on_failure = FALSE,
  output = FALSE,
  json_outfile = NULL,
  log_level = "ERROR",
  msg_template = NULL
)
```

# Receitas e Despesas Intra-Orcamentárias

Verifica se as operações intra-orçamentárias registradas na base
intraorçamentária de repasse estão adequadamente detalhadas no
orçamento.

## Usage

``` r
check_intra_despesa(
  base_orcam_despesa_item_fiscal,
  base_intra_orcamentaria_repasse,
  stop_on_failure = FALSE,
  output = FALSE,
  json_outfile = NULL,
  log_level = "ERROR",
  msg_template = NULL
)
```

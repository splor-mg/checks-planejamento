# Consistência entre "QDD Fiscal" e "Orçamento da Despesa Fiscal / Itens de Despesa"

Verificar se o valor total do orçamento é igual entre as bases "QDD
Fiscal" e "Orçamento da Despesa Fiscal / Itens de Despesa" com
agrupamento pelas classificações:

- orgão

- unidade orçamentária

- função

- subfunção

- programa

- projeto, atividade ou operação especial

- categoria econômica

- grupo de despesa

- modalidade de aplicação

- elemento de despesa

- identificador de ação governamental (IAG)

- fonte de recurso

- identificador de procedência e uso (IPU)

## Usage

``` r
check_valores_sisor(
  base_qdd_fiscal,
  base_orcam_despesa_item_fiscal,
  stop_on_failure = FALSE,
  output = FALSE,
  json_outfile = NULL,
  log_level = "ERROR",
  msg_template = NULL
)
```

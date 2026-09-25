# Verificar se os totais dos valores orçamentários e físicos de cada ano do PPAG estão consistentes entre as bases

Verificar se os totais dos valores orçamentários e físicos de cada ano
do PPAG são iguais entre as bases "Programas", "Ações" e "Localizadores"
com agrupamento pelas colunas:

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
check_valores_sigplan_programas(
  acoes_planejamento,
  programas_planejamento,
  stop_on_failure = FALSE,
  output = FALSE,
  json_outfile = NULL,
  log_level = "ERROR",
  msg_template = NULL
)
```

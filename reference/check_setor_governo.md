# Verificar se os setores de governo estão corretos

A referência correta dos setores de governo é disponibilizada pela DCPPN
via tabela auxiliar no recurso
[`volumes-loa-dados.desc_setor_governo`](https://github.com/splor-mg/volumes-loa-dados)

## Usage

``` r
check_setor_governo(
  acoes_planejamento,
  aux_setor_governo,
  output = FALSE,
  stop_on_failure = FALSE,
  json_outfile = NULL,
  log_level = "ERROR",
  msg_template = NULL
)
```

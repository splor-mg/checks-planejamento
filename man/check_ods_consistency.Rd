% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_ods_consistency.R
\name{check_ods_consistency}
\alias{check_ods_consistency}
\title{Verifica se existem programas com ODS 18 conjugado com outro ODS}
\usage{
check_ods_consistency(
  programas_planejamento,
  output = FALSE,
  stop_on_failure = FALSE,
  json_outfile = NULL,
  log_level = "ERROR",
  msg_template = NULL
)
}
\description{
Se a opção "ODS 18 - Não Possui ODS" foi selecionada, não faz sentido
que outro ODS também seja selecionado
}

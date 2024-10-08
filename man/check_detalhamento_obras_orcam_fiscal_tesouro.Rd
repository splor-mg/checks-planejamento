% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_detalhamento_obras_orcam_fiscal_tesouro.R
\name{check_detalhamento_obras_orcam_fiscal_tesouro}
\alias{check_detalhamento_obras_orcam_fiscal_tesouro}
\title{Consistência entre entre detalhamento de obras e QDD (Fiscal, Investimento, Plurianuais)}
\usage{
check_detalhamento_obras_orcam_fiscal_tesouro(
  base_qdd_fiscal,
  base_detalhamento_obras,
  stop_on_failure = FALSE,
  output = FALSE,
  json_outfile = NULL,
  log_level = "ERROR",
  msg_template = NULL
)
}
\description{
Detalhamento de Obras igual a QDD Fiscal - elemento de despesa 51 | QDD Invest - Cat. 4610
Detalhamento de Obras plurianual igual a Menor ou Igual ao GND 44 - QDD FISCAL*
}

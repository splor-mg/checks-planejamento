% Generated by roxygen2: do not edit by hand
% Please edit documentation in
%   R/check_indicadores_indice_referencia_data_futura.R
\name{check_indicadores_indice_referencia_data_futura}
\alias{check_indicadores_indice_referencia_data_futura}
\title{Verifica se indicadores não possuem índice de referência com data futura}
\usage{
check_indicadores_indice_referencia_data_futura(
  indicadores_planejamento,
  output = FALSE,
  stop_on_failure = FALSE
)
}
\description{
A data futura ocorre quando dt_apuracao >= updated_at
}

#' Verifica se indicadores não possuem índice de referência zerados
#'
#' 
#'
#'
#'
#' @export
check_indicadores_indice_referencia_zerado <- function(indicadores_planejamento,
                                                       output = FALSE,
                                                       stop_on_failure = FALSE,
                                                       json_outfile = NULL,
                                                       log_level = "ERROR",
                                                       msg_template = NULL
                                                       ) {
  df <- indicadores_planejamento |>
        filter(is_deleted_programa == FALSE &
               is_deleted_indicador == FALSE &
               is_em_apuracao_indice_de_referencia == FALSE
               )

  report <- check_that(df, indice_de_referencia != 0)
  
  default_message = "O programa {programa_cod} contém indicador ({paste(unlist(strsplit(indicador, ' '))[1:3], collapse = ' ')}...) que, apesar de não estar em apuração, está com índice de referência zerado."
  
  # prioritize the parameter error message if used
  msg_template = msg_template %||% default_message

  check_result(df, report,
               stop_on_failure = stop_on_failure,
               output = output,
               json_outfile = json_outfile,
               log_level = log_level,
               msg_template = msg_template
               )
}

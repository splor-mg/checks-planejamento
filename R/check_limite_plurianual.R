#' Valor Limite anual = Valor Utilizado + Valor Transferido
#'
#' O valor do limite anutal não levará em consideração o valor transferido
#' quando se tratar dos anos 1, 2 e 3 do plurianual.
#'
#'
#' @export
check_limite_plurianual <- function(base_limite_cota,
                                    stop_on_failure = FALSE,
                                    output = FALSE,
                                    json_outfile = NULL,
                                    log_level = "ERROR",
                                    msg_template = NULL
                                    ) {
  df <- base_limite_cota |>
        as_accounting()
  
  report <- check_that(df, 
                       vlr_limite_ano1 == vlr_utilizado_ano1,
                       vlr_limite_ano2 == vlr_utilizado_ano2,
                       vlr_limite_ano3 == vlr_utilizado_ano3
                       )
  
  default_message = "A uo {uo_cod} está com uma diferença de R$ {ifelse(vlr_limite_ano1 != vlr_utilizado_ano1, vlr_limite_ano1 - vlr_utilizado_ano1, ifelse(vlr_limite_ano2 != vlr_utilizado_ano2, vlr_limite_ano2 - vlr_utilizado_ano2, vlr_limite_ano3 - vlr_utilizado_ano3))} entre o valor de limite no ano {ifelse(vlr_limite_ano1 != vlr_utilizado_ano1, 1, ifelse(vlr_limite_ano2 != vlr_utilizado_ano2, 2, 3))} (R$ {ifelse(vlr_limite_ano1 != vlr_utilizado_ano1, vlr_limite_ano1, ifelse(vlr_limite_ano2 != vlr_utilizado_ano2, vlr_limite_ano2, vlr_limite_ano3))}) e o valor utilizado no mesmo ano (R$ {ifelse(vlr_limite_ano1 != vlr_utilizado_ano1, vlr_utilizado_ano1, ifelse(vlr_limite_ano2 != vlr_utilizado_ano2, vlr_utilizado_ano2, vlr_utilizado_ano3))})."
  
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

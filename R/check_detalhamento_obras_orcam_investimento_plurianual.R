#' Consistência entre entre detalhamento de obras e QDD (Fiscal, Investimento, Plurianuais)
#'
#' @description
#' Detalhamento de Obras igual a QDD Fiscal - elemento de despesa 51 | QDD Invest - Cat. 4610
#' Detalhamento de Obras plurianual igual a Menor ou Igual ao GND 44 - QDD FISCAL*
#'
#' @export
check_detalhamento_obras_orcam_investimento_plurianual <- function(base_qdd_plurianual_invest,
                                                                   base_detalhamento_obras,
                                                                   stop_on_failure = FALSE,
                                                                   output = FALSE,
                                                                   json_outfile = NULL,
                                                                   log_level = "ERROR",
                                                                   msg_template = NULL
                                                                   ) {
  key <- c("uo_cod",
           "funcao_cod",
           "subfuncao_cod",
           "programa_cod",
           "acao_cod",
           "iag_cod"
           )
  
  x <- base_qdd_plurianual_invest |> 
    aggregate("vlr_loa_desp_invest",
              by = key,
              filter = categoria == "4610"
              )
  
  y <- base_detalhamento_obras |> 
    aggregate("vlr_outros_ano",
              by = key,
              filter = str_sub(uo_cod, 1, 1) == 5
              )
  
  df <- merge(x, y,
              by = key,
              all = TRUE
              ) |>
     as_accounting(replace_missing = TRUE)

  report <- df |> check_that(vlr_loa_desp_invest_ano0 >= vlr_outros_ano0,
                             vlr_loa_desp_invest_ano1 >= vlr_outros_ano1,
                             vlr_loa_desp_invest_ano2 >= vlr_outros_ano2,
                             vlr_loa_desp_invest_ano3 >= vlr_outros_ano3
                             )
  
  default_message = default_message = paste0(
                    "A ação {acao_cod}, na UO {uo_cod}, funcional-programática {sprintf('%02d', funcao_cod)}.",
                    "{sprintf('%03d', subfuncao_cod)}.{sprintf('%03d', programa_cod)}, iag {iag_cod}, está com ",
                    "valor na base qdd-plurianual-investimento (R$ ",
                    "{ifelse(",
                    " vlr_loa_desp_invest_ano0 < vlr_outros_ano0, ",
                    " vlr_loa_desp_invest_ano0,",
                    " ifelse(",
                    "  vlr_loa_desp_invest_ano1 < vlr_outros_ano1, ",
                    "  vlr_loa_desp_invest_ano1, ",
                    "  ifelse(",
                    "   vlr_loa_desp_invest_ano2 < vlr_outros_ano2, ",
                    "   vlr_loa_desp_invest_ano2, ",
                    "   vlr_loa_desp_invest_ano3)))})", 
                    "menor do que na base detalhamento de obras (R$ ",
                    "{ifelse(",
                    "  vlr_loa_desp_invest_ano0 < vlr_outros_ano0, ",
                    "  vlr_outros_ano0, ",
                    "  ifelse(",
                    "   vlr_loa_desp_invest_ano1 < vlr_outros_ano1, ",
                    "   vlr_outros_ano1, ",
                    "   ifelse(",
                    "    vlr_loa_desp_invest_ano2 < vlr_outros_ano2, ",
                    "    vlr_outros_ano2, ",
                    "    vlr_outros_ano3)))}",
                    ") para o ano ",
                    "{ifelse(",
                    "  vlr_loa_desp_invest_ano0 < vlr_outros_ano0, ",
                    "  0, ",
                    "  ifelse(",
                    "   vlr_loa_desp_invest_ano1 < vlr_outros_ano1, ",
                    "   1, ",
                    "   ifelse(",
                    "    vlr_loa_desp_invest_ano2 < vlr_outros_ano2, ",
                    "    2, ",
                    "    3)))}",
                    "."
                    )
  
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

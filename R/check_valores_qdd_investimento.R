#' Total do orçamento fiscal e investimento SIGPLAN vs SISOR
#'
#' Verificar se o valor total do orçamento (orçamento fiscal e orçamento de
#' investimento das empresas controladas) coincide com a projeção do PPAG
#' para o ano seguinte
#'
#' @export
check_valores_qdd_investimento <- function(base_qdd_investimento,
                                           acoes_planejamento,
                                           stop_on_failure = FALSE,
                                           output = FALSE,
                                           json_outfile = NULL,
                                           log_level = "ERROR",
                                           msg_template = NULL
                                           ) {

  key <- c("uo_cod",
           "programa_cod",
           "acao_cod",
           "funcao_cod",
           "subfuncao_cod",
           "iag_cod"
           )

  x <- base_qdd_investimento |>
       aggregate("vlr_loa_desp_invest",
                 by = key
                 )

  y <- acoes_planejamento |>
       aggregate("vr_meta_orcamentaria_ano0",
                 by = key,
                 filter = is_deleted_acao == FALSE &
                          identificador_tipo_acao_cod %in% c(3, 6, 8),
                 rename = list(uo_acao_cod = "uo_cod")
                 )

  df <- merge(x, y,
              by = key,
              all = TRUE
              ) |>
        as_accounting()

  report <- check_that(df, vlr_loa_desp_invest == vr_meta_orcamentaria_ano0)
  
  default_message = "A uo {uo_cod}, programa {programa_cod}, ação {acao_cod}, função {funcao_cod}, subfunção {subfuncao_cod} está com valor na base qdd-investimento (R$ {vlr_loa_desp_invest}) diferente da base ações-planejamento (R$ {vr_meta_orcamentaria_ano0}). Diferença de R$ {ifelse(is.na(vlr_loa_desp_invest) & is.na(vr_meta_orcamentaria_ano0), 'NA', ifelse(is.na(vlr_loa_desp_invest), vr_meta_orcamentaria_ano0, ifelse(is.na(vr_meta_orcamentaria_ano0), vlr_loa_desp_invest, vlr_loa_desp_invest - vr_meta_orcamentaria_ano0)))}."
  
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

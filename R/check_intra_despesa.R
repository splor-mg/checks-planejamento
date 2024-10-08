#' Receitas e Despesas Intra-Orcamentárias
#'
#' Verifica se as operações intra-orçamentárias registradas na
#' base intraorçamentária de repasse estão adequadamente detalhadas no orçamento.
#'
#'
#' @export
check_intra_despesa <- function(base_orcam_despesa_item_fiscal,
                                base_intra_orcamentaria_repasse,
                                stop_on_failure = FALSE,
                                output = FALSE,
                                json_outfile = NULL,
                                log_level = "ERROR",
                                msg_template = NULL
                                ) {
  key <- c("uo_cod",
           "identificador_tipo_acao_cod",
           "projeto_atividade_cod",
           "grupo_cod",
           "modalidade_cod",
           "elemento_cod",
           "item_cod"
           )

  x <- base_orcam_despesa_item_fiscal |>
       aggregate("vlr_loa_desp$",
                 by = key,
                 filter = modalidade_cod == 91
                 )

  y <- base_intra_orcamentaria_repasse |>
       separate(programa_trabalho_fmt,
                sep = "\u00A0",
                convert = TRUE,
                into = c(NA, NA, NA,
                         "funcao_cod",
                         "subfuncao_cod",
                         "programa_cod",
                         "identificador_tipo_acao_cod",
                         "projeto_atividade_cod",
                         NA
                         )
                ) |>
       separate(natureza_desp_fmt,
                sep = "\u00A0",
                convert = TRUE,
                into = c("categoria_cod",
                         "grupo_cod",
                         "modalidade_cod",
                         "elemento_cod",
                         "item_cod"
                         )
                ) |>
       aggregate("vlr_repassado",
                 by = key,
                 rename = list(uo_repassadora_cod = "uo_cod")
                 )

  df <- merge(x, y,
              by = key,
              all = TRUE
              ) |>
        as_accounting()
  
  report <- check_that(df, vlr_loa_desp == vlr_repassado)
  
  default_message = "A uo {uo_cod}, na ação {identificador_tipo_acao_cod}{sprintf('%03d', projeto_atividade_cod)}, grupo {grupo_cod}, elemento-item {sprintf('%02d', elemento_cod)}{sprintf('%02d', item_cod)}, possui inconsistência nos valores da despesa intraorçamentária (modalidade 91) da base despesa-item fiscal (R$ {vlr_loa_desp}) e da base intra-repasse (R$ {vlr_repassado})."
  
  # prioritize the parameter error message if used
  msg_template = msg_template %||% default_message
  
  check_result(df, report,
               stop_on_failure = stop_on_failure,
               output = output,
               summary = aggregate(df, "vlr"),
               json_outfile = json_outfile,
               log_level = log_level,
               msg_template = msg_template
               )
}

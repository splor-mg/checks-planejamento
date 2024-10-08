#' Verifica se o detalhamento do quantitativo de inativos civis foi realizado
#'
#' O detalhamento é obrigatório para ações orçamentárias que apresentam valores 
#' superiores a R$ 1.000 nas ações do FFP.
#'
#'
#' @export
check_detalhamento_pessoal_inativo_civil <- function(base_categoria_pessoal, 
                                                     base_qdd_fiscal, 
                                                     uo_acao_inativo_civil, 
                                                     stop_on_failure = FALSE, 
                                                     output = FALSE,
                                                     json_outfile = NULL,
                                                     log_level = "ERROR",
                                                     msg_template = NULL
                                                     ) {
  
  x <- base_qdd_fiscal |>
       aggregate("vlr_loa_desp$", 
                 by = c("uo_cod", "acao_cod"),
                 filter = (uo_cod == 4711 &
                           acao_cod %notin% c(7004, 7008, 7016, 7023)
                           ) |
                           ipu_cod == 5)
  
  y <- uo_acao_inativo_civil |> 
       rename(acao_cod = acao,
              uo_inativo_cod = cod_uo
              ) |> 
       mutate(uo_cod = 4711) |> 
       select(uo_cod,
              acao_cod,
              uo_inativo_cod
              )

  z <- base_categoria_pessoal |>
       filter(categoria == "INATIVO CIVIL" &
              quantidade > 0 &
              !is.na(quantidade)
              ) |> 
       separate(col = "uo_cod_sigla",
                sep = " - ",
                into = c("uo_cod", "uo_sigla"),
                convert = TRUE
                ) |> 
       select(uo_cod,
              uo_sigla,
              quantidade
              )
      
  df <- merge(x, y,
              all.x = TRUE,
              by = c("uo_cod", "acao_cod")
              ) |> 
        mutate(uo_inativo_cod = ifelse(acao_cod == 7006,
                                       uo_cod,
                                       uo_inativo_cod
                                       )
               ) |> 
        merge(z,
              by.x = "uo_inativo_cod",
              by.y = "uo_cod",
              all = TRUE
              )
  
  report <- check_that(df,
                      if (vlr_loa_desp > 1000)
                          !is.na(quantidade) &
                          quantidade > 0
                          )
  
  default_message = paste0(
                      "A ação {acao_cod}, na uo {uo_cod}, possui valor na base qdd-fiscal superior ",
                      "a R$ 1.000,00, porém não contém detalhamento de quantitativo de inativos ",
                      "civis na base categoria-pessoal."
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

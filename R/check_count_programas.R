#' Verificar quantidade de programas entre as bases
#'
#' É esperado que haja a mesma quantidade de programas nas bases de programas, 
#' ações e de localizadores, todas com a mesma descrição para cada programa.
#'
#'
#' @export
check_count_programas <- function(programas_planejamento,
                                  acoes_planejamento,
                                  localizadores_todos_planejamento,
                                  output = FALSE,
                                  stop_on_failure = FALSE,
                                  json_outfile = NULL,
                                  log_level = "ERROR",
                                  msg_template = NULL) {
  x <- programas_planejamento |>
    filter(is_deleted_programa == FALSE) |>
    distinct(programa_cod,
             programa_desc
             ) |>
    rename(programas = programa_desc)

  y <- acoes_planejamento |>
    filter(is_deleted_programa == FALSE) |>
    distinct(programa_cod,
             programa_desc
             ) |>
    rename(acoes = programa_desc)

  z <- localizadores_todos_planejamento |>
    filter(is_deleted_programa == FALSE) |>
    distinct(programa_cod,
             programa_desc
             ) |>
    rename(localizadores = programa_desc)

  df <- merge(x, y, by = "programa_cod",
                    all = TRUE
                    ) |>
        merge(z, by = "programa_cod",
                 all = TRUE)

  report <- check_that(df, programas == acoes,
                           acoes == localizadores)
  
  default_message =   "O programa {programa_cod} {ifelse(
                      is.na(acoes) & is.na(localizadores), 
                      'consta na base programas, porém não nas bases ações-planejamento e localizadores.', 
                      ifelse(
                        is.na(programas) & is.na(localizadores), 
                        'consta na base ações-planejamento, porém não nas bases programas e localizadores.', 
                        ifelse(
                          is.na(programas) & is.na(acoes), 
                          'consta na base localizadores, porém não nas bases programas e ações-planejamento.', 
                          ifelse(
                            is.na(programas), 
                            'não consta na base programas, em detrimento das bases ações-planejamento e localizadores.', 
                            ifelse(
                              is.na(acoes), 
                              'não consta na ações-planejamento, em detrimento das bases programas e localizadores', 
                              ifelse(
                                is.na(localizadores), 
                                'não consta na base localizadores, em detrimento das bases programas e ações-planejamento', 
                                paste('apresenta descrição inconsistente entre as bases programas (',
                                      paste(unlist(strsplit(programas, ' '))[1:2], collapse = ' '),
                                      '...), ações (',
                                      paste(unlist(strsplit(acoes, ' '))[1:2], collapse = ' '),
                                      '...) e localizadores (',
                                      paste(unlist(strsplit(localizadores, ' '))[1:2], collapse = ' '),
                                      ').'
                                      )
                              )
                            )
                          )
                        )
                      )
                    )
                  }"
  
  # prioritize the parameter error message if used
  msg_template = msg_template %||% default_message

  check_result(df, report,
               stop_on_failure = stop_on_failure,
               output = output,
               summary = count(df),
               json_outfile = json_outfile,
               log_level = log_level,
               msg_template = msg_template
  )
}

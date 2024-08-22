#' Verificar quantidade de programas excluídos entre as bases
#'
#' Se o processo de exclusão de programas que foram incluídos e excluídos
#' durante o mesmo ciclo (momento R) não tiver sido realizado no SIGPLAN
#' esse teste vai apontar problemas.
#'
#' @export
check_count_programas_is_deleted <- function(programas_planejamento,
                                             acoes_planejamento,
                                             localizadores_todos_planejamento,
                                             output = FALSE,
                                             stop_on_failure = FALSE,
                                             json_outfile = NULL, log_level = "ERROR",
                                             msg_template = NULL) {
  x <- programas_planejamento |>
    filter(is_deleted_programa == TRUE) |>
    distinct(programa_cod, programa_desc) |>
    rename(programas = programa_desc)

  y <- acoes_planejamento |>
    filter(is_deleted_programa == TRUE) |>
    distinct(programa_cod, programa_desc) |>
    rename(acoes = programa_desc)

  z <- localizadores_todos_planejamento |>
    filter(is_deleted_programa == TRUE) |>
    distinct(programa_cod, programa_desc) |>
    rename(localizadores = programa_desc)

  df <- merge(x, y, by = "programa_cod", all = TRUE) |>
    merge(z, by = "programa_cod", all = TRUE)

  report <- check_that(df, programas == acoes, acoes == localizadores)
  
  default_message = "Foram encontrados erros no teste."
  
  # prioritize the parameter error message if used
  msg_template = msg_template %||% default_message

  check_result(
    df, report,
    stop_on_failure = stop_on_failure, output = output, summary = count(df), 
    json_outfile = json_outfile, log_level = log_level, msg_template = msg_template
  )
}
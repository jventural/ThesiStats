#' Format Likert Response Options as a Single String
#'
#' Turns a data frame of Likert response options and their scores into one
#' string of the form `"0. Option; 1. Option; ..."`, ordered by score. This is
#' the format read by [remplace_alternative_response()].
#'
#' @param df A data frame with the columns `Alternativas` (response options)
#'   and `score` (their numeric values), such as the output of
#'   [detect_expression_Likert()].
#'
#' @return A character string.
#' @export
#' @examples
#' likert <- data.frame(
#'   Alternativas = c("Nunca", "A veces", "Siempre"),
#'   score = c(0, 1, 2)
#' )
#' convert_to_expresions(likert)
convert_to_expresions <- function(df) {
  if (!all(c("Alternativas", "score") %in% colnames(df))) {
    stop("El dataframe debe contener las columnas 'Alternativas' y 'score'.")
  }

  df %>%
    arrange(score) %>%
    mutate(combined = paste0(score, ". ", Alternativas)) %>%
    pull(combined) %>%
    paste(collapse = "; ")
}

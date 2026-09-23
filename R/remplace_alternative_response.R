#' Replace Likert Text Responses with Their Scores
#'
#' Converts text responses into numbers in one or more blocks of item columns.
#' Each block is described by its first and last column and by a string that
#' maps each response option to its score (`"0. Nunca; 1. A veces; ..."`, the
#' format returned by [convert_to_expresions()]).
#'
#' @param df A data frame.
#' @param columnas_valores_entradas A list of blocks. Each block is a list
#'   with two elements: a character vector `c(first_column, last_column)`
#'   (for example `c("ANS1", "ANS9")`) and the mapping string.
#'
#' @return `df` with the responses of the listed columns replaced by their
#'   scores; responses not found in the mapping become `NA`.
#' @export
#' @examples
#' df <- data.frame(P1 = c("Nunca", "Siempre"), P2 = c("A veces", "Nunca"))
#' remplace_alternative_response(
#'   df, list(list(c("P1", "P2"), "0. Nunca; 1. A veces; 2. Siempre"))
#' )
remplace_alternative_response <- function(df, columnas_valores_entradas) {
  convertir_string_a_lista <- function(input_string) {
    input_string <- gsub("\n", "; ", input_string)
    lineas <- trimws(strsplit(input_string, "; ")[[1]])
    lineas <- lineas[lineas != ""]
    textos <- sub("^[0-9]+\\.\\s*", "", lineas)
    numeros <- as.integer(sub("\\..*$", "", lineas))
    list(textos, numeros)
  }

  transformar <- function(x, valores_numeros) {
    indices <- match(x, valores_numeros[[1]])
    ifelse(is.na(indices), NA, valores_numeros[[2]][indices])
  }

  for (col_val_num in columnas_valores_entradas) {
    valores_numeros <- convertir_string_a_lista(col_val_num[[2]])
    inicio <- as.numeric(sub("\\D+", "", col_val_num[[1]][1]))
    final <- as.numeric(sub("\\D+", "", col_val_num[[1]][2]))
    prefijo <- gsub("[0-9]", "", col_val_num[[1]][1])

    column_names <- paste0(prefijo, inicio:final)

    df <- df %>%
      mutate(across(all_of(column_names), ~ transformar(., valores_numeros)))
  }

  df
}

#' Detect and Score Several Blocks of Likert Items
#'
#' For each block of items, detects the response options with
#' [detect_expression_Likert()], builds the score mapping with
#' [convert_to_expresions()] and replaces the text responses by their scores
#' with [remplace_alternative_response()].
#'
#' @param df A data frame.
#' @param specs A list of blocks. Each block is a list with `prefix` (item
#'   prefix, for example `"ANS"`), `n_items` (number of items, named
#'   `prefix1` to `prefixN`), `levels` (response options in ascending order)
#'   and `start_zero` (logical, scores start at 0 or 1).
#'
#' @return `df` with the items of every block scored.
#' @export
#' @examples
#' df <- data.frame(ANS1 = c("Nunca", "Siempre", "A veces"),
#'                  ANS2 = c("A veces", "Nunca", "Siempre"))
#' process_likert_blocks(df, list(
#'   list(prefix = "ANS", n_items = 2, start_zero = TRUE,
#'        levels = c("Nunca", "A veces", "Siempre"))
#' ))
process_likert_blocks <- function(df, specs) {
  exprs_list <- lapply(specs, function(spec) {
    cols <- paste0(spec$prefix, seq_len(spec$n_items))
    missing_cols <- setdiff(cols, names(df))
    if (length(missing_cols) > 0) {
      stop("Columnas faltantes en df: ", paste(missing_cols, collapse = ", "))
    }
    df %>%
      select(all_of(cols)) %>%
      detect_expression_Likert(
        start_zero    = spec$start_zero,
        likert_levels = spec$levels
      ) %>%
      convert_to_expresions()
  })

  column_mappings <- Map(function(spec, expr_df) {
    list(c(paste0(spec$prefix, "1"), paste0(spec$prefix, spec$n_items)), expr_df)
  }, specs, exprs_list)

  remplace_alternative_response(df, column_mappings)
}

#' Generate dplyr Code that Adds Factor Sum Scores
#'
#' Reads text with one line per factor (`"Factor: Item1, Item2, ..."`) and
#' returns, as a string, the dplyr code that adds the row sum of each factor
#' to a data frame. The code can be printed with [cat()] and pasted into a
#' script.
#'
#' @param text A single string with one line per factor, separated by `"\n"`.
#' @param name Name of the data frame used in the generated code.
#'
#' @return A character string with R code.
#' @export
#' @examples
#' codigo <- generate_code("Ansiedad: A1, A2, A3\nEstres: B1, B2", name = "datos")
#' cat(codigo)
generate_code <- function(text, name = "df_new_renombrado") {
  lines <- unlist(strsplit(text, "\n"))

  factors <- gsub(":.+", "", lines)
  items <- gsub(".+:", "", lines)
  items <- lapply(items, function(x) unlist(strsplit(trimws(x), ", ")))

  code <- paste0(name, " <- ", name, " %>% \n  rowwise() %>% \n  mutate(")

  for (i in seq_along(factors)) {
    factor <- factors[i]
    if (grepl(" ", factor)) {
      factor <- paste0("`", factor, "`")
    }
    item_list <- paste(items[[i]], collapse = ",")
    code <- paste0(code, factor, " = sum(c_across(c(", item_list, "))),\n         ")
  }

  paste0(substr(code, 1, nchar(code) - 11), ") %>% \n  ungroup()")
}

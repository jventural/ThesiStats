#' Extract the Items of Each Factor from Text Lines
#'
#' Reads lines of the form `"Factor: Item1, Item2, Item3"` and returns a named
#' list with the items of each factor.
#'
#' @param text Character vector, one line per factor.
#' @param prefix Optional text inserted in the item names between an
#'   uppercase letter and the lowercase letter that follows it. Defaults to
#'   `""` (names unchanged).
#'
#' @return A named list: one character vector of item names per factor.
#' @export
#' @examples
#' extract_items(c("Ansiedad: ANS1, ANS2, ANS3", "Depresion: DEP1, DEP2"))
extract_items <- function(text, prefix = "") {
  extracted <- list()
  for (line in text) {
    parts <- strsplit(line, ": ")[[1]]
    tag <- parts[1]
    values <- unlist(strsplit(parts[-1], ", "))
    values <- gsub("(?<=[A-Z])(?=[a-z])", prefix, values, perl = TRUE)
    extracted[[tag]] <- values
  }
  extracted
}

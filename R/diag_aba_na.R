#' Lower Triangular Format for a Correlation Matrix
#'
#' Numbers the rows (`"1. Name"`) and columns (`1`, `2`, ...) of a square
#' matrix, puts `"-"` on the diagonal and `NA` above it, the usual layout of a
#' correlation table in a thesis.
#'
#' @param matriz A square matrix with row names.
#'
#' @return A character matrix with the lower triangle of `matriz`.
#' @export
#' @examples
#' m <- round(cor(mtcars[, 1:4]), 2)
#' diag_aba_na(m)
diag_aba_na <- function(matriz) {
  encabezados_filas_originales <- rownames(matriz)
  colnames(matriz) <- seq_len(ncol(matriz))
  rownames(matriz) <- paste0(seq_along(encabezados_filas_originales), ". ",
                             encabezados_filas_originales)
  diag(matriz) <- "-"
  matriz[upper.tri(matriz)] <- NA
  matriz
}

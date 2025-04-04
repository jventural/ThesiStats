rename_items_only <- function(df, prefix1 = "COPE", inicio = NULL, final = NULL, n_items1 = NULL) {
  if (!is.null(inicio)) {
    inici_idx <- which(colnames(df) == inicio)
  } else {
    inici_idx <- 1
  }

  if (!is.null(final)) {
    final_idx <- which(colnames(df) == final)
  } else {
    final_idx <- ncol(df)
  }

  n_vars <- final_idx - inici_idx + 1

  if (is.null(n_items1)) {
    n_items1 <- n_vars
  }

  if (n_items1 != n_vars) {
    stop("El número de columnas a renombrar no coincide con el número de nombres nuevos.")
  }

  nombres1 <- paste0(prefix1, 1:n_items1)
  colnames(df)[inici_idx:final_idx] <- nombres1

  return(df)
}

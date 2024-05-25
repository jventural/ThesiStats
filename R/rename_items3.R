rename_items3 <- function(df, prefix1 = "COPE", prefix2 = "E", prefix3 = "F", inicio = NULL, final = NULL, n_items1 = NULL, n_items2 = NULL, n_items3 = NULL) {
  # Obtener los índices de las variables a renombrar
  if (!is.null(inicio)) {
    inicio_idx <- which(colnames(df) == inicio)
  } else {
    inicio_idx <- 1
  }

  if (!is.null(final)) {
    final_idx <- which(colnames(df) == final)
  } else {
    final_idx <- ncol(df)
  }

  n_vars <- final_idx - inicio_idx + 1

  # Distribuir las columnas entre los tres grupos de prefijos si no se especifican cantidades
  if (is.null(n_items1) && is.null(n_items2) && is.null(n_items3)) {
    n_items1 <- ceiling(n_vars / 3)
    n_items2 <- ceiling((n_vars - n_items1) / 2)
    n_items3 <- n_vars - n_items1 - n_items2
  } else if (!is.null(n_items1) && is.null(n_items2) && is.null(n_items3)) {
    n_items2 <- ceiling((n_vars - n_items1) / 2)
    n_items3 <- n_vars - n_items1 - n_items2
  }

  # Crear los vectores de nombres para cada prefijo
  nombres1 <- paste0(prefix1, 1:n_items1)
  nombres2 <- paste0(prefix2, 1:n_items2)
  nombres3 <- paste0(prefix3, 1:n_items3)

  # Concatenar los vectores de nombres
  nuevos_nombres <- c(nombres1, nombres2, nombres3)

  # Verificar que el número de columnas a renombrar coincide con el número de nombres nuevos
  if (n_vars != length(nuevos_nombres)) {
    stop("El número de columnas a renombrar no coincide con el número de nombres nuevos.")
  }

  # Verificar que el número de columnas a renombrar es igual al número de columnas que existen
  if (final_idx > ncol(df)) {
    stop("El número de columnas a renombrar es mayor que el número de columnas que existen.")
  }

  # Renombrar las variables del data frame
  colnames(df)[inicio_idx:final_idx] <- nuevos_nombres

  # Retornar el data frame con las variables renombradas
  return(df)
}

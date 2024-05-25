remplace_alternative_response <- function(df, columnas_valores_entradas) {

  # Función interna para convertir un string en listas de textos y números
  convertir_string_a_lista <- function(input_string) {
    # Cambiar los saltos de línea por comas si están presentes
    input_string <- gsub("\n", ", ", input_string)
    # Dividir el string modificado en elementos
    lineas <- strsplit(input_string, ", ")[[1]]
    lineas <- trimws(lineas)
    lineas <- lineas[lineas != ""]
    textos <- sub("^[0-9]+\\.\\s*", "", lineas)
    numeros <- as.integer(sub("\\..*$", "", lineas))
    list(textos, numeros)
  }

  # Función interna para transformar una columna según los valores y números dados
  transformar <- function(x, valores_numeros) {
    indices <- match(x, valores_numeros[[1]])
    ifelse(is.na(indices), NA, valores_numeros[[2]][indices])
  }

  # Aplicar la transformación a todas las columnas especificadas
  for (col_val_num in columnas_valores_entradas) {
    valores_numeros <- convertir_string_a_lista(col_val_num[[2]])
    inicio <- as.numeric(sub("\\D+", "", col_val_num[[1]][1]))  # Extrae el número inicial
    final <- as.numeric(sub("\\D+", "", col_val_num[[1]][2]))  # Extrae el número final
    prefijo <- gsub("[0-9]", "", col_val_num[[1]][1])          # Extrae el prefijo de las columnas

    column_names <- paste0(prefijo, inicio:final)              # Crea los nombres de las columnas

    df <- df %>%
      dplyr::mutate(across(all_of(column_names), ~ transformar(., valores_numeros)))
  }

  return(df)
}

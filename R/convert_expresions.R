convert_expresions <- function(df) {
  # Verificar si el dataframe tiene la columna 'Alternativas'
  if (!"Alternativas" %in% names(df)) {
    stop("El dataframe no contiene una columna llamada 'Alternativas'.")
  }

  # Usar la función `paste` para concatenar los valores junto con su índice
  expresiones_formateadas <- paste(seq_along(df$Alternativas), df$Alternativas, sep=". ", collapse="; ")

  return(expresiones_formateadas)
}

convert_to_expresions <- function(df) {
  # Verificar que el dataframe tenga las columnas necesarias
  if (!all(c("Alternativas", "score") %in% colnames(df))) {
    stop("El dataframe debe contener las columnas 'Alternativas' y 'score'.")
  }

  # Crear la cadena de texto en el formato especificado
  result <- df %>%
    arrange(score) %>%
    mutate(combined = paste0(score, ". ", Alternativas)) %>%
    pull(combined) %>%
    paste(collapse = "; ")

  return(result)
}

rename_columns <- function(df, new_names, columns) {
  library(dplyr)
  old_names <- names(df)[columns]  # Obtiene los nombres antiguos usando los índices
  names_dict <- setNames(old_names, new_names)  # Crea un diccionario con los nombres antiguos y los nuevos

  # Renombra las columnas en el dataframe usando el diccionario
  df <- rename(df, !!!names_dict)

  return(df)
}

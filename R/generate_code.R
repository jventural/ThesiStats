generate_code <- function(text, name = "df_new_renombrado") {
  # Dividir el texto en líneas
  lines <- unlist(strsplit(text, "\n"))

  # Extraer factores e ítems
  factors <- gsub(":.+", "", lines)
  items <- gsub(".+:", "", lines)
  items <- lapply(items, function(x) unlist(strsplit(trimws(x), ", ")))

  # Crear el código
  code <- paste0(name, " <- ", name, " %>% \n  rowwise() %>% \n  mutate(")

  for (i in seq_along(factors)) {
    factor <- factors[i]
    if (grepl(" ", factor)) {
      factor <- paste0("`", factor, "`")
    }
    item_list <- paste(items[[i]], collapse = ",")
    code <- paste0(code, factor, " = sum(c_across(c(", item_list, "))),\n         ")
  }

  # Ajuste final para el cierre del mutate
  code <- paste0(substr(code, 1, nchar(code) - 11), ") %>% \n  ungroup()")

  return(code)
}

normalize_ciclo <- function(df, col_name = "Ciclo") {
  # instalar y cargar dependencias
  for(pkg in c("dplyr","stringr","readr")) {
    if (!requireNamespace(pkg, quietly=TRUE)) install.packages(pkg)
    library(pkg, character.only=TRUE, quietly=TRUE, warn.conflicts=FALSE)
  }

  # 1) Limpiar y extraer valores
  df2 <- df %>%
    mutate(
      .clean = .data[[col_name]] %>%
        str_to_lower() %>%
        str_replace_all("º|°", "") %>%
        str_replace_all("[[:punct:]]", "") %>%
        str_squish(),

      # extraer número puro sin warnings
      .rawnum = suppressWarnings(parse_number(.clean)),

      # numerales romanos
      .roman = str_extract(.clean,
                           "^(?i)(?:m{0,4}(?:cm|cd|d?c{0,3})(?:xc|xl|l?x{0,3})(?:ix|iv|v?i{0,3}))$"
      ),
      .num = coalesce(
        .rawnum,
        if_else(!is.na(.roman),
                as.numeric(as.roman(str_to_upper(.roman))),
                NA_real_)
      ),

      # patrones ordinales en texto
      .num = case_when(
        str_detect(.clean, "^(primer|primero|1er|1ero)(?: ciclo)?$")  ~ 1,
        str_detect(.clean, "^(segundo|2do)(?: ciclo)?$")             ~ 2,
        str_detect(.clean, "^(tercer|3ro|3er)(?: ciclo)?$")          ~ 3,
        str_detect(.clean, "^(cuarto|4to)(?: ciclo)?$")             ~ 4,
        str_detect(.clean, "^(quinto|5to)(?: ciclo)?$")             ~ 5,
        str_detect(.clean, "^(sexto|6to)(?: ciclo)?$")              ~ 6,
        str_detect(.clean, "^(s[eé]ptimo|7mo)(?: ciclo)?$")         ~ 7,
        str_detect(.clean, "^(octavo|8vo)(?: ciclo)?$")             ~ 8,
        str_detect(.clean, "^(noveno|9no)(?: ciclo)?$")             ~ 9,
        str_detect(.clean, "^(d[eé]cimo|10mo|decimo)(?: ciclo)?$")   ~ 10,
        str_detect(.clean, "^(once|11)(?:avo|ero)?(?: ciclo)?$")     ~ 11,
        TRUE                                                       ~ .num
      )
    )

  # 2) Identificar y eliminar filas que no pudieron normalizar
  problematic <- df2 %>%
    filter(is.na(.num)) %>%
    select(Original = !!sym(col_name), Limpio = .clean) %>%
    distinct()

  if (nrow(problematic) > 0) {
    message("Se eliminaron ", nrow(problematic),
            " fila(s) por no poder normalizar en '", col_name, "':")
    print(problematic, n = Inf)
  }

  # 3) Filtrar solo los casos válidos y finalizar
  df2 %>%
    filter(!is.na(.num)) %>%
    mutate(
      !!sym(col_name) := as.integer(.num)
    ) %>%
    select(-.clean, -.rawnum, -.roman, -.num)
}

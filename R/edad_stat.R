edad_stat <- function(df, columna) {
  library(dplyr)
  df %>%
    summarise(Media = mean({{ columna }}),
              DesviacionEstandar = sd({{ columna }}),
              Minimo = min({{ columna }}),
              Maximo = max({{ columna }}))
}

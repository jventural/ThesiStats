\name{normalize_ciclo}
\alias{normalize_ciclo}
\title{Normalize Academic Cycle Strings to Integer Values}
\description{
Standardiza representaciones heterogéneas de ciclos académicos en un data frame, convirtiendo distintas formas de texto (por ejemplo, “4to ciclo”, “III”, “7mo”, “10.0”) en valores enteros de 1 a 11. Reconoce números arábigos, numerales romanos, palabras ordinales en español y abreviaturas con sufijo opcional “ciclo”.
}
\usage{
normalize_ciclo(df, col_name = "Ciclo")
}
\arguments{
  \item{df}{Un data frame o tibble que contiene la columna a normalizar.}
  \item{col_name}{Cadena de caracteres con el nombre de la columna que contiene la información de ciclo; por defecto \code{"Ciclo"}.}
}
\details{
La función se encarga de instalar (si es necesario) y cargar los paquetes \pkg{dplyr}, \pkg{stringr} y \pkg{readr}. Luego:
\enumerate{
  \item Limpia la columna objetivo convirtiendo a minúsculas, quitando signos de grado (º, °), puntuación y espacios sobrantes.
  \item Extrae valores numéricos con \code{parse_number()} (suprimiendo advertencias), de modo que “8.0” → 8.
  \item Detecta numerales romanos válidos (I–XI) con expresiones regulares y los convierte mediante \code{as.roman()}.
  \item Mapea palabras ordinales en español (“primero”, “séptimo”) y abreviaturas (“1ero”, “7mo”) — con o sin la palabra “ciclo” — a su equivalente numérico.
  \item Reemplaza la columna original con el entero resultante y elimina las columnas auxiliares de procesamiento.
}
}
\value{
Un data frame idéntico al original, salvo que la columna especificada en \code{col_name} ha sido sustituida por valores enteros (1–11). Las demás columnas permanecen sin cambios.
}
\examples{
## Data frame de ejemplo con formatos mixtos
df <- data.frame(Ciclo = c("4to ciclo", "III", "7mo", "10.0", "Sexto ciclo"))

## Normalizar a enteros
df_norm <- normalize_ciclo(df, "Ciclo")

## Resultado esperado:
##   Ciclo
## 1     4
## 2     3
## 3     7
## 4    10
## 5     6
}
\author{
Dr. José Ventura-León
}

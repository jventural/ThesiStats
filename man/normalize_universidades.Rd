\name{normalize_universidades}
\alias{normalize_universidades}
\title{Normalize Peruvian University Names to Standard Acronyms}
\description{
Estandariza nombres de universidades peruanas en un data frame, asignando a cada entrada su acrónimo oficial según la base interna \code{ThesiStats::universidades_peruanas}. Utiliza distancia de Jaro–Winkler para comparar cadenas y coincide nombres completos o sus siglas.
}
\usage{
normalize_universidades(df, col_name)
}
\arguments{
  \item{df}{Un data frame o tibble que contiene la columna con nombres de universidades a normalizar.}
  \item{col_name}{Cadena de caracteres con el nombre de la columna que contiene las universidades originales.}
}
\details{
La función se encarga de instalar (si es necesario) y cargar los paquetes \pkg{dplyr}, \pkg{stringr} y \pkg{stringdist}. A continuación:
\enumerate{
  \item Obtiene la referencia interna \code{ThesiStats::universidades_peruanas}, que incluye columnas \code{Nombre} y \code{Acrónimo}.
  \item Define \code{match_one()}, que para cada cadena:
    \begin{itemize}
      \item Limpia texto (minúsculas, quita puntuación y espacios extras).
      \item Calcula la distancia de Jaro–Winkler entre la entrada y los nombres completos (\code{Nombre}) y los acrónimos (\code{Acrónimo}) del catálogo.
      \item Selecciona la coincidencia con menor distancia, priorizando nombres completos sobre acrónimos si la distancia mínima es igual o menor, y acepta coincidencias hasta un umbral de 0.15.
      \item Retorna el acrónimo correspondiente o \code{NA} si no hay coincidencia confiable.
    \end{itemize}
  \item Aplica \code{match_one()} a toda la columna de entrada y recopila las entradas que no pudieron normalizarse (quedaron \code{NA}), mostrando un mensaje con los casos no emparejados o confirmando el éxito.
  \item Inserta una nueva columna \code{<col_name>_norm} con los acrónimos normalizados inmediatamente después de la columna original.
}
}
\value{
Un data frame igual al original, pero con una columna adicional \code{<col_name>_norm} que contiene los acrónimos oficiales de las universidades (o \code{NA} si no pudo normalizarse). La columna original permanece intacta.
}
\examples{
## Data frame de ejemplo con nombres inconsistentes
df <- data.frame(Universidad = c("UniNacional de San Marcos", "PUCP", "San Martin", "Univ. Agraria"))

## Normalizar a acrónimos oficiales
df_norm <- normalize_universidades(df, "Universidad")

## Resultado esperado (según catálogo interno):
##                Universidad Universidad_norm
## 1 UniNacional de San Marcos              UNMSM
## 2                  PUCP               PUCP
## 3           San Martin              UNSM
## 4           Univ. Agraria              UNAJ
}
\author{
Dr. José Ventura-León
}

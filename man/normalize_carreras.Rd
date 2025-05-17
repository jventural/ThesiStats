\name{normalize_carreras}
\alias{normalize_carreras}
\title{Normalize Academic Program Names and Faculties to Standard Labels}
\description{
Estandariza nombres de programas académicos en un data frame, asignando a cada entrada su etiqueta oficial y, opcionalmente, la facultad correspondiente, según la base interna \code{ThesiStats::carreras_peruanas}. Emplea distancia de Jaro–Winkler sobre cadenas preprocesadas (sin acentos ni puntuación) para identificar la mejor coincidencia dentro de un umbral configurable.
}
\usage{
normalize_carreras(df, col_name = "Carrera", max_dist = 0.15, facultad = FALSE)
}
\arguments{
\item{df}{Un data frame o tibble que contiene la columna con los nombres de carreras a normalizar.}
\item{col_name}{Cadena de caracteres con el nombre de la columna que contiene las carreras originales; por defecto \code{"Carrera"}.}
\item{max_dist}{Distancia máxima de Jaro–Winkler para considerar una coincidencia válida; por defecto \code{0.15}.}
\item{facultad}{Lógico; si es \code{TRUE}, además de la columna de carreras normalizadas (\code{<col_name>_norm}), añade otra columna (\code{<col_name>_facultad}) con la facultad asociada.}
}
\details{
La función realiza los siguientes pasos:
\enumerate{
\item Instala (si es necesario) y carga los paquetes \pkg{dplyr}, \pkg{stringr}, \pkg{stringdist} y \pkg{stringi}.
\item Carga el catálogo interno \code{ThesiStats::carreras_peruanas}, que incluye las columnas \code{Carrera} y \code{Facultad}.
\item Preprocesa tanto las entradas de \code{df} como el catálogo, convirtiendo a minúsculas, eliminando acentos (vía \code{stri_trans_general}), quitando puntuación y espacios sobrantes.
\item Para cada valor original:
\begin{itemize}
\item Calcula la distancia de Jaro–Winkler frente a todas las cadenas del catálogo.
\item Selecciona la entrada con la mínima distancia si ésta es menor o igual a \code{max_dist}; en caso contrario, asigna \code{NA}.
\end{itemize}
\item Construye la columna \code{<col_name>_norm} con el nombre de la carrera normalizada; si \code{facultad = TRUE}, añade también \code{<col_name>_facultad} con la facultad correspondiente.
\item Muestra un mensaje listando las entradas que quedaron sin normalizar (\code{NA}) o confirma que todas se procesaron correctamente.
}
}
\value{
Un data frame idéntico al original, con las siguientes columnas adicionales:
\item{\code{<col_name>_norm}}{Vector de caracteres con las carreras normalizadas (o \code{NA} si no hubo coincidencia).}
\item{\code{<col_name>_facultad}}{(Sólo si \code{facultad = TRUE}) Vector de caracteres con la facultad asociada a cada carrera normalizada (o \code{NA}).}
}
\examples{

Data frame de ejemplo con variantes de nombres
df <- data.frame(
Carrera = c("Psicologia", "Ing. Industrias Alimentarias", "Derecho", "PSICO"),
stringsAsFactors = FALSE
)

Solo normalizar nombres de carrera
normalize_carreras(df)

Normalizar nombres y obtener facultades
normalize_carreras(df, facultad = TRUE)
}
\author{
Dr. José Ventura-León
}

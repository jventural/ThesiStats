\name{normalize_tiempo_relacion}
\alias{normalize_tiempo_relacion}
\title{Normalize Relationship Duration Strings to Total Months}

\description{
Standardizes various Spanish-language relationship duration formats into a single numeric value (total months), facilitating quantitative analyses of temporal data.
}

\usage{
normalize_tiempo_relacion(
df,
col_name = "Tiempo_Relacion",
remover   = TRUE
)
}

\arguments{
\item{df}{
A data frame or tibble containing the column to normalize.
}

\item{col_name}{
Character string specifying the name of the duration column. Defaults to \code{"Tiempo_Relacion"}.
}

\item{remover}{
Logical flag. If \code{TRUE}, removes rows with unrecognized formats; if \code{FALSE}, retains them with \code{NA} in the normalized column.
}
}

\details{

Package dependencies: Ensures \pkg{dplyr}, \pkg{stringr}, \pkg{readr} are installed and loaded.

Preprocessing:

Trim whitespace and convert text to lowercase.

Special-case patterns (predefined values):
\itemize{
\item “7 casi 8” → 8
\item “1año y 6 meses” → 18
\item “3 años y 8 meses” → 44
\item “1 año 5 meses” → 17
\item “1 año y tres meses” → 15
\item “Un año con dos meses” → 14
\item “1 año y cuatro meses” → 16
\item “1 año con 11 meses” → 23
\item “1 y 7 meses” → 19
}

General parsing:

Extract years (\code{"años"}, \code{"un año"}); convert to months (years × 12).

Extract explicit months (\code{"meses"}, \code{"mes"}); convert to numeric.

Detect half-years (\code{"y medio"}); add 6 months.

Convert pure numeric strings (e.g., \code{"5.0"}) to months.

Convert weeks to months (7 days ≈ 1 month), rounding to one decimal.

Computation: Combine special-case, years, months, half-year, numeric, or week-derived values in order of priority.

Output: Adds a new column \code{<col_name>_norm} with numeric total months; drops temporary helper columns. If \code{remover = TRUE}, rows with \code{NA} in normalized column are removed.
}

\value{
A data frame identical to \code{df} but with an added column \code{<col_name>_norm} containing the normalized month values.
}

\author{
Dr. José Ventura-León
}

\examples{
\dontrun{

Example dataset

df <- data.frame(
Tiempo_Relacion = c(
"2 años y medio",
"8 meses casi 9",
"1 año y tres meses",
"Un año con dos meses",
"1 y 7 meses",
"5.0"
),
stringsAsFactors = FALSE
)

Apply normalization

df_norm <- normalize_tiempo_relacion(df)
print(df_norm)
}

Expected output:

Tiempo_Relacion Tiempo_Relacion_norm

#1 2 años y medio                   30
#2 8 meses casi 9                    8
#3 1 año y tres meses               15
#4 Un año con dos meses             14
#5 1 y 7 meses                      19
#6 5.0                               5

\author{
Dr. José Ventura-León
}

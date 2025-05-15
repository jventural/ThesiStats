\name{normalize_tiempo_relacion}
\alias{normalize_tiempo_relacion}
\title{Normalize Relationship Duration Strings to Total Months}
\description{
This function standardizes heterogeneous representations of relationship duration in a dataset by converting various Spanish-language formats (e.g., “2 años y medio”, “8 meses casi 9”, “1,5 meses”, “Un año”) into a single numeric value representing total months. It is ideal for preparing temporal relationship data for quantitative analyses.
}
\usage{
normalize_tiempo_relacion(df, col_name = "Tiempo_Relacion")
}
\arguments{
\item{df}{A data frame or tibble containing the column to normalize.}
\item{col_name}{A string specifying the name of the column with duration information; defaults to \code{"Tiempo_Relacion"}.}
}
\details{
The function ensures \pkg{dplyr} and \pkg{stringr} are installed and loaded. It then:
\enumerate{
\item Cleans the target column by trimming whitespace and converting to lowercase.
\item Extracts the number of years when patterns like “años” or “un año” appear, converting to numeric.
\item Extracts explicit months when patterns “meses” or “mes” appear.
\item Detects “medio” in the context of years and adds 6 months for half-year durations.
\item Interprets purely numeric values (e.g., “5.0”, “05”) as months.
\item Computes the total in months by summing (years × 12) + explicit months + pure-month values + half-year months.
\item Overwrites the original column with the computed total months and removes all temporary helper columns.
}
}
\value{
A modified version of the input \code{df} where the specified \code{col_name} has been replaced by an integer or numeric vector giving the total relationship duration in months. All other columns remain unchanged.
}
\examples{

Example dataset
df <- data.frame(
Tiempo_Relacion = c(
"2 años y medio",
"8 meses casi 9",
"1 año",
"1,5 meses",
"Un año y medio",
"20 meses"
),
stringsAsFactors = FALSE
)

Apply normalization
df_norm <- normalize_tiempo_relacion(df, "Tiempo_Relacion")

Expected df_norm$Tiempo_Relacion:
[1] 30 8 12 1.5 18 20
}
\author{
Dr. José Ventura-León
}

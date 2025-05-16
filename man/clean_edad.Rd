\name{clean_edad}
\alias{clean_edad}
\title{Clean and Convert Age Strings to Fractional Years with Exclusion Reporting}
\description{
This function standardizes a character column containing age or time-duration information in Spanish by extracting years and months—converting months to fractional years—and coercing the result to numeric. Rows without any numeric content are excluded, with a message reporting how many were removed. It handles diverse formats such as “19 años”, “1 año 6 meses”, “03 meses”, “2.5”, or “2 meses y 20 días”.
}
\usage{
clean_edad(df, col_name = "Edad", round_decimals = 2)
}
\arguments{
\item{df}{A data frame or tibble containing the column to be cleaned.}
\item{col_name}{A string giving the name of the column to clean; defaults to \code{"Edad"}.}
\item{round_decimals}{Integer: number of decimal places to round the final fractional years; defaults to 2.}
}
\details{
\pkg{clean_edad} ensures that \pkg{dplyr}, \pkg{stringr}, and \pkg{readr} are installed and loaded. It then:
\enumerate{
\item Filters out rows that contain no digits, excluding them from the result.
\item Cleans each value by trimming whitespace and converting to lowercase.
\item Extracts the number of years from patterns like “\saños?”, “un año”, or numeric strings.
\item Extracts months from patterns like “\smeses?” and converts them to a fraction of a year.
\item Parses any pure numeric value (e.g., “2.5”, “03”) as years.
\item Computes total years as \code{years + months/12}, then rounds to \code{round_decimals}.
\item Replaces the original column with the computed numeric values and drops helper columns.
\item Displays a message indicating how many rows were excluded for lacking numeric content.
}
}
\value{
A data frame identical to the input \code{df} except:
\item{–}{The specified \code{col_name} is replaced by a numeric vector of fractional years.}
\item{–}{Rows without any digits in the original column are removed.}
}
\examples{
df <- data.frame(
Edad = c(
"19 años", "1 año 6 meses", "03 meses", "2 años y medio",
"veinte años", "2.5", "sin dato"
),
stringsAsFactors = FALSE
)
df_clean <- clean_edad(df)

Message: "2 filas excluídas por no contener ningún número en 'Edad'"
df_clean$Edad:
1 19.00
2 1.50
3 0.25
4 2.50
5 2.50
6 2.50
}
\author{
Dr. José Ventura-León
}

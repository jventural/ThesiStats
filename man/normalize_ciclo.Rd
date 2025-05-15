\name{normalize_ciclo}
\alias{normalize_ciclo}
\title{Normalize Academic Cycle Strings to Integer Values}
\description{
This function standardizes heterogeneous representations of academic cycle or semester data in a data frame by converting various string formats (e.g., “4to ciclo”, “III”, “7mo”, “10.0”) into integer cycle numbers (1–11). It handles numeric, Roman numeral, ordinal text (Spanish), and optional “ciclo” suffix variants.
}
\usage{
normalize_ciclo(df, col_name = "Ciclo")
}
\arguments{
\item{df}{A data frame or tibble containing the column to normalize.}
\item{col_name}{A string specifying the name of the column with cycle information; defaults to \code{"Ciclo"}.}
}
\details{
The function ensures that \pkg{dplyr}, \pkg{stringr}, and \pkg{readr} are installed and loaded. It then:
\enumerate{
\item Cleans the target column by trimming whitespace and converting to lowercase.
\item Uses \code{parse_number()} (with warnings suppressed) to extract any numeric value (e.g., “8.0” → 8).
\item Detects full Roman numerals (I–XI) and converts them via \code{as.roman()}.
\item Maps Spanish ordinal words (e.g., “primero”, “séptimo”) and abbreviations (“1ero”, “7mo”) — with an optional “ ciclo” suffix — to their numeric equivalents.
\item Writes the final integer back into the original column and drops all temporary helper columns.
}
}
\value{
A modified version of the input \code{df} in which the specified \code{col_name} has been replaced by integer cycle numbers (1–11). All other columns remain unchanged.
}
\examples{

Suppose df has a column 'Ciclo' with mixed formats
df <- data.frame(Ciclo = c("4to ciclo", "III", "7mo", "10.0", "Sexto ciclo"))

Normalize to integers
df_norm <- normalize_ciclo(df, "Ciclo")

Result:
Ciclo
1 4
2 3
3 7
4 10
5 6
}
\author{
Dr. José Ventura-León
}

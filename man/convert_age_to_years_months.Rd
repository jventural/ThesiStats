\name{convert_age_to_years_months}
\alias{convert_age_to_years_months}
\title{Convert Age Strings with Years and Months to Numeric Years}

\description{
Parses a character column of age descriptions in Spanish (e.g., “2 años”, “6 meses”, “1 año 3 meses”, “3,5”) and converts them into numeric year values—expressing months as fractional years—optionally removing entries that originated as missing or zero.
}

\usage{
convert_age_to_years_months(df, col_name = "Edad", round_decimals = 2, drop_missing = FALSE)
}

\arguments{
\item{df}{A data frame containing the age column to be converted.}
\item{col_name}{Name of the column (string) in \code{df} with age descriptions. Defaults to \dQuote{Edad}.}
\item{round_decimals}{Number of decimal places to round the resulting numeric years. Defaults to 2.}
\item{drop_missing}{Logical; if \code{TRUE}, rows where conversion yields zero (i.e., originally non-numeric or missing) are removed from the cleaned output. Defaults to \code{FALSE}.}
}

\details{
The function performs the following steps:

Installs (if necessary) and loads required packages: \pkg{dplyr}, \pkg{stringr}, \pkg{tibble}, and \pkg{readr}.

Coerces the specified column to character and applies an internal parser that:
\itemize{
\item Detects explicit “años” patterns and extracts the numeric part as years.
\item Recognizes “un año” as 1 year.
\item Detects “meses” patterns and extracts the numeric part as months.
\item Falls back to any standalone numeric value if no keywords are present.
}

Computes the total in years as \code{years + months/12}, or \code{months/12} if no years were provided, or the raw numeric if neither keyword appears.

Rounds the result to \code{round_decimals} digits and emits a verification message for pure months entries.

Constructs a \code{cleaned_df} replacing the original column with the numeric output and a \code{verify_df} pairing raw and cleaned values.

If \code{drop_missing = TRUE}, filters out rows where the converted value equals zero.
}

\value{
A named list with two elements:
\describe{
\item{\code{cleaned_df}}{A data frame identical to \code{df} but with the specified column converted to numeric years (and optionally filtered).}
\item{\code{verify_df}}{A tibble with two columns, \dQuote{raw} and \dQuote{cleaned}, showing original strings and their numeric conversion.}
}
}

\examples{

Convert without dropping zeroes
res1 <- convert_age_to_years_months(my_data, col_name = "Edad_menor", round_decimals = 2)
cleaned <- res1$cleaned_df
verify <- res1$verify_df

Convert and drop entries that were originally non-numeric or zero
res2 <- convert_age_to_years_months(my_data, col_name = "Edad_menor", drop_missing = TRUE)
cleaned2 <- res2$cleaned_df
}

\note{
Requires installation and loading of the \pkg{dplyr}, \pkg{stringr}, \pkg{tibble}, and \pkg{readr} packages.}

\author{
Professor José Ventura-León
}

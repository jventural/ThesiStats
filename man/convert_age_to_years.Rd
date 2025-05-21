\name{convert_age_to_years}
\alias{convert_age_to_years}
\title{Convert Age Strings to Numeric Years}

\description{
Parses a character column of age descriptions in Spanish (e.g., “2 años”, “1 año”, “3,5”) and converts them into numeric year values, optionally removing entries with missing or non-numeric ages.
}

\usage{
convert_age_to_years(df,
col_name = "Edad",
round_decimals = 2,
drop_missing = FALSE)
}

\arguments{
\item{df}{A data frame containing the age column to be converted.}
\item{col_name}{Name of the column (string) in \code{df} with age descriptions. Defaults to \dQuote{Edad}.}
\item{round_decimals}{Number of decimal places to round the resulting numeric years. Defaults to 2.}
\item{drop_missing}{Logical; if \code{TRUE}, rows where conversion yields \code{NA} are removed from the cleaned output. Defaults to \code{FALSE}.}
}

\details{
The function performs the following steps:

Installs (if necessary) and loads required packages: \pkg{dplyr}, \pkg{stringr}, \pkg{tibble}, and \pkg{readr}.

Coerces the specified column to character and applies an internal parser that:

Detects explicit “años” patterns and extracts the numeric part.

Recognizes “un año” as 1.

Falls back to any standalone numeric value if no “años” keyword is present.

Rounds the result to \code{round_decimals} digits.

Constructs a \code{cleaned_df} replacing the original column with the numeric output and a \code{verify_df} pairing raw and cleaned values.

If \code{drop_missing = TRUE}, filters out rows with \code{NA} in the converted column.

If \code{drop_missing = FALSE}, emits a message listing rows that could not be parsed (non-digit entries).
}

\value{
A named list with two elements:
\describe{
\item{\code{cleaned_df}}{A data frame identical to \code{df} but with the specified column converted to numeric years (and optionally filtered).}
\item{\code{verify_df}}{A tibble with two columns, \dQuote{raw} and \dQuote{cleaned}, showing original strings and their numeric conversion.}
}
}

\examples{

# Convert without dropping missing
res1 <- convert_age_to_years(my_data, col_name = "Edad_padre")
cleaned <- res1$cleaned_df
verify <- res1$verify_df

# Convert and drop rows where conversion failed
res2 <- convert_age_to_years(my_data, col_name = "Edad_padre", drop_missing = TRUE)
cleaned2 <- res2$cleaned_df
}

\note{
Requires installation and loading of the \pkg{dplyr}, \pkg{stringr}, \pkg{tibble}, and \pkg{readr} packages.}

\author{
Professor José Ventura-León
}

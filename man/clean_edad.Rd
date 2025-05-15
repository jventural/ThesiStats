\name{clean_edad}
\alias{clean_edad}
\title{Clean Age Column by Removing “ años” Suffix and Converting to Numeric}
\description{
This function cleans a character column containing age information in Spanish by removing the trailing “ años” and converting the result to numeric. It is useful for preparing age data recorded as strings (e.g., “19 años”, “20”, “21.5 años”) for analysis.
}
\usage{
clean_edad(df, col_name = "Edad")
}
\arguments{
\item{df}{A data frame or tibble containing the column to be cleaned.}
\item{col_name}{A string giving the name of the column to clean; defaults to \code{"Edad"}.}
}
\details{
The function first checks whether \pkg{dplyr} and \pkg{stringr} are installed, installing them if necessary, and then loads them. It applies \code{str_remove()} to strip the exact suffix “ años” from each value and then coerces the remaining string to numeric with \code{as.numeric()}. Values that cannot be coerced will become \code{NA}.
}
\value{
A data frame identical to the input \code{df} but with the specified column replaced by numeric values representing age in years.
}
\examples{

Example usage:
df <- data.frame(Edad = c("19 años", "20", "18 años", "21.5 años", "veinte años"))

Clean and convert:
df_clean <- clean_edad(df)

Result:
Edad
1 19
2 20
3 18
4 21.5
5 NA
}
\author{
Dr. José Ventura-León
}

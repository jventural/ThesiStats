\name{validation_categoria}
\alias{validation_categoria}
\title{Validate and List Unique Categories in Specified Columns}
\description{
Extracts and alphabetically sorts all unique values (categories) present in a selected set of columns of a data frame or tibble, making it easy to detect inconsistencies or variations in response levels.
}
\usage{
validation_categoria(df, cols)
}
\arguments{
\item{df}{A \code{data.frame} or \code{tibble} containing the columns whose unique values you want to inspect.}
\item{cols}{A selection of columns using \pkg{dplyr} syntax (e.g., \code{VAR1:VAR5}, \code{starts_with("Q")}, etc.) from which to extract categories.}
}
\details{
The function performs the following steps:
\enumerate{
\item Ensures that the \pkg{dplyr} package is loaded.
\item Selects only the columns specified by \code{cols}.
\item Flattens the selected subset into a single atomic vector via \code{unlist(use.names = FALSE)}.
\item Extracts unique values using \code{unique()}.
\item Sorts those values alphabetically with \code{sort()}.
}
This workflow allows you to quickly identify spelling variants, extra spaces, or differences in capitalization among the categories.
}
\value{
A character vector, sorted alphabetically, containing each distinct level or category found in the specified columns.
}
\examples{

Example data
df <- data.frame(
PROC1 = c("Yes", "No", "Yes"),
PROC2 = c("No", "Maybe", "No"),
PROC3 = c("Yes", "Yes", "Maybe"),
stringsAsFactors = FALSE
)

List all unique categories in PROC1:PROC3
validation_categoria(df, PROC1:PROC3)
#> [1] "Maybe" "No" "Yes"
}
\author{
Dr. José Ventura-León
}

\name{Correct_category}
\alias{Correct_category}
\title{Recodes Specified Values in Selected Columns Using Replacement Formulas}
\description{
Applies user-defined recoding rules to one or more columns of a data frame or tibble, replacing exact matches of old values with new values as specified by two-sided formulas.
}
\usage{
Correct_category(df, cols, ...)
}
\arguments{
\item{df}{A \code{data.frame} or \code{tibble} containing the columns to be recoded.}
\item{cols}{A selection of columns using \pkg{dplyr} syntax (e.g., \code{VAR1:VAR5}, \code{starts_with("Q")}, etc.) to which the recoding rules will be applied.}
\item{\dots}{One or more two-sided formulas of the form \code{"old_value" ~ "new_value"} defining the recoding mapping. Each formula must have a left-hand side (the value to replace) and a right-hand side (the replacement value).}
}
\details{
The function executes the following steps:
\enumerate{
\item Loads (if necessary) the \pkg{dplyr} package.
\item Captures the recoding formulas passed via \dots{} into a list.
\item Validates that each element in this list is a two-sided formula (\code{lhs ~ rhs}).
\item Uses \code{mutate(across())} to apply the recoding to each specified column:
\begin{itemize}
\item Iterates over each formula.
\item Extracts the old value (\code{lhs}) and the new value (\code{rhs}).
\item Replaces all exact matches of the old value with the new value within the column vector.
\end{itemize}
}
}
\value{
A data frame identical to \code{df}, except that in the columns specified by \code{cols}, every occurrence of each \code{old_value} has been replaced by the corresponding \code{new_value}. Other columns remain unchanged.
}
\examples{
library(dplyr)

Example data
df <- tibble(
Q1 = c("Yes", "No", "yes", "No"),
Q2 = c("Maybe", "maybe", "No", "Yes")
)

Recode exact matches (case-sensitive)
df_fixed <- Correct_category(
df,
Q1:Q2,
"yes" ~ "Yes",
"maybe" ~ "Maybe"
)

df_fixed
#> # A tibble: 4 × 2
#> Q1 Q2
#> <chr> <chr>
#> 1 Yes Maybe
#> 2 No Maybe
#> 3 Yes No
#> 4 No Yes
}
\author{
Dr. José Ventura-León
}

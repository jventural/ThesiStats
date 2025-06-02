\name{generate_and_apply}
\alias{generate_and_apply}
\title{Generate and Apply Summation of Item Columns by Factor to a New Data Frame}

\description{
Parses a vector of text lines that define factors and their corresponding item columns (e.g., “Individual: DP1, DP2, DP3, …”), computes the row-wise sums of those item columns for each factor, and returns a modified data frame with new factor-score columns. The result is also assigned to the global environment under a user-specified name.
}

\usage{
generate_and_apply(df, text_lines, new_name = "df_new")
}

\arguments{
\item{df}{A data frame containing the original item-level columns (e.g., DP1, DP2, …, ASP1, ASP2, …).}
\item{text_lines}{A character vector where each element has the form \dQuote{FactorName: Item1, Item2, …}. Each line defines one factor and the items to be summed for that factor.}
\item{new_name}{Name (string) that the resulting data frame will be saved as in the global environment. Defaults to \dQuote{df_new}.}
}

\details{
The function performs the following sequence of operations:

\itemize{
\item Parses each element of \code{text_lines} by splitting at the first colon (\code{:}) to extract the factor name (everything before the colon) and a comma-separated list of item column names (everything after the colon).
\item Trims whitespace and splits the list of item names by commas, producing a character vector of column names for each factor.
\item Makes a copy of the original \code{df} into \code{df_new} to avoid modifying the input directly.
\item For each factor:
\begin{itemize}
\item Checks that all listed item column names exist in \code{df_new}. If any are missing, the function stops with an informative error.
\item Coerces each of those item columns to numeric (using \pkg{dplyr}::\code{across(all_of(...), as.numeric)}) to ensure that subsequent summation will work without type issues.
\item Uses \code{rowSums(df_new[, vars_i, drop = FALSE], na.rm = TRUE)} to compute the row-wise sum of the specified item columns. This sum is stored in a new column named exactly as the factor (e.g., \dQuote{Individual}, \dQuote{Grupal}, etc.) within \code{df_new}.
\end{itemize}
\item After computing all factor scores, the function assigns \code{df_new} to the global environment under the name provided in \code{new_name} using \code{assign(new_name, df_new, envir = .GlobalEnv)}.
\item Invisibly returns \code{df_new} as the function value, allowing the user to capture it if desired.
}
}

\value{
An invisible copy of the modified data frame (\code{df_new}) containing all original columns plus one new column per factor, where each new column holds the row-wise sum of its specified item columns. The data frame is also saved in the user’s global environment under the name given by \code{new_name}.
}

\examples{
\dontrun{

# 1. Read the definition lines from a text file
texto <- readLines("Cotrina_Texto.txt", encoding = "UTF-8")

# 2. Apply the function to your original data frame df2,
# creating a new data frame called "df2_resumido" in the global environment:
generate_and_apply(df2, texto, new_name = "df2_resumido")

After running this, you will have:
- df2 (original data frame)
- df2_resumido (new data frame with new columns)
}

\note{
Requires installation and loading of the \pkg{dplyr} package. Ensure that all item column names specified in \code{text_lines} exactly match the column names in \code{df}. The function will stop if any listed items are not present in \code{df}.
}

\author{
Professor José Ventura-León
}

\name{rename_items}
\alias{rename_items}
\title{Rename Data Frame Columns}

\description{
This function renames the columns of a data frame within a specified range, giving them sequential names based on a single prefix. It is particularly useful for organizing data frames used in psychometric analysis or surveys, where items need to be clearly labeled and grouped.
}

\usage{
rename_items(df, prefix1 = "COPE", inici = NULL, final = NULL, n_items1 = NULL)
}

\arguments{
  \item{df}{A data frame containing the data with columns to be renamed.}
  \item{prefix1}{A string that specifies the prefix for the new column names. Defaults to "COPE".}
  \item{inici}{The name or position of the first column to rename. If not provided, renaming starts from the first column.}
  \item{final}{The name or position of the last column to rename. If not provided, renaming continues until the last column.}
  \item{n_items1}{An integer specifying the number of columns to rename. If not provided, all columns within the specified range are renamed.}
}

\details{
The function performs the following steps:
1. Determines the start and end indices of the columns to rename based on the provided arguments.
2. Calculates the number of variables to rename.
3. If `n_items1` is not provided, defaults to renaming all columns within the specified range.
4. Renames the columns by generating sequential names with the specified prefix.
5. Returns the modified data frame.
}

\value{
A data frame with renamed columns, based on the provided prefix and range.
}

\examples{
df <- data.frame(COPE1 = 1:5, COPE2 = 2:6, COPE3 = 3:7)
renamed_df <- rename_items(df, prefix1 = "COPE", inici = "COPE1", final = "COPE3", n_items1 = 3)
print(renamed_df)
}

\author{
Professor José Ventura León
}

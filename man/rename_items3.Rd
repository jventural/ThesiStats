\name{rename_items3}
\alias{rename_items3}
\title{Advanced Item Renaming Tool for Complex Datasets}

\description{
This function facilitates the systematic renaming of variables in a dataset across three distinct sections or types, applying different prefixes to each group. It is particularly useful for organizing complex datasets with variables that require clear, standardized identifiers. This tool allows the user to select specific ranges within the dataset for renaming, making it highly flexible for varied data structures.
}

\usage{
rename_items3(df, prefix1 = "COPE", prefix2 = "E", prefix3 = "F", inicio = "StartVar", final = "EndVar", n_items1 = 10, n_items2 = 15, n_items3 = 5)
}

\arguments{
  \item{df}{The data frame containing the variables to be renamed.}
  \item{prefix1}{The prefix for the first group of variables.}
  \item{prefix2}{The prefix for the second group of variables.}
  \item{prefix3}{The prefix for the third group of variables.}
  \item{inicio}{The column name or identifier where renaming should start. This can be an actual name or position.}
  \item{final}{The column name or identifier where renaming should end, defining the range of interest.}
  \item{n_items1}{The number of variables in the first group to receive prefix1.}
  \item{n_items2}{The number of variables in the second group to receive prefix2.}
  \item{n_items3}{The number of variables in the third group to receive prefix3.}
}

\details{
The function starts by identifying the range of variables to be renamed from 'inicio' to 'final'. It then assigns new names based on the provided prefixes, applying them sequentially within each group. The distribution of items across the three prefixes can be manually specified or automatically calculated to ensure balanced renaming across the dataset. Error checks are performed to ensure the renaming does not exceed the bounds of the dataset.
}

\value{
Returns a data frame with the variables in the specified range renamed. The new names are constructed using the provided prefixes, followed by sequential numbering within each prefix group.
}

\examples{
# Assuming 'data' is a data frame with columns needing structured renaming
data_renamed <- rename_items3(data,
                              prefix1 = "COPE",
                              prefix2 = "E",
                              prefix3 = "F",
                              inicio = "first_variable",
                              final = "last_variable",
                              n_items1 = 10,
                              n_items2 = 15,
                              n_items3 = 5)
}

\author{
Professor José Ventura León
}

\name{rename_columns}
\alias{rename_columns}
\title{Function to Rename DataFrame Columns}

\description{
  This function renames specified columns in a dataframe according to a provided list of new names. It is designed to simplify the process of renaming columns, especially in cases where column names are too long, not descriptive, or need to be standardized for further analysis.
}

\usage{
rename_columns(df, new_names, columns)
}

\arguments{
  \item{df}{The data frame whose columns will be renamed.}
  \item{new_names}{A vector of new names that will replace the current column names. The order of names in this vector should correspond to the order of column indices provided.}
  \item{columns}{A vector of indices indicating the columns to be renamed in the data frame. These indices should correspond to the order of new names provided.}
}

\details{
  The function takes a dataframe, a list of new column names, and the corresponding indices of the columns to be renamed. It first extracts the current names of the specified columns based on the provided indices, then creates a mapping of old names to new names. Using the \code{rename} function from the \pkg{dplyr} package, it updates the column names in the dataframe. This approach ensures that only specified columns are renamed, maintaining the integrity of other data within the dataframe.
}

\value{
  Returns the modified dataframe with updated column names. This allows for seamless integration into data processing workflows, enhancing readability and accessibility of data for analysis.
}

\examples{
df_new <- rename_columns(
  df,
  new_names = c("Carrera", "Programa", "Ciclo_Actual", "Campus", "Ciclo_Ingreso", "Edad"),
  columns = 1:6
)
}

\author{
  Dr. José Ventura-León
}


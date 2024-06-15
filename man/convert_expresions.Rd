\name{convert_expresions}
\alias{convert_expresions}
\title{Convert List of Expressions to Formatted String}

\description{
  This function converts a list of expressions from a specified column in a dataframe into a single formatted string. Each expression is prefixed with its corresponding index, making it ideal for presenting ordered categorical data in a readable format.
}

\usage{
convert_expresions(df)
}

\arguments{
  \item{df}{The data frame containing the column 'Alternativas' with expressions to be formatted.}
}

\details{
  The function checks if the specified column 'Alternativas' exists in the dataframe. If the column is found, it concatenates each item in the column along with its index number into a single string, separated by semicolons. This process facilitates the clear presentation of ordered data, especially useful in contexts like survey data reporting or data summaries.
}

\value{
  Returns a single string that concatenates all expressions in the 'Alternativas' column, each prefixed by its index and separated by semicolons. This string can be directly used in reports, presentations, or any documentation where a clear enumeration of items is required.
}

\examples{
# Asumiendo que 'df' es un dataframe con una columna llamada 'Alternativas'
# Aplicando la función detect_Likert2 y seleccionando columnas ES1 a ES2

expresiones_ordenadas_df <- detect_Likert2(df_new_renombrado)
expresiones_ordenadas_df <- expresiones_ordenadas_df[ , c("ES1", "ES2")]
print(expresiones_ordenadas_df)
}

formatted_string <- convert_expresions(expresiones_ordenadas_df)
print(formatted_string)
}

\author{
  Professor José Ventura León
}


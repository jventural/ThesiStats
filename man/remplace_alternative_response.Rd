\name{remplace_alternative_response}
\alias{remplace_alternative_response}
\title{Response Option Replacement Tool}

\description{
  This function automates the process of replacing textual response options with numeric codes across selected columns in a dataset, based on user-provided mappings. It is particularly useful for standardizing survey data before analysis, ensuring that all responses are numerically coded for statistical software.
}

\usage{
remplace_alternative_response(df, columnas_valores_entradas)
}

\arguments{
  \item{df}{The data frame containing the variables to be transformed.}
  \item{columnas_valores_entradas}{A list of column ranges and their corresponding string-to-number mappings. Each element of the list should be a sublist containing two elements: a range of columns and a string of mappings.}
}

\details{
  The function comprises two primary operations: converting string mappings to numeric codes and applying these codes to specified columns. The `convertir_string_a_lista` internal function parses a string of text options and their numeric equivalents into two separate vectors. The `transformar` internal function then matches text responses in the data frame to their numeric codes and replaces them. Columns are specified in a flexible manner allowing for selective transformations across the dataset.
}

\value{
  Returns a data frame with specified columns transformed from textual to numeric response options, facilitating easier and more robust data analysis.
}

\examples{
# Assuming 'survey_data' is a data frame with text responses in certain columns
column_mappings <- list(
  list(c("CU1", "CU18"), "1. Nunca, 2. Ocasionalmente, 3. Algunas veces, 4. Frecuentemente, 5. Siempre"),
  list(c("CP1", "CP16"), "1. Muy desacuerdo, 2. En desacuerdo, 3. Algo en desacuerdo, 4. De acuerdo, 5. Algo de acuerdo, 6. Muy de acuerdo"),
  list(c("PT1", "PT14"), "1. Totalmente en desacuerdo, 2. En desacuerdo, 3. Ni de acuerdo ni en desacuerdo, 4. De acuerdo, 5. Totalmente de acuerdo")
)
df_transformado <- remplace_alternative_response(df_new_renombrado, column_mappings)
print(df_transformado)
}

\author{
  Professor José Ventura León
}

\name{detect_expression_Likert}
\alias{detect_expression_Likert}
\title{Likert Scale Response Detection and Sequence Addition}

\description{
  This function detects Likert scale response options within a given dataset, normalizes the expressions for comparison, assigns scores based on predefined mappings, and adds a numeric sequence to the resulting data frame. The sequence can start at either 0 or 1, depending on the user's preference.
}

\usage{
detect_expression_Likert(df, start_zero = TRUE)
}

\arguments{
  \item{df}{The data frame containing the variables to be analyzed for Likert scale responses.}
  \item{start_zero}{A logical value indicating whether the added sequence should start at 0 (TRUE) or 1 (FALSE). The default is TRUE.}
}

\details{
  The function consists of two main operations: detecting Likert scale expressions and adding a numeric sequence to the results. The `detect_Likert2` internal function identifies and normalizes Likert scale responses from the data frame, assigning scores based on a predefined set of mappings. The `agregar_secuencia` internal function then adds a numeric sequence to the data frame, starting at either 0 or 1 based on the `start_zero` argument.

  \describe{
    \item{Likert Scale Detection:}{Expressions are normalized for case and accent insensitivity, matched against a predefined list of Likert responses, and assigned corresponding scores.}
    \item{Sequence Addition:}{A numeric sequence is added to the resulting data frame, facilitating ordered analysis of the responses.}
  }
}

\value{
  Returns a data frame with Likert scale responses detected, normalized, scored, and ordered by the assigned sequence.
}

\examples{
# Assuming 'df_new_renombrado' is a data frame with Likert scale responses in columns EA1 to EA9
expresiones_ordenadas_df <- detect_expression_Likert(df_new_renombrado %>% select(EA1:EA9), start_zero = TRUE)
print(expresiones_ordenadas_df)

# To add a sequence starting from 1 instead of 0
expresiones_ordenadas_df <- detect_expression_Likert(df_new_renombrado %>% select(EA1:EA9), start_zero = FALSE)
print(expresiones_ordenadas_df)
}

\author{
  Professor José Ventura León
}


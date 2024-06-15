\name{convert_to_expresions}
\alias{convert_to_expresions}
\title{Likert Scale Response Formatter}

\description{
  This function converts a data frame containing Likert scale response options and their scores into a single formatted string. This is particularly useful for summarizing Likert scale data in a concise and readable manner.
}

\usage{
convert_to_expresions(df)
}

\arguments{
  \item{df}{The data frame containing the Likert scale response options and their corresponding scores. The data frame must include columns named 'Alternativas' and 'score'.}
}

\details{
  The function verifies that the input data frame contains the required columns, arranges the rows based on the 'score' column, and then concatenates the 'score' and 'Alternativas' columns into a single string. The resulting string lists each response option in ascending order of score, formatted as "score. Alternativa", with each entry separated by a semicolon.
}

\value{
  Returns a single string summarizing the Likert scale responses and their scores in the format "score. Alternativa; ...".
}

\examples{
# Assuming 'likert_data' is a data frame with columns 'Alternativas' and 'score'
likert_data <- data.frame(
  Alternativas = c("Nunca", "Casi nunca", "Algunas veces", "Casi siempre", "Regularmente", "Bastantes veces", "Siempre"),
  score = c(0, 1, 2, 3, 4, 5, 6)
)

formatted_string <- convert_to_expresions(likert_data)
print(formatted_string)
# Output: "0. Nunca; 1. Casi nunca; 2. Algunas veces; 3. Regularmente; 4. Bastantes veces; 5. Casi siempre; 6. Siempre"
}

\author{
  Professor José Ventura León
}

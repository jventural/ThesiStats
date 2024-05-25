\name{detect_Likert}
\alias{detect_Likert}
\title{Likert Scale Detection Tool}

\description{
  This function examines a dataframe for unique Likert scale expressions by consolidating all column values into a single column and identifying unique entries. It is designed to assist in recognizing standardized responses in survey data, making it useful for preliminary data exploration or ensuring data consistency.
}

\usage{
detect_Likert(df)
}

\arguments{
  \item{df}{The data frame to be examined for Likert scale expressions.}
}

\details{
  The function uses \code{pivot_longer} from the \pkg{tidyr} package to reshape the data frame, turning all columns into a single 'expression' column. Then, it uses \code{unique} to extract unique expressions found across the dataset. This process helps in identifying the different levels or options of Likert-type responses that might be present in the data.
}

\value{
  Returns a \code{tibble} containing a column 'Alternativas' with all unique Likert scale expressions found in the data frame. This simplifies the process of examining and understanding the range of responses in survey datasets.
}

\examples{
# Assuming 'survey_data' is a data frame with multiple Likert scale questions
likert_expressions <- detect_Likert(survey_data)
print(likert_expressions)
}

\author{
  Professor José Ventura León
}

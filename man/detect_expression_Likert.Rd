\name{detect_expression_Likert}
\alias{detect_expression_Likert}
\title{Likert Scale Response Detection and Sequence Addition}

\description{
  This function detects Likert scale response options within a given dataset by searching for responses that match a specified ordered set of Likert levels. It normalizes the expressions (converting to lower case, removing accents, and trimming whitespace) to ensure accurate matching, filters and orders the responses according to the provided levels, and then adds a numeric sequence to the resulting data frame. The numeric sequence can start at either 0 or 1 based on the user's preference.
}

\usage{
detect_expression_Likert(df, start_zero = TRUE,
  likert_levels = c("Completamente en desacuerdo", "En desacuerdo", "Me es indiferente", "De acuerdo", "Completamente de acuerdo"))
}

\arguments{
  \item{df}{The data frame containing the variables to be analyzed for Likert scale responses.}
  \item{start_zero}{A logical value indicating whether the added numeric sequence should start at 0 (TRUE) or 1 (FALSE). The default is TRUE.}
  \item{likert_levels}{A character vector specifying the ordered Likert scale response options. The default is \code{c("Completamente en desacuerdo", "En desacuerdo", "Me es indiferente", "De acuerdo", "Completamente de acuerdo")}.}
}

\details{
  The function performs two primary operations:

  \describe{
    \item{Likert Scale Detection:}{Responses from the input data frame are first transformed to a long format. Each response is normalized by converting to lower case, removing accent marks, and trimming extra spaces, which allows for a case-insensitive and accent-insensitive match against the predefined Likert levels. Duplicate responses are then removed, and the responses are arranged according to the order specified in \code{likert_levels}.}
    \item{Sequence Addition:}{A numeric sequence is appended to the resulting data frame. The sequence starts at 0 if \code{start_zero} is TRUE, or at 1 if FALSE, thereby facilitating subsequent ordered analysis of the responses.}
  }
}

\value{
  Returns a data frame (tibble) with two columns: \code{Alternativas}, containing the detected and ordered Likert scale responses, and \code{score}, containing the numeric sequence corresponding to each response.
}

\examples{
# Assuming 'df_new_renombrado' is a data frame with Likert scale responses in columns EA1 to EA9

# Detect responses with the sequence starting at 0
expresiones_ordenadas_df <- detect_expression_Likert(
  df_new_renombrado %>% select(EA1:EA9),
  start_zero = TRUE
)
print(expresiones_ordenadas_df)

# Detect responses with the sequence starting at 1
expresiones_ordenadas_df <- detect_expression_Likert(
  df_new_renombrado %>% select(EA1:EA9),
  start_zero = FALSE
)
print(expresiones_ordenadas_df)

# Custom Likert levels example:
expresiones_ordenadas_df <- detect_expression_Likert(
  df_new_renombrado %>% select(EA1:EA9),
  likert_levels = c("Muy en desacuerdo", "En desacuerdo", "Neutral", "De acuerdo", "Muy de acuerdo")
)
print(expresiones_ordenadas_df)
}

\author{
  Professor José Ventura León
}



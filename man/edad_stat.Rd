\name{edad_stat}
\alias{edad_stat}
\title{Statistical Summary for Age Data}

\description{
  This function provides a concise statistical summary of an age column within a dataset. It calculates the mean, standard deviation, minimum, and maximum values of the specified column, offering a quick overview of the distribution and variability of age data.
}

\usage{
edad_stat(df, columna)
}

\arguments{
  \item{df}{The data frame containing the age data to be summarized.}
  \item{columna}{The specific column in the data frame that contains age data and for which statistics will be calculated.}
}

\details{
  The function utilizes the \code{dplyr} package to perform its operations, ensuring efficient and clear handling of the data. It uses \code{summarise} to calculate the mean, standard deviation, minimum, and maximum values, providing a foundational statistical analysis of the age data. This is particularly useful for preliminary data analysis in research or data science projects focusing on demographic information.
}

\value{
  Returns a data frame containing the calculated mean, standard deviation, minimum, and maximum of the specified age column. This summary helps in quickly assessing the central tendency and dispersion of age in a dataset.
}

\examples{
# Assuming 'population_data' is a data frame with a column named 'Age'
age_statistics <- edad_stat(population_data, Age)
print(age_statistics)
}

\author{
  Professor José Ventura León
}

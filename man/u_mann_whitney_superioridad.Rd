\name{u_mann_whitney_superioridad}
\alias{u_mann_whitney_superioridad}
\title{Probability of Superiority Calculation for Mann-Whitney U Test}

\description{
This function calculates the probability of superiority (\(PS\)) as a measure of effect size for the Mann-Whitney U test. It provides an interpretation of the \(PS\) value based on predefined thresholds to classify the effect as "No effect", "Small", "Medium", or "Large". The function is useful for statistical analyses where the Mann-Whitney U test is applied to compare two independent groups.
}

\usage{
u_mann_whitney_superioridad(data, formula, alternative = "two.sided")
}

\arguments{
  \item{data}{A data frame containing the variables to be analyzed.}
  \item{formula}{A formula specifying the dependent and independent variables for the Mann-Whitney U test (e.g., \code{outcome ~ group}). The independent variable must have exactly two levels.}
  \item{alternative}{A character string specifying the alternative hypothesis. Options are \code{"two.sided"} (default), \code{"greater"}, or \code{"less"}.}
}

\details{
The function performs the following steps:
\enumerate{
  \item Verifies that the independent variable in the formula has exactly two levels.
  \item Automatically calculates the sample sizes (\(n_1\) and \(n_2\)) from the levels of the independent variable.
  \item Applies the Mann-Whitney U test using the \code{wilcox.test} function.
  \item Extracts the test statistic and \(p\)-value using the \code{broom::glance} function.
  \item Computes the probability of superiority (\(PS\)) using the formula:
  \deqn{PS = \frac{U}{n_1 \cdot n_2}}
  where \(U\) is the Mann-Whitney test statistic, and \(n_1\) and \(n_2\) are the sample sizes of the two groups.
  \item Interprets the \(PS\) value based on the following thresholds:
  \itemize{
    \item \code{"No effect"} if \(PS \leq 0.0\),
    \item \code{"Small"} if \(PS \geq 0.56\),
    \item \code{"Medium"} if \(PS \geq 0.64\),
    \item \code{"Large"} if \(PS \geq 0.71\).
  }
}

The function returns a data frame with the test statistic, \(p\)-value, \(PS\), and its interpretation.
}

\value{
A data frame with the following columns:
\itemize{
  \item{\code{statistic}: The test statistic of the Mann-Whitney U test.}
  \item{\code{p.value}: The \(p\)-value of the test.}
  \item{\code{method}: The name of the test applied.}
  \item{\code{alternative}: The alternative hypothesis specified.}
  \item{\code{PSest}: The calculated probability of superiority (\(PS\)).}
  \item{\code{Interpretación}: The interpretation of the \(PS\) value as "No effect", "Small", "Medium", or "Large".}
}
}

\examples{
# Example dataset
df <- data.frame(
  sexo = c("Hombre", "Hombre", "Hombre", "Mujer", "Mujer", "Mujer"),
  salario = c(2500, 2700, 3000, 2300, 2600, 3100)
)

# Applying the function
resultado <- u_mann_whitney_superioridad(data = df, formula = salario ~ sexo)
print(resultado)
}

\note{
Installation and loading of the \code{broom} and \code{dplyr} libraries are required for the proper functioning of this function. The independent variable in the formula must be a factor with exactly two levels.
}

\author{
Professor José Ventura León
}

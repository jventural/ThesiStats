\name{u_mann_whitney_superioridad}
\alias{u_mann_whitney_superioridad}
\title{Probability of Superiority Calculation for Mann-Whitney U Test}

\description{
This function calculates the probability of superiority (\(PS\)) as a measure of effect size for the Mann-Whitney U test. It provides an interpretation of the \(PS\) value based on predefined thresholds to classify the effect as "No effect", "Small", "Medium", or "Large". The function is useful for statistical analyses where the Mann-Whitney U test is applied to compare two independent groups.
}

\usage{
u_mann_whitney_superioridad(data, formula, n1, n2, alternative = "two.sided")
}

\arguments{
  \item{data}{A data frame containing the variables to be analyzed.}
  \item{formula}{A formula specifying the dependent and independent variables for the Mann-Whitney U test (e.g., `outcome ~ group`).}
  \item{n1}{The sample size of the first group.}
  \item{n2}{The sample size of the second group.}
  \item{alternative}{A character string specifying the alternative hypothesis. Options are \code{"two.sided"} (default), \code{"greater"}, or \code{"less"}.}
}

\details{
The function performs the following steps:
1. Applies the Mann-Whitney U test using the \code{wilcox.test} function.
2. Extracts the test statistic and \(p\)-value using the \code{broom::glance} function.
3. Computes the probability of superiority (\(PS\)) using the formula:
   \[
   PS = \frac{U}{n_1 \cdot n_2}
   \]
   where \(U\) is the Mann-Whitney test statistic, and \(n_1\) and \(n_2\) are the sample sizes of the two groups.
4. Interprets the \(PS\) value based on the following thresholds:
   - \code{"No effect"} if \(PS \leq 0.0\),
   - \code{"Small"} if \(PS \geq 0.56\),
   - \code{"Medium"} if \(PS \geq 0.64\),
   - \code{"Large"} if \(PS \geq 0.71\).

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

\examples{
# Example dataset
df <- data.frame(
  sexo = c("Hombre", "Hombre", "Hombre", "Mujer", "Mujer", "Mujer"),
  salario = c(2500, 2700, 3000, 2300, 2600, 3100)
)

# Applying the function with sample sizes of 3 for each group
resultado <- u_mann_whitney_superioridad(data = df, formula = salario ~ sexo, n1 = 3, n2 = 3)
print(resultado)
}

\note{
Installation and loading of the `broom` and `dplyr` libraries are required for the proper functioning of this function.
}

\author{
Professor José Ventura León
}

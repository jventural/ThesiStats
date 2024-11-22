\name{epsilon_cuadrado_kruskal}
\alias{epsilon_cuadrado_kruskal}
\title{Effect Size Calculation for Kruskal-Wallis Test using Epsilon Squared}

\description{
This function calculates the effect size (\( \epsilon^2 \)) for the Kruskal-Wallis test, which measures the proportion of variance explained by the grouping variable. The function also provides an interpretation of the \( \epsilon^2 \) value based on thresholds ("Small", "Medium", "Large", or "Not significant"). It is a useful tool for understanding the magnitude of differences between groups in non-parametric statistical analyses.
}

\usage{
epsilon_cuadrado_kruskal(data, formula, n)
}

\arguments{
  \item{data}{A data frame containing the variables to be analyzed.}
  \item{formula}{A formula specifying the dependent and independent variables for the Kruskal-Wallis test (e.g., \code{outcome ~ group}).}
  \item{n}{The total sample size (sum of observations across all groups).}
}

\details{
The function performs the following steps:
1. Applies the Kruskal-Wallis test using the \code{kruskal.test} function.
2. Extracts the test statistic (\( H \)) and \( p \)-value using the \code{broom::glance} function.
3. Computes the effect size (\( \epsilon^2 \)) using the formula:
   \[
   \epsilon^2 = \frac{H}{\frac{n^2 - 1}{n + 1}}
   \]
   where \( H \) is the Kruskal-Wallis test statistic, and \( n \) is the total sample size.
4. Interprets the \( \epsilon^2 \) value based on the following thresholds:
   - \code{"Not significant"} if \( \epsilon^2 < 0.10 \),
   - \code{"Small"} if \( \epsilon^2 \geq 0.10 \),
   - \code{"Medium"} if \( \epsilon^2 \geq 0.30 \),
   - \code{"Large"} if \( \epsilon^2 \geq 0.50 \).

The function returns a data frame with the Kruskal-Wallis test results, the computed \( \epsilon^2 \), and its interpretation.

}

\value{
A data frame with the following columns:
\itemize{
  \item{\code{statistic}: The Kruskal-Wallis test statistic (\( H \)).}
  \item{\code{p.value}: The \( p \)-value of the test.}
  \item{\code{parameter}: The degrees of freedom of the test.}
  \item{\code{method}: The name of the test applied.}
  \item{\code{Epsilon}: The calculated \( \epsilon^2 \) value.}
  \item{\code{Interpretación}: The interpretation of \( \epsilon^2 \) as "Not significant", "Small", "Medium", or "Large".}
}

\examples{
# Example dataset
df <- data.frame(
  group = c("A", "A", "A", "B", "B", "B", "C", "C", "C"),
  value = c(3, 4, 5, 7, 8, 6, 10, 12, 11)
)

# Applying the function with n = 9
resultado <- epsilon_cuadrado_kruskal(data = df, formula = value ~ group, n = 9)
print(resultado)
}

\note{
Installation and loading of the \code{broom} and \code{dplyr} libraries are required for the proper functioning of this function.
}

\author{
Professor José Ventura León
}

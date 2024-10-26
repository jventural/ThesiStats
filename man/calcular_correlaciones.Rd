\name{calcular_correlaciones}
\alias{calcular_correlaciones}
\title{Calculate Correlation}

\description{
This function calculates a correlation matrix using either Pearson, Spearman, or Winsorized Pearson methods. It is designed to work with a range of columns within a dataset, allowing for flexible analysis of variable relationships. The function can handle both normal and non-parametric data distributions, making it suitable for a wide range of statistical applications. Optionally, it can display significance levels for each correlation based on p-values and also returns a p-value matrix in numeric format without scientific notation.
}

\usage{
calcular_correlaciones(data, columna_inicial, columna_final, method = c("spearman", "pearson"), winsorize = FALSE, show_pval = FALSE)
}

\arguments{
  \item{data}{The dataset in which the correlation will be calculated.}
  \item{columna_inicial}{The name of the first column indicating the start of the range of variables for correlation analysis.}
  \item{columna_final}{The name of the last column indicating the end of the range of variables for correlation analysis.}
  \item{method}{The method of correlation calculation. Options are "spearman" for Spearman's rank correlation or "pearson" for Pearson's correlation coefficient. Default is "spearman".}
  \item{winsorize}{A logical value. If TRUE and the method is "pearson", the function calculates a Winsorized Pearson correlation coefficient. Default is FALSE.}
  \item{show_pval}{A logical value. If TRUE, significance levels for p-values are displayed in the correlation matrix as asterisks. Default is FALSE.}
}

\details{
When `winsorize` is set to TRUE, the Pearson correlation is adjusted to minimize the effect of outliers by Winsorizing the data (replacing extreme values) before calculation. This can be particularly useful in datasets with significant outliers.

If `show_pval` is set to TRUE, the correlation matrix includes asterisks indicating the significance level for each correlation coefficient based on its p-value:
  \itemize{
    \item ns. p > 0.05
    \item *. p ≤ 0.05
    \item **. p ≤ 0.01
    \item ***. p ≤ 0.001
    \item ****. p ≤ 0.0001
  }
The function also returns a matrix of p-values in numeric format without scientific notation, allowing for clear interpretation of statistical significance.
}

\value{
A list with two elements:
  \itemize{
    \item \code{correlation}: A data frame containing the correlation matrix with the specified method, including significance asterisks if `show_pval` is set to TRUE.
    \item \code{p_values}: A data frame containing the p-value matrix in numeric format without scientific notation.
  }
}

\examples{
# Example of use with Winsorized Pearson correlation and significance levels displayed
resultado <- calcular_correlaciones(df_new_renombrado, "Ansiedad de separación", "Búsqueda de atención", method = "pearson", winsorize = TRUE, show_pval = TRUE)
print(resultado$correlation) # Correlation matrix with asterisks
print(resultado$p_values)    # P-value matrix in numeric format
}

\author{
Professor José Ventura León
}

#' Correlation Matrix of a Range of Columns
#'
#' Computes the Spearman or Pearson correlation matrix of the columns that go
#' from `columna_inicial` to `columna_final`, optionally with Winsorized
#' Pearson correlations and significance marks, and returns it in the lower
#' triangular format used in thesis tables.
#'
#' @param data A data frame.
#' @param columna_inicial,columna_final Names of the first and last columns of
#'   the range.
#' @param method `"spearman"` (default) or `"pearson"`.
#' @param winsorize Logical. With `method = "pearson"`, compute 20% Winsorized
#'   correlations with `WRS2::winall()` (requires the WRS2 package).
#' @param show_pval Logical. Append significance marks to each correlation:
#'   `ns` p > .05, `*` p <= .05, `**` p <= .01, `***` p <= .001, `****`
#'   p <= .0001.
#'
#' @return A list with `correlation` (data frame with the correlations in the
#'   lower triangle, "-" on the diagonal and `NA` above it) and `p_values` (the
#'   p values in the same format).
#' @export
#' @examples
#' set.seed(1)
#' x <- rnorm(80)
#' df <- data.frame(ansiedad = x, depresion = 0.5 * x + rnorm(80),
#'                  estres = 0.3 * x + rnorm(80))
#' calcular_correlaciones(df, "ansiedad", "estres", method = "pearson",
#'                        show_pval = TRUE)
calcular_correlaciones <- function(data, columna_inicial, columna_final,
                                   method = c("spearman", "pearson"),
                                   winsorize = FALSE, show_pval = FALSE) {
  method <- match.arg(method)

  column_range <- which(names(data) %in% c(columna_inicial, columna_final))
  data <- data[, min(column_range):max(column_range)]

  # significance marks from the NUMERIC p values
  add_significance <- function(cor_values, p_num) {
    significance <- ifelse(is.na(p_num), "",
                    ifelse(p_num > 0.05, "ns",
                    ifelse(p_num > 0.01, "*",
                    ifelse(p_num > 0.001, "**",
                    ifelse(p_num > 0.0001, "***", "****")))))
    cor_with_significance <- paste0(format(cor_values, nsmall = 2), significance)
    matrix(cor_with_significance, nrow = nrow(cor_values), dimnames = dimnames(cor_values))
  }

  if (winsorize && method == "pearson") {
    if (!requireNamespace("WRS2", quietly = TRUE)) {
      stop("winsorize = TRUE needs the 'WRS2' package: install.packages(\"WRS2\").",
           call. = FALSE)
    }
    win_results <- WRS2::winall(data, tr = 0.2)
    cor_values <- round(win_results$cor, 2)
    p_num <- win_results$p.values
    dimnames(cor_values) <- dimnames(p_num) <- list(colnames(data), colnames(data))
  } else {
    cor_values <- round(stats::cor(data, method = method, use = "pairwise.complete.obs"), 2)

    n_vars <- ncol(data)
    p_num <- matrix(NA_real_, nrow = n_vars, ncol = n_vars,
                    dimnames = list(colnames(data), colnames(data)))
    for (i in 1:(n_vars - 1)) {
      for (j in (i + 1):n_vars) {
        p_ij <- tryCatch(
          stats::cor.test(data[[i]], data[[j]], method = method, exact = FALSE)$p.value,
          error = function(e) NA_real_
        )
        p_num[i, j] <- p_num[j, i] <- p_ij
      }
    }
  }

  result_cor <- if (show_pval) add_significance(cor_values, p_num) else cor_values
  p_values <- format(p_num, scientific = FALSE, digits = 4)

  result_cor <- diag_aba_na(result_cor)
  p_values <- diag_aba_na(p_values)

  list(correlation = as.data.frame(result_cor), p_values = as.data.frame(p_values))
}

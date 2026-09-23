#' Compare Two Groups with Welch's t Test and Cohen's d
#'
#' For each variable in `cols`, compares the two groups defined by
#' `group_var` with Welch's t test and reports the means and standard
#' deviations of each group, the t statistic, its degrees of freedom, the
#' p value and Cohen's d with a verbal interpretation.
#'
#' @param data A data frame.
#' @param cols Character vector with the names of the variables to compare.
#' @param group_var Name of the grouping variable. It must have exactly two
#'   groups.
#' @param Robust Logical. If `FALSE` (default), Cohen's d uses the pooled
#'   standard deviation. If `TRUE`, the robust effect size of Algina, Keselman
#'   and Penfield (`WRS2::akp.effect()`) is reported instead; this requires the
#'   WRS2 package.
#'
#' @details The two groups are taken in the order of
#'   `levels(factor(data[[group_var]]))`, the same order used by
#'   [stats::t.test()], so the means, standard deviations, the difference and
#'   the sign of d always refer to the same group. The interpretation uses
#'   |d| > .80 "Grande", > .50 "Mediano", > .30 "Pequeno" and "Trivial"
#'   otherwise.
#'
#' @return A data frame with one row per variable and the columns
#'   `Variables_interes`, `<group 1>(SD1)`, `<group 2>(SD2)`, `t`, `gl`, `p`,
#'   `d_cohen` and `Interpretacion`.
#' @export
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   grupo = rep(c("Mujer", "Varon"), each = 50),
#'   ansiedad = c(rnorm(50, 20, 4), rnorm(50, 18, 4)),
#'   depresion = c(rnorm(50, 15, 3), rnorm(50, 15, 3))
#' )
#' Calcule_Comparative(df, cols = c("ansiedad", "depresion"), group_var = "grupo")
Calcule_Comparative <- function(data, cols, group_var, Robust = FALSE) {
  if (isTRUE(Robust) && !requireNamespace("WRS2", quietly = TRUE)) {
    stop("Robust = TRUE needs the 'WRS2' package: install.packages(\"WRS2\").",
         call. = FALSE)
  }

  grupo <- factor(data[[group_var]])
  group_values <- levels(droplevels(grupo[!is.na(grupo)]))
  if (length(group_values) != 2) {
    stop("'", group_var, "' must have exactly two groups; it has ",
         length(group_values), ".", call. = FALSE)
  }

  one_variable <- function(col) {
    y <- data[[col]]
    g <- factor(data[[group_var]], levels = group_values)
    y1 <- y[g == group_values[1] & !is.na(g)]
    y2 <- y[g == group_values[2] & !is.na(g)]
    y1 <- y1[!is.na(y1)]
    y2 <- y2[!is.na(y2)]

    tt <- stats::t.test(y1, y2, var.equal = FALSE)

    d <- if (isTRUE(Robust)) {
      WRS2::akp.effect(y ~ g, data = data.frame(y = c(y1, y2),
                                                g = rep(group_values, c(length(y1), length(y2)))),
                       EQVAR = FALSE)$AKPeffect
    } else {
      n1 <- length(y1); n2 <- length(y2)
      sp <- sqrt(((n1 - 1) * stats::var(y1) + (n2 - 1) * stats::var(y2)) / (n1 + n2 - 2))
      (mean(y1) - mean(y2)) / sp
    }

    data.frame(
      Variables_interes = col,
      M1 = paste0(round(mean(y1), 2), " (", round(stats::sd(y1), 2), ")"),
      M2 = paste0(round(mean(y2), 2), " (", round(stats::sd(y2), 2), ")"),
      t = unname(tt$statistic),
      gl = unname(tt$parameter),
      p = tt$p.value,
      d_cohen = d,
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, lapply(cols, one_variable))
  out$Variables_interes <- factor(out$Variables_interes, levels = cols)
  out$Interpretacion <- dplyr::case_when(
    abs(out$d_cohen) > 0.80 ~ "Grande",
    abs(out$d_cohen) > 0.50 ~ "Mediano",
    abs(out$d_cohen) > 0.30 ~ "Peque\u00f1o",
    TRUE ~ "Trivial"
  )
  names(out)[names(out) == "M1"] <- paste0(group_values[1], "(SD1)")
  names(out)[names(out) == "M2"] <- paste0(group_values[2], "(SD2)")
  tibble::as_tibble(out)
}

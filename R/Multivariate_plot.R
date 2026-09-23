#' Q-Q Plot of Mahalanobis Distances with Mardia's Coefficients
#'
#' Plots the squared Mahalanobis distances of the observations against the
#' quantiles of a chi-square distribution (a check of multivariate normality)
#' and overlays a table with Mardia's skewness and kurtosis tests from
#' [mardia_test()].
#'
#' @param data A data frame or matrix of numeric variables.
#' @param xmin,xmax,ymin,ymax Position of the Mardia table inside the plot, in
#'   the units of the axes.
#'
#' @return A ggplot object.
#' @export
#' @examples
#' set.seed(1)
#' df <- as.data.frame(matrix(rnorm(200 * 4), ncol = 4))
#' Multivariate_plot(df, xmin = 8, xmax = 14, ymin = 1, ymax = 5)
Multivariate_plot <- function(data, xmin = 30, xmax = 40, ymin = 2, ymax = 7) {
  Descrip_fiabilidad <- as.matrix.data.frame(mardia_test(data))

  tt2 <- gridExtra::ttheme_default(
    core = list(bg_params = list(fill = c("#F2F3F4", "#F2F3F4", "#F2F3F4"), col = NA),
                fg_params = list(fontface = 1)),
    colhead = list(fg_params = list(col = "black", fontface = c(1, 1, 1, 1, 1))),
    rowhead = list(fg_params = list(col = "black", fontface = 1)), base_size = 8)

  distancias <- stats::mahalanobis(data, colMeans(data), stats::cov(data))
  teorico <- stats::qchisq(stats::ppoints(length(distancias)), df = ncol(data))
  df1 <- data.frame(Teorico = teorico, Observado = sort(distancias))

  tabla <- gridExtra::tableGrob(Descrip_fiabilidad, rows = NULL, theme = tt2)
  tabla <- gtable::gtable_add_grob(tabla,
                                   grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 2)),
                                   t = 2, b = nrow(tabla), l = 1, r = ncol(tabla))
  tabla <- gtable::gtable_add_grob(tabla,
                                   grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 2)),
                                   t = 1, l = 1, r = ncol(tabla))

  ggplot2::ggplot(df1, ggplot2::aes(x = Observado, y = Teorico)) +
    ggplot2::geom_point(shape = 19, color = "gray20", size = 2) +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1.2) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Q-Q Plot de Distancias de Mahalanobis",
                  x = "Squared Mahalanobis Distance", y = "Chi-Square Quantile") +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 12),
                   axis.title = ggplot2::element_text(size = 14)) +
    ggplot2::annotation_custom(tabla, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
}

#' Boxplots of Several Variables
#'
#' Draws one boxplot per variable, each in its own panel with its own scale,
#' to inspect the distribution and outliers of the scores.
#'
#' @param data A data frame.
#' @param cols Character vector with the names of the variables.
#'
#' @return A ggplot object.
#' @export
#' @examples
#' set.seed(1)
#' df <- data.frame(ansiedad = rnorm(60, 20, 4), depresion = rexp(60, 0.2))
#' grafico_boxplots(df, c("ansiedad", "depresion"))
grafico_boxplots <- function(data, cols) {
  dat.m <- data %>%
    mutate(ID = seq_len(nrow(data))) %>%
    select(ID, all_of(cols)) %>%
    tidyr::pivot_longer(all_of(cols), names_to = "Variables", values_to = "value") %>%
    mutate(Variables = factor(Variables, levels = cols))

  ggplot2::ggplot(dat.m) +
    ggplot2::geom_boxplot(ggplot2::aes(x = ID, y = value, fill = Variables)) +
    ggplot2::facet_wrap(~Variables, scales = "free") +
    ggplot2::theme_bw() +
    ggplot2::xlab(" ") +
    ggplot2::theme(legend.position = "none")
}

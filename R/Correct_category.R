Correct_category <- function(df, cols, ...) {
  library(dplyr)
  # 1. Capturamos los reemplazos en una lista
  replacements <- list(...)

  # 2. Comprobamos que todos sean fórmulas de largo 3 (lhs ~ rhs)
  if (!all(vapply(replacements, function(f) inherits(f, "formula") && length(f) == 3, logical(1)))) {
    stop('Cada reemplazo debe ser una fórmula bidireccional: "viejo" ~ "nuevo".')
  }

  df %>%
    mutate(
      across(
        {{cols}},
        function(x) {
          vec <- x
          for (f in replacements) {
            # extraemos los valores viejo y nuevo
            old_val <- eval(f[[2]], envir = parent.frame())
            new_val <- eval(f[[3]], envir = parent.frame())
            vec[vec == old_val] <- new_val
          }
          vec
        }
      )
    )
}

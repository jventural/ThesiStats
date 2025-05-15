clean_edad <- function(df, col_name = "Edad") {
  # Instala y carga dplyr
  if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
  library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
  # Instala y carga stringr
  if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
  library(stringr, quietly = TRUE, warn.conflicts = FALSE)

  df %>%
    mutate(
      across(
        all_of(col_name),
        ~ str_remove(.x, " años$") %>% as.numeric()
      )
    )
}

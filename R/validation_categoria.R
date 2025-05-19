validation_categoria <- function(df, cols) {
  library(dplyr)
  df %>%
    select({{cols}}) %>%
    unlist(use.names = FALSE) %>%      # aplanar a un vector
    unique() %>%                       # únicas
    sort()                             # ordenar
}

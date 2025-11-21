calcular_correlaciones <- function(data, columna_inicial, columna_final, method = c("spearman", "pearson"), winsorize = FALSE, show_pval = FALSE) {

  # Función para instalar y cargar librerías
  install_and_load <- function(package) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package)
    }
    library(package, character.only = TRUE)
  }

  # Instalar y cargar las librerías requeridas
  install_and_load("dplyr")
  install_and_load("WRS2")

  # Asegurar que method sea válido
  method <- match.arg(method)

  # Seleccionar las columnas
  column_range <- which(names(data) %in% c(columna_inicial, columna_final))
  data <- data[, min(column_range):max(column_range)]

  # Función auxiliar para agregar asteriscos en función del p-valor
  add_significance <- function(cor_values, p_values) {
    significance <- ifelse(p_values > 0.05, "ns",
                           ifelse(p_values <= 0.05 & p_values > 0.01, "*",
                                  ifelse(p_values <= 0.01 & p_values > 0.001, "**",
                                         ifelse(p_values <= 0.001 & p_values > 0.0001, "***", "****"))))
    cor_with_significance <- paste0(format(cor_values, nsmall = 2), significance)
    return(matrix(cor_with_significance, nrow = nrow(cor_values), dimnames = dimnames(cor_values)))
  }

  # Calcular la correlación y p-valores
  if (winsorize && method == "pearson") {
    win_results <- WRS2::winall(data, tr = 0.2)
    cor_values <- round(win_results$cor, 2)
    p_values <- format(win_results$p.values, scientific = FALSE, digits = 4) # Evitar notación científica
  } else {
    cor_values <- round(cor(data, method = method), 2)

    # Calcular p-valores para cualquier método (Pearson o Spearman)
    n_vars <- ncol(data)
    p_values <- matrix(NA, nrow = n_vars, ncol = n_vars)
    rownames(p_values) <- colnames(data)
    colnames(p_values) <- colnames(data)

    for (i in 1:(n_vars - 1)) {
      for (j in (i + 1):n_vars) {
        tryCatch({
          test_result <- cor.test(data[[i]], data[[j]], method = method, exact = FALSE)
          p_values[i, j] <- test_result$p.value
          p_values[j, i] <- test_result$p.value
        }, error = function(e) {
          p_values[i, j] <- NA
          p_values[j, i] <- NA
        })
      }
    }

    p_values <- format(p_values, scientific = FALSE, digits = 4)
  }

  # Aplicar significancia si `show_pval` es TRUE
  if (show_pval && !is.null(p_values)) {
    result_cor <- add_significance(cor_values, p_values)
  } else {
    result_cor <- cor_values
  }

  # Convertir la diagonal principal en NA
  result_cor <- diag_aba_na(result_cor)

  # Convertir p-values a NA en la diagonal, si existen
  if (!is.null(p_values)) {
    p_values <- diag_aba_na(p_values)
  }

  # Retornar resultados como lista
  return(list(correlation = as.data.frame(result_cor), p_values = as.data.frame(p_values)))
}

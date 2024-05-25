mardia_test <- function(data) {
  # Función para instalar y cargar librerías
  install_and_load <- function(package) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package)
    }
    library(package, character.only = TRUE)
  }

  # Instalar y cargar las librerías requeridas
  install_and_load("psych")
  install_and_load("dplyr")

  A <- mardia(data, plot=F)

  # Crear el data frame de resultados
  result <- data.frame(
    Test = c("Mardia Skewness", "Mardia Kurtosis"),
    Statistic = c(A$small.skew, A$kurtosis),
    p.value = c(A$p.small, A$p.kurt),
    Result = c(ifelse(A$p.small < 0.05, "NO", "YES"), ifelse(A$p.kurt < 0.05, "NO", "YES"))
  )

  # Ajustar primero los valores p y luego aplicar el formato condicional
  result$p.value <- round(result$p.value, 3)  # Redondear todos los valores de p

  # Aplicar la condición de formato especial después del redondeo
  result$p.value <- ifelse(result$p.value <= 0.001, "p < .001", as.character(result$p.value))

  return(result)
}

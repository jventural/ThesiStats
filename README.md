<p align="center">
  <img src="https://github.com/jventural/ThesiStats/blob/master/Logo_ThesiStats3.png" alt="ThesiStats" width="200" height="200"/>
</p>

<h1 align="center">ThesiStats</h1>

<p align="center">
    Una librería que ayuda en el preprocesamiento y limpieza de datos en el contexto de las tesis.
</p>

<!-- BADGES -->
<p align="center">
  <!-- Si tienes badges, por ejemplo de CRAN, puedes incluirlos aquí: -->
  <img src="https://www.r-pkg.org/badges/version/ThesiStats" alt="CRAN version"/>
</p>


# Instalación
Puedes instalar la última versión de ThesiStats desde GitHub con ayuda del paquete `devtools`:
```r
if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("jventural/ThesiStats")
```
# Tutorial básico de la librería de R ThesiStats
Para conocer más sobre el funcionamiento de esta librería, haga clic [aquí](https://rpubs.com/jventural/Tutorial_ThesiStats)


# Algunas de las funciones de la librería
## Omega coefficient calculation using ThesiStats library
Esta función calcula la confiabilidad de todos los factores de un test en un conjunto de datos. Utiliza modelos de ecuaciones estructurales para estimar la confiabilidad compuesta, conocida como Omega, para cada factor especificado en el objeto extracted. Es útil en el análisis preliminar de demostrar la fiabilidad de los instrumentos antes de hacer estudios de correlación.
```r
extracted <- extract_items(text)
final_result <- calcula_omega_all(extracted, data)
print(final_result)
```
----
## Correlation matrix calculation using ThesiStats library
Esta función calcula una matriz de correlación utilizando métodos de Pearson, Spearman o Pearson Winsorizado. Está diseñada para trabajar con un rango de columnas dentro de un conjunto de datos, permitiendo un análisis flexible de las relaciones entre variables.

```r
resultado <- calcular_correlaciones(df_new_renombrado, "Ansiedad de separación", "Búsqueda de atención", method = "pearson", winsorize = TRUE)
print(resultado)
```
----

## Percentage distribution calculation using ThesiStats library
Esta función calcula la distribución porcentual para columnas especificadas dentro de un conjunto de datos. Es útil para entender las frecuencias relativas de diferentes categorías o valores en cada variable.

```r
# Suponiendo que 'data' es un data frame y 'columnas' es un vector de nombres de columnas
porcentajes <- calcular_porcentajes(data, columnas)
print(porcentajes)
```

----
## Likert Scale Detection using ThesiStats library
Esta función examina un data frame para encontrar expresiones únicas de escalas Likert consolidando todos los valores de las columnas en una sola columna e identificando entradas únicas. Es útil para reconocer respuestas estandarizadas en datos de encuestas.

```r
# Suponiendo que 'survey_data' es un data frame con múltiples preguntas de escala Likert
likert_expressions <- detect_Likert(survey_data)
print(likert_expressions)

```


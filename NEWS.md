# ThesiStats 1.1.0

First version submitted to CRAN.

## Breaking change

* `generate_and_apply()` no longer writes the result into the global
  environment; it returns the data frame. Assign it:
  `df2 <- generate_and_apply(df, lines)`. The `new_name` argument is kept but
  ignored.

## Bug fixes

* `Calcule_Comparative()` labelled the groups in order of appearance while
  `t.test()` orders them alphabetically, so a table could show one group's
  mean with the other group's label and standard deviation. Both now follow
  `levels(factor(group))`. Cohen's d is computed directly (pooled SD); the
  robust option still uses `WRS2::akp.effect()`.
* `calcula_omega_all()` failed and `Fiabilidad()` returned a list with recent
  semTools versions, where `compRelSEM()` returns a list; both return numbers
  again.
* `u_mann_whitney_superioridad()` labelled large effects in favour of the
  second group (PS well below .50) as "No efecto"; the size now uses
  max(PS, 1 - PS). Cases with missing values are excluded before counting n.
* `calcular_correlaciones()` assigned significance marks by comparing p values
  as text; it now uses the numeric p values.
* `normalize_carreras(remove_unmatched = TRUE)` failed because it used the
  output before creating it.
* Decimal commas ("3,5") were read as thousands separators (35) in
  `clean_edad()`, `convert_age_to_years()` and
  `convert_age_to_years_months()`; "un año" is now recognized as 1 year.
* `epsilon_cuadrado_kruskal()` counts only the cases used by the test.

## CRAN compliance

* No function installs or attaches packages at run time; dependencies are
  declared in DESCRIPTION and imported in the NAMESPACE.
* Documentation generated with roxygen2; every function has a runnable
  example and the two datasets are documented.
* Progress is reported with `message()` instead of `print()`.
* Non-ASCII characters in the code are escaped.

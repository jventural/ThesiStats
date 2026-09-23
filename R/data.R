#' Catalog of Peruvian University Degrees
#'
#' Reference list of undergraduate degrees and their faculties, used by
#' [normalize_carreras()] to standardize free-text degree names.
#'
#' @format A tibble with 93 rows and 2 columns:
#' \describe{
#'   \item{Carrera}{Standard name of the degree.}
#'   \item{Facultad}{Faculty the degree belongs to.}
#' }
#' @source Compiled by the package author from the degree offer of Peruvian
#'   universities.
"carreras_peruanas"

#' Catalog of Peruvian Universities
#'
#' Reference list of Peruvian universities and their acronyms, used by
#' [normalize_universidades()].
#'
#' @format A tibble with 99 rows and 2 columns: the full name of the
#'   university (`Nombre`) and its acronym (second column).
#' @source Compiled by the package author from public lists of Peruvian
#'   universities.
"universidades_peruanas"

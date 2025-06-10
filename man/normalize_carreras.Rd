\name{normalize_carreras}
\alias{normalize_carreras}
\title{Normalize Academic Program Names and Faculties to Standard Labels}
\description{
Standardizes academic program names in a data frame, assigning each entry its official label and, optionally, the corresponding faculty, based on the internal \code{ThesiStats::carreras_peruanas} catalog. It employs multiple fuzzy-matching strategies (Jaro–Winkler, Levenshtein, cosine, Jaccard) and a preliminary manual dictionary to maximize coverage of name variants.
}
\usage{
normalize_carreras(df,
col_name = "Carrera",
max_dist = 0.15,
facultad = FALSE,
manual_path = NULL,
force_match = FALSE,
fallback_dist = 0.4,
remove_unmatched = FALSE)
}
\arguments{
\item{df}{A \code{data.frame} or \code{tibble} containing the column with program names to normalize.}
\item{col_name}{Name of the column in \code{df} with the original program names; defaults to \code{"Carrera"}.}
\item{max_dist}{Maximum allowed distance (Jaro–Winkler or other methods) for a valid match on the first pass; defaults to \code{0.15}.}
\item{facultad}{Logical; if \code{TRUE}, in addition to \code{<col_name>_norm}, adds \code{<col_name>_facultad} with the associated faculty.}
\item{manual_path}{Path to an Excel file containing a manual recoding dictionary with columns \code{raw} and \code{normalized}; if \code{NULL}, no manual recoding is applied.}
\item{force_match}{Logical; if \code{TRUE}, retries matching for unmatched entries using a more permissive threshold (\code{fallback_dist}) and keyword-based strategies.}
\item{fallback_dist}{Maximum distance for forced matching (only used if \code{force_match = TRUE}); defaults to \code{0.4}.}
\item{remove_unmatched}{Logical; if \code{TRUE}, removes rows that could not be normalized instead of leaving them as \code{NA}.}
}
\details{
The function performs the following steps:
\enumerate{
\item Installs (if missing) and loads the required packages: \pkg{dplyr}, \pkg{stringr}, \pkg{stringdist}, \pkg{stringi}, and \pkg{readxl}.
\item Loads the internal catalog \code{ThesiStats::carreras_peruanas} and preprocesses its names (lowercase, ASCII conversion, punctuation removal).
\item If \code{manual_path} is provided, reads the Excel dictionary, cleans its keys, and constructs a named vector \code{dic_manual}.
\item Preprocesses the original column in the same way as the catalog and applies manual recoding where applicable.
\item For each prepared entry, computes fuzzy-matching distances using several methods and selects the best match within \code{max_dist}.
\item If \code{force_match = TRUE}, reattempts matching on initially unmatched entries using \code{fallback_dist} and also attempts keyword-based matches.
\item Builds the \code{<col_name>_norm} column with normalized names and, if \code{facultad = TRUE}, the \code{<col_name>_facultad} column.
\item If \code{remove_unmatched = TRUE}, removes rows with no normalization; otherwise, reports which entries remain \code{NA} and suggests possible matches.
}
}
\value{
A \code{data.frame} identical to the input, with these additional columns:
\item{\code{<col_name>_norm}}{Character vector of normalized program names (or \code{NA} if no match).}
\item{\code{<col_name>_facultad}}{(Only if \code{facultad = TRUE}) Character vector of the associated faculty (or \code{NA}).}
}
\examples{

# Basic example: standard fuzzy matching only
df <- data.frame(
Carrera = c("Psicologia", "Ing. sistemas", "cosmetologa"),
stringsAsFactors = FALSE
)
normalize_carreras(df)

# Include faculty information
normalize_carreras(df, facultad = TRUE)

# Use a manual dictionary stored in Excel (with columns raw and normalized)
saved at "data/dic_manual.xlsx"
normalize_carreras(df,
manual_path = "data/dic_manual.xlsx",
max_dist = 0.2)

# Force more permissive matching and remove unmatched rows
normalize_carreras(df,
force_match = TRUE,
fallback_dist = 0.5,
remove_unmatched = TRUE)
}
\author{Dr. José Ventura-León}

\name{process_likert_blocks}
\alias{process_likert_blocks}
\title{Batch Convert and Replace Likert-Scale Blocks in a Data Frame}
\description{
This function processes multiple blocks of Likert-type items in a data frame by detecting response expressions, converting them to numeric or factor codes, and then replacing the original items with the standardized expressions. It is especially useful when you have several sections (blocks) of survey items sharing the same response levels but different prefixes or lengths.
}
\usage{
process_likert_blocks(df, specs)
}
\arguments{
\item{df}{A data frame or tibble containing the raw Likert-scale item columns.}
\item{specs}{A list of specification lists, one per block. Each specification must include:
\describe{
\item{\code{prefix}}{Character: common column name prefix (e.g., “AU”) for that block.}
\item{\code{n_items}}{Integer: number of items in the block (columns \code{prefix}1 through \code{prefix}\eqn{n_items}).}
\item{\code{levels}}{Character vector: ordered Likert response labels (e.g., \code{c("Muy en desacuerdo", ..., "Muy de acuerdo")}).}
\item{\code{start_zero}}{Logical: whether the first response level is coded as zero (TRUE) or one (FALSE).}
}
}
}
\details{
\code{process_likert_blocks} works in three steps:
\enumerate{
\item For each block specification, it generates the full set of column names (\code{prefix}1 ... \code{prefix}\eqn{n_items}), verifies their presence, and then applies \code{detect_expression_Likert()} (from \pkg{ThesiStats}) followed by \code{convert_to_expresions()} to standardize responses.
\item It constructs a mapping of column ranges to converted expression data frames in the format required by \code{remplace_alternative_response()}.
\item It calls \code{remplace_alternative_response()} to replace the original item columns in \code{df} with their converted versions, preserving other columns unchanged.
}
}
\value{
A data frame identical to the input \code{df} except that each specified block of Likert items has been replaced by the standardized, coded expressions.
}
\examples{

Define one block of 10 items with prefix "AU"
specs <- list(
list(
prefix = "AU",
n_items = 10,
levels = c("Muy en desacuerdo", "En desacuerdo",
"De acuerdo", "Muy de acuerdo"),
start_zero = FALSE
)
)

Apply to a data frame 'df_new_renombrado'
df_new_renombrado2 <- process_likert_blocks(df_new_renombrado, specs)
}
\author{
Dr. José Ventura-León

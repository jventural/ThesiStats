## Submission summary

New submission of ThesiStats (version 1.1.0): helpers for the analyses that
quantitative theses in the social and behavioral sciences repeat (item
scoring, Likert recoding, cleaning of Spanish-language sociodemographic
variables, descriptives, normality, omega reliability, correlation tables and
group comparisons with effect sizes).

## Test environments

* Local: Windows 11 x64, R 4.4.1 (R CMD check --as-cran --run-donttest,
  PDF manual built with pdflatex)
* win-builder: R-devel

## R CMD check results

<RESULTADOS>

## Notes for the reviewer

* The package includes two small reference datasets (Peruvian degrees and
  universities) with UTF-8 strings, used to normalize free-text answers.
* All examples use simulated or inline data. The examples that fit ordinal
  confirmatory factor models are wrapped in \donttest{}.
* No function installs packages, writes files or modifies the global
  environment.

## Downstream dependencies

There are currently no downstream dependencies.

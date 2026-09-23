#' Rename Columns by Position
#'
#' Renames the columns at the positions given in `columns` with `new_names`.
#'
#' @param df A data frame.
#' @param new_names Character vector with the new names.
#' @param columns Integer vector with the positions of the columns to rename,
#'   in the same order as `new_names`.
#'
#' @return `df` with the columns renamed.
#' @export
#' @examples
#' df <- data.frame(a = 1:2, b = 3:4, c = 5:6)
#' rename_columns(df, c("Edad", "Sexo"), 1:2)
rename_columns <- function(df, new_names, columns) {
  old_names <- names(df)[columns]
  names_dict <- stats::setNames(old_names, new_names)
  rename(df, !!!names_dict)
}

#' Rename a Block of Item Columns with One Prefix
#'
#' Renames the consecutive columns from `inicio` to `final` as `prefix1`
#' followed by 1, 2, 3, ...
#'
#' @param df A data frame.
#' @param prefix1 Prefix of the new names.
#' @param inicio,final Names of the first and last columns of the block.
#'   Defaults to the first and last columns of `df`.
#' @param n_items1 Number of items; if given, it must equal the number of
#'   columns in the block.
#'
#' @return `df` with the block renamed.
#' @export
#' @examples
#' df <- data.frame(id = 1:2, p1 = 1:2, p2 = 3:4, p3 = 5:6)
#' rename_items_only(df, prefix1 = "ANS", inicio = "p1", final = "p3")
rename_items_only <- function(df, prefix1 = "COPE", inicio = NULL, final = NULL, n_items1 = NULL) {
  inici_idx <- if (!is.null(inicio)) which(colnames(df) == inicio) else 1
  final_idx <- if (!is.null(final)) which(colnames(df) == final) else ncol(df)

  n_vars <- final_idx - inici_idx + 1
  if (is.null(n_items1)) n_items1 <- n_vars
  if (n_items1 != n_vars) {
    stop("El n\u00famero de columnas a renombrar no coincide con el n\u00famero de nombres nuevos.")
  }

  colnames(df)[inici_idx:final_idx] <- paste0(prefix1, seq_len(n_items1))
  df
}

#' Rename a Block of Item Columns with Two Prefixes
#'
#' Renames the consecutive columns from `inicio` to `final`: the first
#' `n_items1` as `prefix1` 1, 2, ... and the next `n_items2` as `prefix2` 1,
#' 2, ... If the counts are not given, the block is split in halves.
#'
#' @inheritParams rename_items_only
#' @param prefix2 Prefix of the second group of items.
#' @param n_items2 Number of items of the second group.
#'
#' @return `df` with the block renamed.
#' @export
#' @examples
#' df <- data.frame(p1 = 1, p2 = 2, p3 = 3, p4 = 4, p5 = 5)
#' rename_items2(df, prefix1 = "ANS", prefix2 = "DEP", n_items1 = 3)
rename_items2 <- function(df, prefix1 = "COPE", prefix2 = "E", inicio = NULL, final = NULL,
                          n_items1 = NULL, n_items2 = NULL) {
  inicio_idx <- if (!is.null(inicio)) which(colnames(df) == inicio) else 1
  final_idx <- if (!is.null(final)) which(colnames(df) == final) else ncol(df)
  n_vars <- final_idx - inicio_idx + 1

  if (is.null(n_items1) && is.null(n_items2)) {
    n_items1 <- ceiling(n_vars / 2)
    n_items2 <- n_vars - n_items1
  } else if (!is.null(n_items1) && is.null(n_items2)) {
    n_items2 <- n_vars - n_items1
  } else if (is.null(n_items1) && !is.null(n_items2)) {
    n_items1 <- n_vars - n_items2
  }

  nuevos_nombres <- c(paste0(prefix1, seq_len(n_items1)), paste0(prefix2, seq_len(n_items2)))

  if (n_vars != length(nuevos_nombres)) {
    stop("El n\u00famero de columnas a renombrar no coincide con el n\u00famero de nombres nuevos.")
  }

  colnames(df)[inicio_idx:final_idx] <- nuevos_nombres
  df
}

#' Rename a Block of Item Columns with Three Prefixes
#'
#' Like [rename_items2()] with three groups of items. If the counts are not
#' given, the block is split in thirds.
#'
#' @inheritParams rename_items2
#' @param prefix3 Prefix of the third group of items.
#' @param n_items3 Number of items of the third group.
#'
#' @return `df` with the block renamed.
#' @export
#' @examples
#' df <- data.frame(p1 = 1, p2 = 2, p3 = 3, p4 = 4, p5 = 5, p6 = 6)
#' rename_items3(df, prefix1 = "A", prefix2 = "B", prefix3 = "C")
rename_items3 <- function(df, prefix1 = "COPE", prefix2 = "E", prefix3 = "F", inicio = NULL,
                          final = NULL, n_items1 = NULL, n_items2 = NULL, n_items3 = NULL) {
  inicio_idx <- if (!is.null(inicio)) which(colnames(df) == inicio) else 1
  final_idx <- if (!is.null(final)) which(colnames(df) == final) else ncol(df)
  n_vars <- final_idx - inicio_idx + 1

  if (is.null(n_items1) && is.null(n_items2) && is.null(n_items3)) {
    n_items1 <- ceiling(n_vars / 3)
    n_items2 <- ceiling((n_vars - n_items1) / 2)
    n_items3 <- n_vars - n_items1 - n_items2
  } else if (!is.null(n_items1) && is.null(n_items2) && is.null(n_items3)) {
    n_items2 <- ceiling((n_vars - n_items1) / 2)
    n_items3 <- n_vars - n_items1 - n_items2
  } else if (!is.null(n_items1) && !is.null(n_items2) && is.null(n_items3)) {
    n_items3 <- n_vars - n_items1 - n_items2
  }

  nuevos_nombres <- c(paste0(prefix1, seq_len(n_items1)), paste0(prefix2, seq_len(n_items2)),
                      paste0(prefix3, seq_len(n_items3)))

  if (n_vars != length(nuevos_nombres)) {
    stop("El n\u00famero de columnas a renombrar no coincide con el n\u00famero de nombres nuevos.")
  }

  colnames(df)[inicio_idx:final_idx] <- nuevos_nombres
  df
}

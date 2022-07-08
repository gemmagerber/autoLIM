#' export_fun() part of prepack::multinets function
#' Grabs nodes that have export flows across the boundary
#'
#' @param x The flow matrix
#'
export_fun <- function (x) {
  row.i <-
    grep(
      rownames(x),
      pattern = "Input|Export|CO2",
      value = TRUE,
      invert = TRUE
    )
  col.j <-
    grep(
      colnames(x),
      pattern = "Export",
      value = TRUE,
      invert = FALSE,
      ignore.case = TRUE
    )
  export.matrix <- x[c(row.i), c(col.j)]
  naked.t <- t(replace(export.matrix, export.matrix > 0, 1))
  export.vector <-
    as.vector(diag(export.matrix %*% t(
      replace(export.matrix, export.matrix > 0, 1)
    )))
}

#' fmat_fun, part of multinets function
#' Extracts f matrices (only internal matrices excluding boundary flows)
#' @param x an object?
#'
#'
fmat_fun <- function(x) {
  flow.matrix <-
    x[c(
      grep(
        rownames(x),
        pattern = "Input|Export|CO2",
        value = TRUE,
        invert = TRUE,
        ignore.case = TRUE
      )
    ), c(
      grep(
        colnames(x),
        pattern = "Input|Export|CO2",
        value = TRUE,
        invert = TRUE,
        ignore.case = TRUE
      )
    )]
}

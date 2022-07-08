#' part of prepack::multinets function
#' Grabs output
#'
#' @param x the flow matric
#'
output_fun <- function (x) {
  # Create output vector function
  # output.fun <- function (a) {
  #   respiration <- resp.fun(a)
  #   export <- export.fun(a)
  #   output.vector <- respiration + export
  # }
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
      pattern = "Export|CO2",
      value = TRUE,
      invert = FALSE,
      ignore.case = TRUE
    )
  output.vector <-
    as.vector(colSums(t(x[c(row.i), c(col.j)]), na.rm = FALSE))
}

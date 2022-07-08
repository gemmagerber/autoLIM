#' resp_fun() part of prepack::multinets function
#' Grabs internal, living nodes that respire
#'
#' @param x The flow matrix
#'
resp_fun <- function(x) {
  resp.vector <-
    as.vector(x[c(grep(
      rownames(x),
      pattern = "Input|Export|CO2",
      value = TRUE,
      invert = TRUE
    )), c(grep(
      colnames(x),
      pattern = "CO2",
      value = TRUE,
      invert = FALSE,
      ignore.case = TRUE
    ))])

}

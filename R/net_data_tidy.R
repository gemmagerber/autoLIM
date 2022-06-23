#' net_data_tidy
#' Tidies up network data input list
#' @param x the sheet from network data input
#' @param NLNode the defined NLNodes from the main autoLIMR argument
#'
#' @export
#'
net_data_tidy <- function (x, NLNode) {
  x <- as.matrix(x)
  rownames(x) <- x[, 1] # Make Compartment Name the Row Name
  x <- x[, -1] # Make Compartment Name the Row Name
  x <- NLNode_mat(x, NLNode = NLNode)

  x <-
    apply(
      X = x,
      MARGIN = 2,
      FUN = gsub,
      pattern = ",",
      replace = "."
    )   # Substitute commas for periods
  x <-
    x[!x[, "Biomass"] == "0" |
        !x[, "Biomass"] == 0 , ] # Drop any rows that have biomass of zero or NA
  x <-
    x[!is.na(x[, "Biomass"]),]

  x <- sci_notation_off(x)
  x <-
    apply(
      X = x,
      MARGIN = 2,
      FUN = gsub,
      pattern = " ",
      replace = ""
    ) # Substitute spaces for nothing
  x
}

# Function: Tidy up adj mat sheets
#' Title
#' Tidies up adjacency matrix/matrices input
#' @param x the sheet from network data input
#' @param NLNode the defined NLNodes from the main autoLIMR argument
#'
#' @export
#'
adj_mat_tidy <- function (x, NLNode) {
  x <- as.matrix(x)
  rownames(x) <- x[, 1] # Make Compartment Name the Row Name
  x <- x[,-1] # Make Compartment Name the Row Name
  rownames(x) <- gsub(rownames(x), pattern = " ", replacement = ".")
  x <- sci_notation_off(x)
  x <-
    apply(
      X = x,
      MARGIN = 2,
      FUN = gsub,
      pattern = " ",
      replace = ""
    ) # Substitute spaces for nothing
  x <- NLNode_mat(x, NLNode = NLNode)
  return(x)
}

# Function:
#' matrix_def
#' Return matrix based on search_cols function
#' @param x the matrix
#' @param mat.type the matrix type to export
#'
#' @export
#'
matrix_def <- function (x, mat.type) {
  if (mat.type == "Input") {
    col.j.in <- search_cols(x, col.match = "Input")
    in.mat <-
      x[c(rownames(x)), c(col.j.in)] # create input matrix based on exports only
    input.matrix <-
      in.mat[rowSums(is.na(in.mat)) != ncol(in.mat), ] # Drop rows where all are NA
    return(input.matrix)
  }

  if (mat.type == "Export") {
    col.j.ex <- search_cols(x, col.match = "Export")
    x <-
      x[c(rownames(x)), c(col.j.ex)] # create export matrix based on exports only
    x <-
      x[rowSums(is.na(x)) != ncol(x), ] # Drop rows where all are NA
    return(x)

  }

}

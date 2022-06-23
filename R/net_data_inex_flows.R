# Function :
#' Input and Export flows definition
#' Defines input and export flows from network input data workbook
#' @param x network data input matrix

#' @export
#'

net_data_inex_flows <-  function (x) {
  ex.mat2 <- matrix_def(x, mat.type = "Export")
  in.mat2 <- matrix_def(x, mat.type = "Input")

  x <- c(
    "! Input flows",
    "",
    paste0(
      rownames(in.mat2),
      "_IN: ",
      rownames(in.mat2),
      "Input",
      " -> ",
      rownames(in.mat2)
    ),
    "",
    "! Export flows",
    "",
    paste0(
      rownames(ex.mat2),
      "_EX: ",
      rownames(ex.mat2),
      " -> ",
      rownames(ex.mat2),
      "Export"
    ),
    ""
  )
  return(x)
}

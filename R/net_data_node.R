#' net_data_node
#' define list of living/NL nodes/input/export
#' @param x matrix/matrices
#' @param node.type living/nonliving/Input/Export
#' @param NLNode the defined NLNodes from the main autoLIMR argument
#' @export
#'
net_data_node <- function (x, node.type, NLNode) {
  if (node.type == "living") {
    LN <- grep(
      as.vector(rownames(x)),
      pattern = paste0(NLNode, "NLNode", collapse = "|"),
      value = TRUE,
      invert = TRUE,
      ignore.case = FALSE
    )
    return(LN)
  }
  if (node.type == "nonliving") {
    NLN <- grep(
      as.vector(rownames(x)),
      pattern = paste0(NLNode, "NLNode", collapse = "|"),
      value = TRUE,
      invert = FALSE,
      ignore.case = FALSE
    )
    return(NLN)
  }

  if (node.type == "Input") {
    input.matrix <- matrix_def(x, mat.type = "Input")
    in.mat3 <- as.vector(paste0(rownames(input.matrix), "Input"))
    return(in.mat3)
  }

  if (node.type == "Export") {
    export.matrix <- matrix_def(x, mat.type = "Export")
    export.nodes <-
      as.vector(paste0(rownames(export.matrix), "Export"))
    return(export.nodes)

  }

}

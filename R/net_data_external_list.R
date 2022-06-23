# Function:
#' net_data_external_list
#' define externals list
#' @param x net data input matrix/matrices
#' @param respiration Respiration = TRUE as defined in main autoLIMR function
#' @param respiration_element if Respiration = TRUE, the respiration element to be defined. Default to "CO2"
#'
#' @export
#'
net_data_external_list <- function (x, respiration,
                                    respiration_element) {
  exports <- net_data_node(x, node.type = "Export")
  inputs <- net_data_node(x, node.type = "Input")

  if (respiration == TRUE) {
    if (!is.null(respiration_element)) {
      resp.vec <- as.vector(paste0(toupper(respiration_element)))
    } else {
      resp.vec <- as.vector(paste0("CO2"))
    }
  } else {
    resp.vec <- as.vector(paste0(""))
  }

  returnme <-
    c(resp.vec,
      sort(inputs),
      sort(exports))
  return(returnme)

}

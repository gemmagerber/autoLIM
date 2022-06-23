#' error_print
#' Print Errors for undefined sheets
#' @param net_data_input the user-defined workbook with network input data
#' @param adj_mat_input the user-defined workbook with adjacency matrices
#' @export
error_print <-
  function(net_data_input,
           adj_mat_input) {
    if (is.null(net_data_input)) {
      stop(
        "Please provide the workbook filename and file extension
      containing biomass and inequalities ('net_data_input' argument)."
      )
    }
    if (is.null(adj_mat_input)) {
      stop(
        "Please provide the workbook filename and file extension
      containing adjacency matrices ('adj_mat_input' argument)."
      )
    }
    if (!is.null(net_data_input) &
        !is.null(adj_mat_input)) {
      message("Good to go!")
    }
  }

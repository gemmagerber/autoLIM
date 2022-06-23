#' Unused material/energy variable definition without exports
#'
#' @param x matrices defined from Network Data Input workbook
#' @param respiration If respiration = TRUE in main autoLIMR argument
#' @export
#'
uvar_wo_ex <- function(x, respiration) {
  if (!is.null(respiration) | respiration == FALSE) {
    wo.ex_uvar <- paste0(x,
                         "_U = ",
                         "Flowto(",
                         x,
                         ") - ",
                         x,
                         "_P")
  }

  if (respiration == TRUE) {
    wo.ex_uvar <- paste0(x,
                         "_U = ",
                         "Flowto(",
                         x,
                         ") - ",
                         x,
                         "_P - ",
                         x,
                         "_R")

  }
  return(wo.ex_uvar)
}

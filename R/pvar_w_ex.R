#' Production variable definition with exports
#'
#' @param x matrices defined from Network Data Input workbook
#' @param respiration If respiration = TRUE in main autoLIMR argument
#' @param NLNode the defined NLNodes from the main autoLIMR argument
#'
#' @export
#'
pvar_w_ex <- function (x, respiration, NLNode) {
  if (!is.null(respiration) | respiration == FALSE) {
    w.ex_pvar <-
      paste0(x,
             "_P = ",
             "Flowfrom(",
             x,
             ") - ",
             x,
             "_U - ",
             x,
             "_EX")
  }
  if (respiration == TRUE) {
    w.ex_pvar <-
      paste0(x,
             "_P = ",
             "Flowfrom(",
             x,
             ") - ",
             x,
             "_R - ",
             x,
             "_U - ",
             x,
             "_EX")
  }
  return(w.ex_pvar)
}

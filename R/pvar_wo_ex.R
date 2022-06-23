#' Production variable definition without exports
#'
#' @param x matrices defined from Network Data Input workbook
#' @param respiration If respiration = TRUE in main autoLIMR argument
#' @param NLNode the defined NLNodes from the main autoLIMR argument
#'
#' @export
#'
pvar_wo_ex <- function(x, respiration, NLNode) {
  if (length(NLNode) > 0) {
    if (!is.null(respiration) | respiration == FALSE) {
      wo.ex_pvar <- paste0(x, "_P = ", "Flowfrom(", x, ") - ",
                           x, "_U")
    }
    if (respiration == TRUE) {
      wo.ex_pvar <- paste0(x,
                           "_P = ",
                           "Flowfrom(",
                           x,
                           ") - ",
                           x,
                           "_R - ",
                           x,
                           "_U")
    }
  } else {
    if (!is.null(respiration) | respiration == FALSE) {
      wo.ex_pvar <- paste0(x, "_P = ", "Flowfrom(", x, ")")
    }
    if (respiration == TRUE) {
      wo.ex_pvar <- paste0(x,
                           "_P = ",
                           "Flowfrom(",
                           x,
                           ") - ",
                           x,
                           "_R")
    }
  }
  return(wo.ex_pvar)
}

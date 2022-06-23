# AE Variable definition
#' Assimilation efficiency defintion
#' Defines assimilation efficiency variables if included in network input data workbook sheets
#' @param x network data input matrix
#' @param respiration If respiration = TRUE in main autoLIMR argument
#'
#' @export
#'
aevar <- function (x, respiration) {

  ae.search <- search_cols(x, col.match = "AE")
  aemat <- x[, c(ae.search), drop = T]
  ae.mat <- aemat[grep("NLNode", rownames(aemat), invert = TRUE), , drop = FALSE]
  w.ae <- names(which(rowSums(is.na(ae.mat)) != ncol(ae.mat)))

  if (length(w.ae) > 0) {
    # Function: Define AE variables only for nodes that have AE inequalities
    aevar_w_ae <- function (x, respiration) {
      w.ae_uvar <- paste0(x,
                          "_AE = ",
                          x,
                          "_P")

      if (respiration == TRUE) {
        w.ae_uvar <- paste0(x,
                            "_AE = ",
                            x,
                            "_P - ",
                            x,
                            "_R")

      }
      return(w.ae_uvar)
    }

    ae_var <- aevar_w_ae(w.ae, respiration = respiration)
    ae_var <-
      c("! Assimilation Efficiency (AE) Variables",
        "",
        sort(ae_var))
  } else {
    ae_var <- c("! No Assimilation Efficiency (AE) Variables defined")
  }
}

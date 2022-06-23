#' Production variable definition
#' Defines production variable based on many arguments
#' @param x network data input matrix
#' @param respiration If respiration = TRUE in main autoLIMR argument
#' @param NLNode the defined NLNodes from the main autoLIMR argument
#'
#' @export
#'
pvar <- function(x, respiration, NLNode) {
  exmat <- x[, grep("Export", colnames(x)), drop = T]
  ex.mat <- exmat[grep("NLNode", rownames(exmat), invert = T), , drop = F]
  wo.ex <- names(which(rowSums(is.na(ex.mat)) == ncol(ex.mat)))
  w.ex <- names(which(rowSums(is.na(ex.mat)) != ncol(ex.mat)))

  if (length(wo.ex) > 0 & length(w.ex) > 0) {
    wo.ex_pvar <- pvar_wo_ex(wo.ex, respiration = respiration, NLNode = NLNode)
    w.ex_pvar <- pvar_w_ex(w.ex, respiration = respiration, NLNode = NLNode)
    var <- c(wo.ex_pvar, w.ex_pvar)
  }

  if (identical(wo.ex, character(0)) & length(w.ex) > 0) {
    var <- pvar_w_ex(w.ex, respiration = respiration, NLNode = NLNode)
  }

  if (length(wo.ex) > 0 & identical(w.ex, character(0))) {
    var <- pvar_wo_ex(wo.ex, respiration = respiration, NLNode = NLNode)
  }

  if (length(var) > 0) {
    p_var <- c("! Production (P/NPP) Variables", "", sort(var), "")
  } else {
    p_var <- c("", "! No Production Variables (U) defined", "")
  }
}

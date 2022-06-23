#' Consumption (Q) Variable definition
#' Living nodes only, depends on Inputs
#' @param x network input data matrix
#'
#' @export
#'
qvar <- function(x) {
  inmat <- x[, grep("Import", colnames(x)), drop = T]
  in.mat <-
    inmat[grep("NLNode", rownames(inmat), invert = T), , drop = F]
  wo.in <-
    names(which(rowSums(is.na(in.mat)) == ncol(in.mat)))
  w.in <- names(which(rowSums(is.na(in.mat)) != ncol(in.mat)))

  if (length(wo.in) > 0 & length(w.in) > 0) {
    wo.in_qvar <- paste0(wo.in, "_Q = ", "Flowto(", wo.in, ")")
    w.in_qvar <-
      paste0(w.in, "_Q = ", "Flowto(", w.in, ") - ", w.in, "_IN")
    var <- c(wo.in_qvar, w.in_qvar)
  }

  if (identical(wo.in, character(0)) & length(w.in) > 0) {
    var <- paste0(w.in, "_Q = ", "Flowfrom(", w.in, ") - ", w.in, "_IN")

  }

  if (length(wo.in) > 0 & identical(w.in, character(0))) {
    var <- paste0(wo.in, "_Q = ", "Flowfrom(", wo.in, ")")

  }

  qvar <-
    c("! Consumption (Q) / Gross Primary Production (GPP) Variables",
      "",
      sort(var),
      "")
  return(qvar)
}

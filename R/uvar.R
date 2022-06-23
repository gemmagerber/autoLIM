#' Unused material/energy variable definition
#'
#' @param x matrices
#'
#' @export
#'
uvar <- function (x) {
  exmat <- x[, grep("Export", colnames(x)), drop = TRUE]
  ex.mat <- exmat[grep("NLNode", rownames(exmat), invert = TRUE), , drop = FALSE]
  wo.ex <- names(which(rowSums(is.na(ex.mat)) == ncol(ex.mat)))
  w.ex <- names(which(rowSums(is.na(ex.mat)) != ncol(ex.mat)))

  if (length(wo.ex) > 0 & length(w.ex) > 0) {
    wo.ex_uvar <- uvar_wo_ex(wo.ex)
    w.ex_uvar <- uvar_w_ex(w.ex)
    var <- c(wo.ex_uvar, w.ex_uvar)

  }

  if (identical(wo.ex, character(0)) & length(w.ex) > 0) {
    var <- uvar_w_ex(w.ex)

  }

  if (length(wo.ex) > 0 & identical(w.ex, character(0))) {
    var <- uvar_wo_ex(wo.ex)

  }

  if (length(var) > 0) {
    u_var <-
      c("! Unused Energy/Material (U) Variables",
        "",
        sort(var),
        "")
  } else {
    u_var <-
      c("",
        "! No Unused Energy/Material (U) Variables defined",
        "")
  }

}

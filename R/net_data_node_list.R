#' net_data_node_list
#' define compartment list
#' @param x the net data input matrix
#'
#' @export
#'
net_data_node_list <- function(x) {
  x2 <- as.matrix(x[, "Biomass"])
  x2 <- as.vector(paste(rownames(x2), "=", x2))
  LN <- grep(x2,
             pattern = "NLNode",
             invert = TRUE,
             value = TRUE) # grab living nodes
  NLN <- grep(x2,
              pattern = "NLNode",
              invert = FALSE,
              value = TRUE) # grab nonliving nodes
  combined <-
    c(sort(LN),
      sort(NLN)) # paste together so that NLN are at bottom of list
  return(combined)

}

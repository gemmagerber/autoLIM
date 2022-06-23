# New function: metadata2 (ad mats) with abbreviations

#' Metadata from adjacency matrices with abbreviations
#' Defines metadata from adjacency matrices
#' @param x Adjacency matrix
#'

#' @export
#'

meta2 <- function(x) {
  internals <- paste0("! Internal flows: ", sum(!is.na(x)))
  metadata2 <- c(
    internals,
    "",
    "! Abbreviations",
    "! GPP = Gross Primary Production (autotrophs only)",
    "! Q = Consumption",
    "! NPP = Net Primary Production (autotrophs only)",
    "! P = Production",
    "! R = respiration",
    "! U = Passive flows to non-living compartments/Unassimilated material",
    "! AE = Assimilation Efficiency",
    "! IN = Import flow",
    "! EX = Export Flow",
    "! NLNode = Non-living compartment",
    ""
  )
  return(metadata2)
}

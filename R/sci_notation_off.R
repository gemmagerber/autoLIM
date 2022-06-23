#' sci_notation_off
#' Turns off scientific notation
#' @param x the imported matrix/matrices
#'
#' @export
#'
sci_notation_off <- function(x) {
  x <- as.matrix(x)
  poss_num <- suppressWarnings(as.numeric(x))
  isna <- is.na(poss_num)
  x[!isna] <- format(poss_num[!isna], scientific = FALSE)
  x <- gsub(x, pattern = "1.0000", replacement = "1")
  x
}

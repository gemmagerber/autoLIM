#' search_cols
#' search and return columns that match type (as character vector) "Inputs", "Exports", "Assimilation Efficieny"
#' @param x the matrix
#' @param col.match the node type to match
#'
#' @export
#'
search_cols <- function (x, col.match) {

  if (col.match == "Input") {
    x <- grep(
      as.vector(colnames(x)),
      pattern = paste0(
        c("Import", "Imports", "Input", "^+In+$", "^+IN+$"),
        collapse = "|"
      ),
      value = TRUE,
      invert = FALSE,
      ignore.case = FALSE
    )
    return(x)
  }

  if (col.match == "Export") {
    x <- grep(
      colnames(x),
      pattern = paste0(c("Export", "Exports", "Ex", "EX"),
                       collapse = "|"),
      value = TRUE,
      invert = FALSE,
      ignore.case = TRUE
    )

    return(x)

  }

  if (col.match == "AE") {
    x <- grep(
      colnames(x),
      pattern = paste0(
        c("AE", "Assimilation", "efficiency", "AssEm"),
        collapse = "|"
      ),
      value = TRUE,
      invert = FALSE,
      ignore.case = TRUE
    )
    return(x)
  }

  if (col.match == "Custom") {
    x <- grep(
      colnames(x),
      pattern = paste0(c("Custom"),
                       collapse = "|"),
      value = TRUE,
      invert = FALSE,
      ignore.case = TRUE
    )
    return(x)
  }

  if (col.match == "Parameters") {
    x <- grep(
      colnames(x),
      pattern = paste0(c("Parameters|Parameters"),
                       collapse = "|"),
      value = TRUE,
      invert = FALSE,
      ignore.case = TRUE
    )
    return(x)
  }
}

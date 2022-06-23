#' read_all_sheets
#' Read all excel sheets, define as list, with sheet names as list element names
#' @param filename the filename of the user-supplied workbook
#' @param tibble import as tibble? Always FALSE
#' @importFrom readxl excel_sheets read_excel
#' @export
#'
read_all_sheets <- function(filename, tibble = FALSE) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop(
      "Package \"readxl\" must be installed to use this function.",
      call. = FALSE
    )
  }
  options('scipen' = 99999)
  sheets <- excel_sheets(filename)
  x <-
    lapply(sheets, function(X)
      read_excel(
        filename,
        sheet = X,
        col_types = "text",
        .name_repair = "unique"
      ))
  if (!tibble)
    x <- lapply(x, as.data.frame)
  names(x) <- sheets
  return(x)
}

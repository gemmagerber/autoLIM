#' Function fetch_autolimexcel
#' Downloads the latest version of autoLIM-Excel from GitHub
#' @return A .xlsx file
#' @export
#' @importFrom utils download.file
#'
fetch_autolimexcel <- function() {

  url_autolimexcel <- "https://github.com/gemmagerber/autoLIM-Excel/raw/main/autoLIM_Excel_4node_Winter.xlsx"
  filename <- "autoLIM-Excel_4node_Winter.xlsx"
  download.file(url = url_autolimexcel, destfile = filename)
  message("Success! The latest version of autoLIMExcel has been downloaded and saved in the working directory")

}

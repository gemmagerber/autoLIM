### Function:
#' Inequalities definition (with headings)
#'
#' @param x network input data matrix
#' @param respiration  If respiration = TRUE in main autoLIMR argument
#' @param primary_producer Primary producers defined in main autoLIMR function

#'
#' @export
#'
net_data_ineq <- function (x, respiration, primary_producer) {
  x <- na.omit(as.data.frame(as.table(x)))
  x$Variable <- ifelse(
    grepl("Consumption|GPP", x$Var2) &
      grepl(paste0(primary_producer, collapse = "|"), x$Var1),
    paste0(x$Var1, "_GPP"),
    ifelse(
      grepl("Consumption|GPP", x$Var2) &
        !grepl(paste0(primary_producer, collapse = "|"), x$Var1),
      paste0(x$Var1, "_Q"),
      ifelse(
        grepl("Production|Net_Primary_Production|NPP|P", x$Var2) &
          grepl(paste0(primary_producer, collapse = "|"), x$Var1),
        paste0(x$Var1, "_NPP"),
        ifelse(
          grepl("Production|Net_Primary_Production|NPP|P", x$Var2) &
            !grepl(paste0(primary_producer, collapse = "|"), x$Var1),
          paste0(x$Var1, "_P"),
          ifelse(
            grepl("respiration|Resp|R", x$Var2),
            paste0(x$Var1, "_R"),
            ifelse(
              grepl("Unused energy|Unused", x$Var2),
              paste0(x$Var1, "_U"),
              ifelse(
                grepl("Export|EX|Ex|Exports", x$Var2),
                paste0(x$Var1, "_EX"),
                ifelse(
                  grepl("Import|Imports|Input|IN|In", x$Var2),
                  paste0(x$Var1, "_IN"),
                  ifelse(
                    grepl("Assimilation|Efficiency|AE", x$Var2),
                    paste0(x$Var1, "_AE"),
                    "none"
                  )
                )
              )
            )
          )
        )
      )
    )
  )


  x$Freq <- as.character(x$Freq)

  x$Inequality <- ifelse(grepl("^1$|^0$", x$Freq),
                         "none",
                         ifelse(
                           grepl("lower|low|min|minimum", x$Var2, ignore.case = TRUE),
                           paste0(x$Variable, " > ", x$Freq),
                           ifelse(
                             grepl("upper|up|max|maximum", x$Var2, ignore.case = TRUE),
                             paste0(x$Variable, " < ", x$Freq),
                             "none"
                           )
                         ))

  x <- x[!grepl("none", x$Inequality),]

  toreturn <- c("! Network Data Input Inequalities",
                "",
                as.vector(x$Inequality),
                "")

  return(toreturn)

}

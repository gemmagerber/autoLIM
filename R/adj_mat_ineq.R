#' Matrix inequalities defintions
#' Defines inequalities from adjacency matrix input workbook
#' @param x Adjacency matrix
#'
#' @export
#'
adj_mat_ineq <- function (x) {
  x <- na.omit(as.data.frame(as.table(x)))
  x$flowtype <-
    ifelse(
      grepl("NLNode", x$Var1) & grepl("NLNode", x$Var2),
      "_to_",
      ifelse(
        grepl("NLNode", x$Var1) & !grepl("NLNode", x$Var2),
        "_Q_",
        ifelse(
          !grepl("NLNode", x$Var1) & grepl("NLNode", x$Var2),
          "_U_",
          ifelse(
            !grepl("NLNode", x$Var1) & !grepl("NLNode", x$Var2),
            "_Q_",
            "test"
          )
        )
      )
    )

  x$flownames <- paste0(x$Var1, x$flowtype, x$Var2)

  x$Variable <-
    ifelse(
      grepl("NLNode", x$Var1) & grepl("NLNode", x$Var2),
      "_to",
      ifelse(
        grepl("NLNode", x$Var1) & !grepl("NLNode", x$Var2),
        "_Q",
        ifelse(
          !grepl("NLNode", x$Var1) & grepl("NLNode", x$Var2),
          "_U",
          ifelse(
            !grepl("NLNode", x$Var1) & !grepl("NLNode", x$Var2),
            "_Q",
            "test"
          )
        )
      )
    )

  x$Freq <- as.character(x$Freq)

  x$ineq <-
    ifelse(
      grepl("1|1.0", x$Freq) & !grepl(",", x$Freq),
      "none",
      ifelse(
        grepl(",", x$Freq) & grepl("NLNode", x$Var2),
        paste0(x$flownames,
               " = ",
               x$Var1,
               x$Variable,
               " * ",
               "[",
               x$Freq,
               "]"),
        ifelse(
          grepl(",", x$Freq) & !grepl("NLNode", x$Var2),
          paste0(x$flownames,
                 " = ",
                 x$Var2,
                 x$Variable,
                 " * ",
                 "[",
                 x$Freq,
                 "]"),
          ifelse(
            !grepl(",", x$Freq) & grepl("NLNode", x$Var2),
            paste0(x$flownames,
                   " < ",
                   x$Var1,
                   x$Variable,
                   " * ",
                   x$Freq)
            ,
            ifelse(
              !grepl(",", x$Freq) & !grepl("NLNode", x$Var2),
              paste0(x$flownames,
                     " < ",
                     x$Var2,
                     x$Variable,
                     " * ",
                     x$Freq)
              ,
              "none"
            )
          )
        )
      )
    )


  na.remove <- x[!grepl("none", x$ineq),]
  ineq.vec <- as.vector(na.remove$ineq)

  toreturn <- c("! Adjacency Matrix Inequalities",
                "",
                ineq.vec)

  return(toreturn)
}

#' centralx0()
#' Solves the CENTRAL SOLUTION as the MCMC starting point in LIM::Xsample.
#' If central solution is not valid, defaults to LSEI
#'
#' @param full_limfile the LIM file built from check_build()
#' @param iter number of iterations defined by the user.
#' @param jmp The jump size of the algorithm defined by the user. If NULL, jump size is internally calculated.
#' @param x0 The starting point. Central solution.
#' @param ... Further LIM::Xsample arguments
#'
#' @return Multiple plausible networks, starting point CENTRAL SOLUTION

#' @importFrom LIM Xsample Flowmatrix
#'

centralx0 <-
  function (full_limfile,
            iter = NULL,
            jmp = NULL,
            x0 = "central",
            ...) {
    x0 <- LIM::Xranges(full_limfile, central = TRUE, ispos = TRUE)[, "central"]
    starting.solution.x0 <- x0
    message("Checking that the central solution is valid...\n")
    cen1 <- max(abs(full_limfile$A %*% x0 - full_limfile$B))
    cen2 <- min(full_limfile$G %*% x0 - full_limfile$H)

    if (!is.na(cen1) & !is.na(cen2) == TRUE) {
      message("Central solution is valid.\n")
      message(
        "Initial solution (x0) calculated using LIM::Xranges central solution.
    Solving multiple plausible networks (this may take a while)...\n"
      )
      print(system.time(
        solved.flow.values <- LIM::Xsample(
          lim = full_limfile,
          x0 = x0,
          jmp = jmp,
          iter = iter,
          outputlength = iter,
          ...
        )
      ))
      solved.flow.matrices <- list(NULL)
      for (i in 1:as.numeric(nrow(solved.flow.values))) {
        solved.flow.matrices[[i]] <-
          LIM::Flowmatrix(full_limfile, web = solved.flow.values[i, ])
      }

      solved.networks = list(
        full_limfile = full_limfile,
        starting.solution.x0 = starting.solution.x0,
        solved.flow.values = solved.flow.values,
        solved.flow.matrices = solved.flow.matrices
      )
      message("Multiple plausible networks solved.\n")

      return(solved.networks)

    } else {
      if (is.na(cen1 | cen2) == TRUE) {
        message("Central solution is not valid. Default starting algorithm initialised\n")

        solved.networks <- defaultx0(
          lim = full_limfile,
          x0 = x0,
          jmp = jmp,
          iter = iter,
          outputlength = iter,
          ...
        )
      }
    }
  }

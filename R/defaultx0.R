#' defaultx0()
#' Solves the CENTRAL SOLUTION as the MCMC starting point in LIM::Xsample
#'
#' @param full_limfile the LIM file built from check_build()
#' @param iter number of iterations defined by the user.
#' @param jmp The jump size of the algorithm defined by the user. If NULL, jump size is internally calculated.
#' @param x0 The starting point. Defaults to LSEI.
#' @param ... Further LIM::Xsample arguments
#'
#' @return Multiple plausible networks, starting point LSEI

#' @importFrom LIM Xsample Flowmatrix

#'
defaultx0 <-
  function (full_limfile,
            iter = NULL,
            jmp = NULL,
            x0 = NULL,
            ...) {
    if (!requireNamespace("LIM", quietly = TRUE)) {
      stop(
        "Package \"LIM\" must be installed to use this function.",
        call. = FALSE
      )
    }
    message(
      "Initial solution (x0) calculated using default LSEI algorithm (Haskell and Hanson 1981).
    Solving multiple plausible networks (this may take a while)...\n"
    )
    starting.solution.x0 <- "LSEI"
    print(system.time(
      solved.flow.values <- LIM::Xsample(
        lim = full_limfile,
        x0 = x0,
        jmp = jmp,
        iter = iter,
        outputlength = iter
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
  }

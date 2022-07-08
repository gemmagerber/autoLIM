#' MPN with multi.nets function
#' @inheritParams check_build
#' @inheritParams prepack_fun
#' @inheritParams defaultx0
#' @inheritParams centralx0
#' @inheritParams as_extended
#' @param pack Logical argument to pack the networks for analysis with package enaR
#'
#' @return
#' @export

#' @examples
multinets <-
  function(file = NULL,
           iter = NULL,
           jmp = NULL,
           x0 = NULL,
           pack = FALSE,
           ...) {

    full_limfile <- check_build(file = file) # Check and build LIM file

    # Starting point choices. Choice 1: Default
    if (is.null(x0)) {
      solved.networks <-
        defaultx0(
          full_limfile = full_limfile,
          iter = iter,
          jmp = jmp,
          x0 = NULL
        )
    } else {
      if (x0 == "central") {
        solved.networks <-
          centralx0(
            full_limfile = full_limfile,
            iter = iter,
            jmp = jmp,
            x0 = "central"
          )
      }
    }

    # pack args
    if (pack == FALSE) {
      return(solved.networks)

    } else {
      if (pack == TRUE) {
        # enaR packing
        message("Packing multiple plausible networks for enaR analysis...\n")
        packed.enaR.nets <-
          lapply(X = solved.networks$solved.flow.matrices, FUN = prepack_fun)
        balanced <- lapply(X = packed.enaR.nets, FUN = ssCheck)
        solved.networks.2 = list(packed.enaR.nets = packed.enaR.nets,
                                 balanced = balanced)
        out <- append(solved.networks, solved.networks.2)

        message("Multiple plausible networks solved and packed for enaR analysis.\n")
        return(out)
      }
    }
  }

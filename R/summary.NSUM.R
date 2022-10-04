#' Summarise NSUM model fits
#'
#' [summary()] method for models fitted with [nsum.mcmc()].
#'
#' @param object object of class "`NSUM`" as returned by [nsum.mcmc()]
#' @param x object of class "`NSUM.summary` as returned by `summary.NSUM()`
#' @param ... other arguments to/from other methods, currently ignored
#'
#' @return The function `summary.NSUM` returns a list with as many elements as
#'   `object` has, but elements corresponding to estimated parameters are
#'   summarised with [summary.default()].
#'
#' @export

summary.NSUM <- function(object, ...) {
  nams <- c("NK", "d", "rho", "tauK", "q")
  has <- names(object) %in% paste0(nams, ".values")
  rval <- object
  rval[has] <- lapply(object[has], apply, 1, summary.default)
  rval$mu.values <- summary.default(object$mu.values)
  rval$sigma.values <- summary.default(object$sigma.values)
  structure(rval, class = "summary.NSUM")
}


#' @rdname summary.NSUM
#' @export

print.summary.NSUM <- function(x, ...) {
  cat("\nNSUM model\n")
  if(!is.null(x$call)) {
    cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"),
        "\n\n", sep = "")
  }
  invisible(NULL)
}

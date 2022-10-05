#' Other functions
#'
#' @param x object returned by `nsum.mcmc()`
#' @param ... other arguments to/from other methods
#'
#' @export
print.NSUM <- function(x, ...) {
  cat("\nNSUM model\n")
  if(!is.null(x$call)) {
    cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"),
        "\n\n", sep = "")
  }
  cat("\nAvailable elements:\n")
  print(names(x))
  invisible(NULL)
}

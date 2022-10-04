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

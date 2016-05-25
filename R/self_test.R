#' Simple self-test
#'
#' \code{say_hello} Prints a welcome message. This is the second '
#' paragraph and should briefly describe the function. All objects need a
#' title (the first line) and a description.
#' % <- see that percent sign, that's a latex comment that is
#'
#' This is simply here for testing
#' @param x Your name
#' @return Prints \code{x}
#' @examples
#' # say_hello("Steve")
#'
#' @export
say_hello <- function(x) {
    print(paste0("Hello ", x, "!\n"))
}

#' @name Interval operators
#' @rdname interval_operators
#' 
#' @title Interval operators
#' 
#' @description Operators to check if values are inside continuous (half) open and closed sets.
#' 
#' @param x An atomic vector.
#' @param y An atomic vector of length 2 containing the limits of a continous set.
#'
#' @details The vector \code{y} specifying the limits in which \code{x} should be contained must be sorted, i.e. have its lower limit as the first element and its upper limit as the second element.
#' The operators are currently implemented for data of classes \code{numeric}, \code{POSIXct} and \code{Date}.
#' Depending on which operator is used, the set specified in \code{y} is treated as closed, open, left-open or right-open:
#' \tabular{ll}{
#'    \code{%[]%} \tab \code{y} is treated as a closed set, i.e. the limits of the set are treated as part of the set\cr
#'    \code{%()%} \tab \code{y} is treated as an open set, i.e. the limits of the set are not a part of the set\cr
#'    \code{%(]%} \tab \code{y} is treated as a left-open set, i.e. only the upper limit of the set is treated as part of the set\cr
#'    \code{%[)%} \tab \code{y} is treated as a right-open set, i.e. only the lower limit of the set is treated as part of the set\cr
#' }
#'
#' @return The operators return an atomic vector of class \code{logical} with the same length as \code{x}.
#'
#' @examples
#' 
#' 4 %[]% c(4, 6)
#' 3 %[]% c(4, 6)
#' 4 %(]% c(4, 6)
#' 6 %(]% c(4, 6)
#' 4 %[)% c(4, 6)
#' 6 %[)% c(4, 6)
#' 6 %()% c(4, 6)
#' 5 %()% c(4, 6)
#' Sys.time() %()% (Sys.time() + c(0, 1) * 60)
#' Sys.Date() %[]% c("2020-01-01", "2020-12-31")
#' set.seed(29)
#' 2 %()% range(rnorm(100))
#' 
NULL

#' @rdname interval_operators
#' @export
`%[]%` <- function (x, y) UseMethod("%[]%")

setMethod(
  "%[]%",
  signature(x = "numeric"),
  function (x, y) {
    if (length(y) != 2) stop("'y' must have length 2.")
    if (!is.numeric(y)) stop("The class of 'y' must suit that of 'x'.")
    (x >= y[1]) & (x <= y[2])
  }
)

setMethod(
  "%[]%",
  signature(x = "POSIXct"),
  function (x, y) {
    if (length(y) != 2) stop("'y' must have length 2.")
    if (inherits(try(sapply(y, as.POSIXct), silent = TRUE), "try-error")) stop("The class of 'y' must suit that of 'x'.")
    if (!inherits(y, "POSIXct")) y <- as.POSIXct(y)
    (x >= y[1]) & (x <= y[2])
  }
)

setMethod(
  "%[]%",
  signature(x = "Date"),
  function (x, y) {
    if (length(y) != 2) stop("'y' must have length 2.")
    if (inherits(try(sapply(y, as.Date), silent = TRUE), "try-error")) stop("The class of 'y' must suit that of 'x'.")
    if (!inherits(y, "Date")) y <- as.Date(y)
    (x >= y[1]) & (x <= y[2])
  }
)

#' @rdname interval_operators
#' @export
`%()%` <- function (x, y) UseMethod("in_open")

setMethod(
  "%()%",
  signature(x = "numeric"),
  function (x, y) {
    if (length(y) != 2) stop("'y' must have length 2.")
    if (!is.numeric(y)) stop("The class of 'y' must suit that of 'x'.")
    (x > y[1]) & (x < y[2])
  }
)

setMethod(
  "%()%",
  signature(x = "POSIXct"),
  function (x, y) {
    if (length(y) != 2) stop("'y' must have length 2.")
    if (inherits(try(sapply(y, as.POSIXct), silent = TRUE), "try-error")) stop("The class of 'y' must be coercable to that of 'x'.")
    if (!inherits(y, "POSIXct")) y <- as.POSIXct(y)
    (x > y[1]) & (x < y[2])
  }
)

setMethod(
  "%()%",
  signature(x = "Date"),
  function (x, y) {
    if (length(y) != 2) stop("'y' must have length 2.")
    if (inherits(try(sapply(y, as.Date), silent = TRUE), "try-error")) stop("The class of 'y' must be coercable to that of 'x'.")
    if (!inherits(y, "Date")) y <- as.Date(y)
    (x > y[1]) & (x < y[2])
  }
)


#' @rdname interval_operators
#' @export
`%(]%` <- function (x, y) UseMethod("in_open")

setMethod(
  "%(]%",
  signature(x = "numeric"),
  function (x, y) {
    if (length(y) != 2) stop("'y' must have length 2.")
    if (!is.numeric(y)) stop("The class of 'y' must suit that of 'x'.")
    (x > y[1]) & (x <= y[2])
  }
)

setMethod(
  "%(]%",
  signature(x = "POSIXct"),
  function (x, y) {
    if (length(y) != 2) stop("'y' must have length 2.")
    if (inherits(try(sapply(y, as.POSIXct), silent = TRUE), "try-error")) stop("The class of 'y' must be coercable to that of 'x'.")
    if (!inherits(y, "POSIXct")) y <- as.POSIXct(y)
    (x > y[1]) & (x <= y[2])
  }
)

setMethod(
  "%(]%",
  signature(x = "Date"),
  function (x, y) {
    if (length(y) != 2) stop("'y' must have length 2.")
    if (inherits(try(sapply(y, as.Date), silent = TRUE), "try-error")) stop("The class of 'y' must be coercable to that of 'x'.")
    if (!inherits(y, "Date")) y <- as.Date(y)
    (x > y[1]) & (x <= y[2])
  }
)


#' @rdname interval_operators
#' @export
`%[)%` <- function (x, y) UseMethod("in_open")

setMethod(
  "%[)%",
  signature(x = "numeric"),
  function (x, y) {
    if (length(y) != 2) stop("'y' must have length 2.")
    if (!is.numeric(y)) stop()
    (x >= y[1]) & (x < y[2])
  }
)

setMethod(
  "%[)%",
  signature(x = "POSIXct"),
  function (x, y) {
    if (length(y) != 2) stop("'y' must have length 2.")
    if (inherits(try(sapply(y, as.POSIXct), silent = TRUE), "try-error")) stop("The class of 'y' must be coercable to that of 'x'.")
    if (!inherits(y, "POSIXct")) y <- as.POSIXct(y)
    (x >= y[1]) & (x < y[2])
  }
)

setMethod(
  "%[)%",
  signature(x = "Date"),
  function (x, y) {
    if (length(y) != 2) stop("'y' must have length 2.")
    if (inherits(try(sapply(y, as.Date), silent = TRUE), "try-error")) stop("The class of 'y' must be coercable to that of 'x'.")
    if (!inherits(y, "Date")) y <- as.Date(y)
    (x >= y[1]) & (x < y[2])
  }
)


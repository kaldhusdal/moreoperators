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
    if (!all(is.numeric(y))) y <- as.numeric(y)
    if (any(is.na(y))) return(NA)
    (x >= y[1]) & (x <= y[2])
  }
)

#tests
#5 %[]% c(5, 7)
#5 %[]% (Sys.Date() + c(0, 1))
#5 %[]% (Sys.time() + c(0, 1))
#5 %[]% c(NA, 7)
#5 %[]% c(5, NA)
#5 %[]% c("A", "Z")


setMethod(
  "%[]%",
  signature(x = "POSIXct"),
  function (x, y) {
    if (length(y) != 2) stop("'y' must have length 2.")
    if (inherits(y, "Date")) {
      y <- as.POSIXct(paste(as.character(y), c("00:00:00", "23:59:59")))
    }
    if (!inherits(y, "POSIXct")) y <- as.POSIXct(y)
    if (any(is.na(y))) return(NA)
    (x >= y[1]) & (x <= y[2])
  }
)

#tests
#Sys.time() %[]% c(1, 10) #Error in as.POSIXct.numeric(y) : 'origin' must be supplied
#Sys.time() %[]% rep(Sys.Date(), 2)
#Sys.time() %[]% (Sys.Date() + c(-1, 1))
#Sys.time() %[]% (Sys.time() + c(-1, 1))
#Sys.time() %[]% c(Sys.time(), NA)
#Sys.time() %[]% c(NA, Sys.time())
#Sys.time() %[]% c("A", "Z")

setMethod(
  "%[]%",
  signature(x = "Date"),
  function (x, y) {
    if (length(y) != 2) stop("'y' must have length 2.")
    if (!inherits(y, "Date")) y <- as.Date(y)
    if (any(is.na(y))) return(NA)
    (x >= y[1]) & (x <= y[2])
  }
)

#tests
#Sys.Date() %[]% c(1, 10) #Error in as.Date.numeric(y) : 'origin' must be supplied
#Sys.Date() %[]% rep(Sys.time(), 2)
#Sys.Date() %[]% (Sys.time() + c(-1, 1))
#Sys.Date() %[]% (Sys.Date() + c(-1, 1))
#Sys.Date() %[]% c(Sys.Date(), NA)
#Sys.Date() %[]% c(NA, Sys.Date())
#Sys.Date() %[]% c("A", "Z")


#' @rdname interval_operators
#' @export
`%()%` <- function (x, y) UseMethod("in_open")

setMethod(
  "%()%",
  signature(x = "numeric"),
  function (x, y) {
    if (length(y) != 2) stop("'y' must have length 2.")
    if (!all(is.numeric(y))) y <- as.numeric(y)
    if (any(is.na(y))) return(NA)
    (x > y[1]) & (x < y[2])
  }
)

setMethod(
  "%()%",
  signature(x = "POSIXct"),
  function (x, y) {
    if (length(y) != 2) stop("'y' must have length 2.")
    if (inherits(y, "Date")) {
      y <- as.POSIXct(paste(as.character(y), c("00:00:00", "23:59:59")))
    }
    if (!inherits(y, "POSIXct")) y <- as.POSIXct(y)
    if (any(is.na(y))) return(NA)
    (x > y[1]) & (x < y[2])
  }
)

setMethod(
  "%()%",
  signature(x = "Date"),
  function (x, y) {
    if (length(y) != 2) stop("'y' must have length 2.")
    if (!inherits(y, "Date")) y <- as.Date(y)
    if (any(is.na(y))) return(NA)
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
    if (!all(is.numeric(y))) y <- as.numeric(y)
    if (any(is.na(y))) return(NA)
    (x > y[1]) & (x <= y[2])
  }
)

setMethod(
  "%(]%",
  signature(x = "POSIXct"),
  function (x, y) {
    if (length(y) != 2) stop("'y' must have length 2.")
    if (inherits(y, "Date")) {
      y <- as.POSIXct(paste(as.character(y), c("00:00:00", "23:59:59")))
    }
    if (!inherits(y, "POSIXct")) y <- as.POSIXct(y)
    if (any(is.na(y))) return(NA)
    (x > y[1]) & (x <= y[2])
  }
)

setMethod(
  "%(]%",
  signature(x = "Date"),
  function (x, y) {
    if (length(y) != 2) stop("'y' must have length 2.")
    if (!inherits(y, "Date")) y <- as.Date(y)
    if (any(is.na(y))) return(NA)
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
    if (!all(is.numeric(y))) y <- as.numeric(y)
    if (any(is.na(y))) return(NA)
    (x >= y[1]) & (x < y[2])
  }
)

setMethod(
  "%[)%",
  signature(x = "POSIXct"),
  function (x, y) {
    if (length(y) != 2) stop("'y' must have length 2.")
    if (inherits(y, "Date")) {
      y <- as.POSIXct(paste(as.character(y), c("00:00:00", "23:59:59")))
    }
    if (!inherits(y, "POSIXct")) y <- as.POSIXct(y)
    if (any(is.na(y))) return(NA)
    (x >= y[1]) & (x < y[2])
  }
)

setMethod(
  "%[)%",
  signature(x = "Date"),
  function (x, y) {
    if (length(y) != 2) stop("'y' must have length 2.")
    if (!inherits(y, "Date")) y <- as.Date(y)
    if (any(is.na(y))) return(NA)
    (x >= y[1]) & (x < y[2])
  }
)


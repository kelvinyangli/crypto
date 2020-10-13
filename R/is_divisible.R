#
#' Divisibility
#'
#' This function checks if an integer is divisible by another integer.
#' @param a An integer.
#' @param b A non-zero integer that is smaller than a.
#' @export

is_divisible = function(a, b) {

  q = a / b
  isDivisible = (q %% 1 == 0)
  return(isDivisible)

}


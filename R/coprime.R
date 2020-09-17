#
#' Checking coprime
#'
#' This function checks if two integers are coprime.
#' @param a An integer.
#' @param b An integer.
#' @export

coprime = function(a, b) {

  # calculate gcd
  g = gcd(a, b)
  if ((g > 1) && (g < max(a, b))) {

    return(FALSE)

  }

  return(TRUE)

}

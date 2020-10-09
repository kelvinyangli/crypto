#
#' Greatest common divisor
#'
#' This function uses the Euclid's algorithm to find the gcd of two integers.
#' @param a An integer.
#' @param b An integer.
#' @export

gcd = function(a, b) {

  r0 = max(a, b)
  r1 = min(a, b)

  repeat {

    r = r0 %% r1
    if (r == 0) break
    r0 = r1
    r1 = r

  }

  return(r1)

}

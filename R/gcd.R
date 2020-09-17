#
#' Greatest common divisor
#'
#' This function uses Euclidean algorithm to find the gcd of two integers.
#' @param a An integer.
#' @param b An integer.
#' @export

gcd = function(a, b) {

  r0 = max(a, b)
  r1 = min(a, b)

  r = r0 %% r1

  if (r == 0) return(r1)

  repeat {

    r0 = r1
    r1 = r
    r = r0 %% r1

    if (r == 0) break

  }

  return(r1)

}

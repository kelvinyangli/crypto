#
#' Polynomial addition
#'
#' Given two polynomials (stored in their coefficient vectors), this
#' function returns the sum of them. Summation is component wise. The
#' two polynomials can be of same or different degrees.
#' @param a A vector of polynomial coefficients. The coefficients
#' must be stored in a in the order of (a0,a1, ..., an).
#' @param b A vector of polynomial coefficients. The coefficients
#' must be stored in a in the order of (b0,b1, ..., bn).
#' @export

polynomial_addition = function(a, b) {

  # match coefficient vector length
  na = length(a)
  nb = length(b)
  if (na < nb) {

    a = c(a, rep(0, nb - na))

  } else if (nb < na) {

    b = c(b, rep(0, na - nb))

  }

  c = a + b

  return(c)

}

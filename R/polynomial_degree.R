#
#' Polynomial degree
#'
#' This function returns the degree of a polynomial.
#' @param a A vector of polynomial coefficients, from the smallest term
#' to the largest term.
#' @export

polynomial_degree = function(a) {

  d = 0
  if (!zero_polynomial(a)) {

    d = max(which(a != 0)) - 1

  }

  return(d)

}

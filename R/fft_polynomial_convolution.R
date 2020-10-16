#
#' Polynomial convolution
#'
#' Given two polynomials (stored in their coefficients), this function
#' returns the polynomial convolution product. More precisely, it first
#' calculates the product of the given two polynomials, then take
#' modulo x^N - 1, where N is an integer. The returned polynomial has
#' degree at most N - 1.
#' @param a A vector of polynomial coefficients. The coefficients
#' must be stored in a in the order of (a0,a1, ..., an).
#' @param b A vector of polynomial coefficients. The coefficients
#' must be stored in a in the order of (b0,b1, ..., bn).
#' @param N An integer that ensures the returned polynomial has
#' degree at most N - 1.
#' @export
#
fft_polynomial_convolution = function(a, b, N) {

  c = fft_polynomial_multiplication(a, b)
  dc = polynomial_degree(c)
  if (dc >= N) c = polynomial_quotient(c, N)

  return(c)

}

#
#' Efficient polynomial multiplication using FFT
#'
#' This function does efficient polynomial multiplication using FFT.
#' The two inputs are coefficients of two polynomials of arbitrary
#' degrees. The function relies on FFT to transform coefficient
#' representations to sample representations, then does multiplication
#' pointwise and transform back to coefficient representation of the
#' final answer.
#' @param a A vector of polynomial coefficients.
#' @param b A vector of polynomial coefficients.
#' @export

fft_polynomial_multiplication = function(a, b) {

  na = length(a)
  nb = length(b)
  n = na + nb
  n = 2 ^ ceiling(log2(n))
  a = c(a, rep(0, n - na))
  b = c(b, rep(0, n - nb))
  A = fft(a)
  B = fft(b)
  C = A * B
  c = round(fft(C, inverse = T))
  nc = na + nb - 1
  c = Re(c[1:nc])
  return(c)

}


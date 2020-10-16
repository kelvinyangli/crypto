#
#' Sample ternary polynomial
#'
#' This function randomly samples ternary polynomials with maximum degree N.
#' A tenary polynomial's coefficients are taken from {-1,0,1} (equivalent to
#' {0,1,2}).
#' @param N An integer for the degree of the sampled polynomial.
#' @param d1 An integer for the number of 1s in the sampled coefficients.
#' @param d2 An integer for the number of -1s in the sampled coefficients.
#' @export

sample_ternary_polynomial = function(N, d1, d2) {

  # fxied the number of 1s and -1s accrdoing to d1 and d2 respectively
  # the rest of the coefficients (if there are any) are 0
  a = c(rep(1, d1), rep(-1, d2), rep(0, N - d1 - d2))
  # randomly shuffle a
  a = sample(a)
  a = omit_zero(a)

  return(a)

}

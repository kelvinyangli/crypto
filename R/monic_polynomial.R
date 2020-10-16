#
#' Monic polynomial
#'
#' This function makes a polynomial to monic. That is, the coefficient
#' of the largest degree term is 1. This can help to simplify
#' polynomial devision.
#' @param a A vector of polynomial coefficients.
#' @param p A integer. The polynomial coeffiicents are taken from Z/pZ.
#' If p is not prime, the function may fail, because the leading term
#' coefficient may not be coprime with p.
#' @export

monic_polynomial = function(a, p) {

  if (zero_polynomial(a)) stop("a is zero polynomial!")

  a = omit_zero(a) %% p
  n = length(a)
  leadingTerm = a[n]
  if (leadingTerm != 1) {

    if (!coprime(leadingTerm, p)) stop("Not possible to do!")
    leadingTermInverse = integer_modular_multiplicative_inverse(leadingTerm, p)
    a = (a * leadingTermInverse) %% p

  }

  # since the leading term is made to 1
  # there is no zero after it, so no need to use omit_zero()
  return(a)

}

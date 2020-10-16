#
#' Polynomial long division in field
#'
#' Polynomial long division in Z/pZ. This is not the most efficient
#' algorithm. The output is a list of quotient and remainder
#' polynomials, stored in their coefficient formats. If p is prime,
#' integer coefficients exist. Otherwise, integer coefficients may not
#' exist.
#' @param a A vector of polynomial coefficients from the smallest to the
#' largest term. This is the divident in the long division.
#' @param b A vector of polynomial coefficients from the smallest to
#' the largest term. The polynomial has to be non-zero. It does not need
#' to be monic. But it has to be smaller than a. This is the divisor.
#' @param p An integer. The polynomial coefficients are taken from
#' Z/pZ. If p is prime, integer coefficients of the quotient and
#' remainder polynomials exist. Otherwise, integer coefficient may not
#' exist.
#' @export

polynomial_long_division = function(a, b, p) {

  a = a %% p
  b = b %% p

  # omit 0s at the back to avoid issues
  a = omit_zero(a)
  b = omit_zero(b)

  # degrees
  da = polynomial_degree(a)
  db = polynomial_degree(b)

  # get b's leading coefficient's inverse
  bLeadingCoeff = b[db + 1]
  bLeadingCoeffInverse = integer_modular_multiplicative_inverse(bLeadingCoeff, p)

  # make b monic for easy leading coefficient division
  # no need to omit zero
  b = monic_polynomial(b, p)

  # a's degree must be no less than b
  # since a is the divident
  if (da < db) stop("a is smaller than b!")

  # make a b have the same length
  if (length(b) < length(a)) b = c(b, rep(0, length(a) - length(b)))

  # vectors to store the quotient and remainder polynomials
  # quotient starts from 0 polynomial
  q = rep(0, da + 1)

  # remainder starts from the divident a
  r = a
  dr = da

  # continue if the remainder polynomial is non-zero and
  # has degree no smaller than b's degree
  while (!zero_polynomial(r) && (dr >= db)) {

    # a temp vector to store the quotient
    # at each step of the long division
    t = rep(0, da + 1)

    # since b is made monic, b's highest degree term has coef 1
    # so r's leading coef divids by b's leading coef is just
    # r's leading coef
    t[dr - db + 1] = r[dr + 1]

    # update quotient polynomial by q = q + t mod p
    q = polynomial_addition(q, t) %% p

    # t*b is quotient times divisor at each step
    tb = fft_polynomial_multiplication(t, b)

    # update remainder polynomial by r = r - tb mod p
    r = polynomial_addition(r, -tb) %% p

    # update remainder's degree
    dr = polynomial_degree(r)

  } # end while

  # multiply back b's leading coefficient's inverse
  q = (q * bLeadingCoeffInverse) %% p

  q = omit_zero(q)
  r = omit_zero(r)
  ls = list("quotient" = q, "remainder" = r)

  return(ls)

}





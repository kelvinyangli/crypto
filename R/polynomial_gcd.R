#
#' Greatest common divisor between polynomials
#'
#' This function uses the Euclidean algorithm to find the gcd of two polynomials.
#' Since we are always interested in integer coefficient polynomials,
#' we require coefficients to be taken from Z/pZ. Ideally, p is prime.
#' @param a A vector of polynomial coefficients.
#' @param b A vector of polynomial coefficients.
#' @param p A integer. The polynomial coeffiicents are taken from Z/pZ.
#' If p is not prime, the algorithm may fail, because some integer
#' coefficients may not have inverses in Z/pZ, causing polynomial
#' division to fail.
#' @export

polynomial_gcd = function(a, b, p) {

  # omit 0s at the back to avoid issues
  a = omit_zero(a)
  b = omit_zero(b)

  # ensure polynomial coefficients are in Z/pZ
  a = a %% p
  b = b %% p

  # polynomial degrees
  da = polynomial_degree(a)
  db = polynomial_degree(b)

  # ensure r0 >= r1
  if (da >= db) {

    r0 = a
    r1 = b

  } else {

    r0 = b
    r1 = a

  }

  repeat { # repeat until the remainder is 0
  # while (!zero_polynomial(r)) {

    # remainder in modulo p
    res = polynomial_long_division(r0, r1, p)
    r = res$remainder %% p

    if (zero_polynomial(r)) break

    r0 = r1
    r1 = r

  }

  r1 = omit_zero(r1)

  # if the last non-zero remainder in the euclid's algorithm calculation
  # is an integer, even if it is not 1, as long as it is coprime with p
  # the gcd=1
  if ((length(r1) == 1) && (coprime(r1, p))) r1 = 1

  return(r1)

}

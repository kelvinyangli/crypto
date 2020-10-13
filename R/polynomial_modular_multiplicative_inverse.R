#
#' Modular inverse
#'
#' Given a polynomail from a polynomial quotient ring R_p=Z_p[x]/(x^N - 1),
#' this function uses the exteneded Euclidean algorithm to find the
#' inverse of the polynomial modulo in R_p. For p prime, the inverse
#' exists if and only if the given polynomial and X^N-1 are coprime.
#' @param a A polynomial represented by its coefficients that is
#' coprime with x^N-1.
#' @param p An prime integer.
#' @export

polynomial_modular_multiplicative_inverse = function(a, N, p) {

  a = omit_zero(a)
  # b=x^N-1
  b = c(-1, rep(0, N - 1), 1) %% p
  gcd = polynomial_gcd(a, b, p)
  if (gcd != 1) stop("the given polynomial is not coprime with
                     x^N - 1! No multiplicative inverse exists!")


  # ensure two polynomials have the same length
  na = length(a)
  nb = length(b)
  if (na < nb) {

    a = c(a, rep(0, nb - na))

  } else {

    b = c(b, rep(0, na - nb))

  }

  u = a
  v = b
  x1 = 1
  x2 = 0

  while(u != 1) {

    q = v %/% u
    r = v %% u

    x = x2 - q * x1

    v = u
    u = r

    x2 = x1
    x1 = x

  }

  return(x1 %% p)

}

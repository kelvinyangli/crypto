#
#' Modular inverse
#'
#' Given a polynomail from a polynomial quotient ring R_p=Z_p[x]/(x^N - 1),
#' this function uses the exteneded Euclidean algorithm to find the
#' inverse of the polynomial modulo in R_p. For p prime, the inverse
#' exists if and only if the given polynomial and X^N-1 are coprime.
#' @param a A polynomial represented by its coefficients. It should be
#' coprime with the polynomial x^N-1. It's degree should be smaller than
#' N. If not, a pre-processing step is neede before running this function.
#' @param N An integer for the degree of the modulus polynomial X^N-1.
#' @param p An prime integer such that polynomial coefficients are
#' taken from Z/pZ.
#' @export

polynomial_modular_multiplicative_inverse = function(a, N, p) {

  a = omit_zero(a) %% p
  b = c(-1, rep(0, N - 1), 1) %% p

  # if gcd != 1
  gcd = polynomial_gcd(a, b, p)
  if (length(gcd) > 1) {

    stop(paste(c("gcd(a, x^N-1)=",gcd),collapse = " "),
      "! No multiplicative inverse exists!")


  } else {

    if (gcd != 1) stop(paste(c("gcd(a, x^N-1)=",gcd),collapse = " "),
                       "! No multiplicative inverse exists!")

  }

  u = a
  v = b # the modulus polynomial
  x1 = 1
  x2 = 0

  # if the remainder r is a non-constant polynomial
  # then gcd(a, b) != 1, hence the program should never
  # proceed to here
  # so if the program has proceeded to here
  # then gcd(a,b)=1
  # stop when the remainder r is a constant that
  # is coprime with p
  repeat {

    # by setting, v always has higher degree than u
    res = polynomial_long_division(v, u, p)
    q = res$quotient
    r = res$remainder

    # polynomial multiplication using FFT
    qTimesX1 = fft_polynomial_convolution(q, x1, N)
    # x = x2 - q * x1
    x = polynomial_addition(x2, -qTimesX1) %% p

    # update corresponding terms
    v = u
    u = r
    x2 = x1
    x1 = x

    # if r is an integer and is coprime with p
    # then stop
    if ((length(r) == 1) && coprime(r, p)) {

      rInverse = integer_modular_multiplicative_inverse(r, p)
      # must ensure to multiply the output by r's inverse
      # because when euclid's algorithm stops at gcd calculation
      # the last non-zero remainder may be an integer that is not 1
      # but coprime with p
      # hence need to multiply x1 by r's inverse to
      # ensure extended euclid's algorithm is solving
      # for the LHS=1
      x1 = x1 * rInverse
      break

    }

  }

  # ensure the output is in the quotient ring
  dx1 = polynomial_degree(x1)
  if (dx1 >= N) x1 = polynomial_quotient(x1, N)
  x1 = omit_zero(x1) %% p

  return(x1)

}

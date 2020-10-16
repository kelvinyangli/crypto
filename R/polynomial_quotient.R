#
#' Polynomial modulo
#'
#' Given a polynomial (stored in its coefficients), this function
#' returns the polynomial modulo x^N - 1, where N is an integer.
#' @param a A vector of polynomial coefficients. The coefficients
#' must be stored in a in the order of (a0,a1, ..., an).
#' @param N An integer that ensures the returned polynomial has
#' degree at most N - 1.
#' @export
#
polynomial_quotient = function(a, N) {

  na = length(a)
  da = polynomial_degree(a)
  if (da < N) {

    b = a

  } else {

    k = ceiling(na / N)
    # fill a with 0s at the back to make the length of a
    # a multiple of N
    if (na < k * N) a = c(a, rep(0, k * N - na))

    b = a[1:N]
    for (i in 1:(k - 1)) {

      b = b + a[(1:N) + i * N]

    }

  }

  b = omit_zero(b)
  return(b)

}


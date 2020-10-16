#
#' Modular inverse
#'
#' This function uses the exteneded Euclidean algorithm to find the
#' inverse of an integer a modulo p. The inverse only exists if a and
#' p are coprime. If F_p is a field, then all none zero elements in it
#' has multiplicative inverse.
#' @param a A positive integer that is less than p.
#' @param p A positive integer.
#' @export

integer_modular_multiplicative_inverse = function(a, p) {

  if (a < 0) stop("a must be positive!")
  if (p < 0) stop("p must be positive!")
  if (!coprime(a, p)) stop("a and p are not coprime! No multiplicative inverse exists!")

  u = a
  v = p
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

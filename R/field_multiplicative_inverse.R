#
#' Modular inverse
#'
#' This function uses the exteneded Euclidean algorithm to find the
#' inverse of an integer a modulo p. The inverse only exists if a and
#' p are coprime. That is, F_p is a field.
#' @param a An integer that is less than p and coprime with p.
#' @param p An integer that is coprime with a.
#' @export

field_multiplicative_inverse = function(a, p) {

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

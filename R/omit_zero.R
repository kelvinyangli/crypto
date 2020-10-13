#
#' Omit zeros
#'
#' Given a polynomial that is stored in its coefficient format, there may
#' be 0s at the back of the coefficient vector. This function omits
#' zeros at the back of the vector, so the length of the vector is the
#' polynomial degree + 1.
#' @param a A vector of polynomial coefficients.
#' @export

omit_zero = function(a) {

  n = polynomial_degree(a) + 1
  a = a[1:n]
  return(a)

}

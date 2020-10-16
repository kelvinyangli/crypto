#
#' Check zero polynomial
#'
#' This function checks if a given polynomial is a zero polynomial.
#' @param a A vector of polynomial coefficients.
#' @export
#
zero_polynomial = function(a) {

  numZeroTerms = length(which(a == 0))
  zero = ifelse(numZeroTerms == length(a), TRUE, FALSE)
  return(zero)

}

#
#' Modular exponentiation
#'
#' This function calculates modular exponentiation in an (space) efficient way.
#' @param b An integer base.
#' @param e An integer exponent.
#' @param m An integer modulus.
#' @export

modular_exponentiation = function(b, e, m) {

  if (m == 1) return(0)

  c = 1
  for (e_prime in 1:e) {

    c = (c * b) %% m

  }

  return(c)

}

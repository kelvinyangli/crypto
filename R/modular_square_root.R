#
#' Modular square root
#'
#' This function calculates the square root of an integer modulo a prime that is congruent to 3 modulo 4. This method is valid only if the given
#' integer has a square root modulo the chosen prime.
#' @param a An integer whose square root exists modulo the prime p.
#' @param p A prime modular.
#' @export

modular_square_root = function(a, p) {

  b = modular_exponentiation_binary(a, (p + 1) / 4, p)

  return(b)

}

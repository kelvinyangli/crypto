#
#' Modular exponentiation
#'
#' This function calculates modular exponentiation using the square-and-multiply method. It is faster than the function modular_exponentiation().
#' @param b An integer base.
#' @param e An integer exponent.
#' @param m An integer modulus.
#' @export

modular_exponentiation_binary = function(b, e, m) {

  eBin = integer2binary(e)
  # since integer2binary returns the binary representation from the most to least sig bits
  # reverse the order
  eBin = rev(eBin)
  n = length(eBin)

  aPrevious = b %% m
  p = aPrevious ^ eBin[1] %% m

  if (n > 1) {

    for (i in 2:n) {

      aCurrent = aPrevious ^ 2 %% m
      aPrevious = aCurrent
      p = (p * (aCurrent ^ eBin[i])) %% m

    }

  }

  return(p)

}

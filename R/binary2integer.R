#
#' Get integer from binary representation
#'
#' This function gets the corresponding integer from the given binary
#' representation.
#' @param x A vector that corresponds to the binary representation
#' of an integer. The left most element is the most significant bit.
#' @export

binary2integer = function(x) {

  x = rev(x)
  k = 0
  for (i in 1:length(x)) {

    if (x[i] == 1) {

      k = k + (2 ^ (i - 1)) * x[i]

    }

  }

  return(k)

}

#
#' Hexadecimal to decimal
#'
#' This function convers hexadecimal to decimal integer representation.
#' @param x An integer.
#' @export

hexadecimal2decimal = function(x) {

  res = x

  if (x >= 10) {

    # number of digits
    if (x %% 10 == 0) {
      n = log(x, 10) + 1
    } else {
      n = ceiling(log(x, 10))
    }


    # value at each digit
    coeff = c()
    for (i in 0:(n - 1)) {
      coeff = c(coeff, floor(x / (10 ^ i)) %% 10)
    }

    res = sum(coeff * (16 ^ (0:(n - 1))))

  }

  return(res)

}

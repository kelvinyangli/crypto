#
#' Binary integer representation
#'
#' This function returns the binary representation of an integer. The output is a vector, in which the left most element is the most significant digit
#' and the right most element is the least significant digit.
#' @param x An integer.
#' @export

integer2binary = function(x) {

  if (x < 2) return(x)

  m = log2(x)

  # if x is an integer power of 2
  if (m %% 1 == 0) return(c(1, rep(0, m)))

  # else
  # the number of bits to represent x in binary format
  n = ceiling(m)

  b = rep(0, n)

  # if x is odd, the last bit is 1, else 0
  if (x %% 2 == 1) b[1] = 1

  # work out the remaining bits in the middle
  if (n > 1) {
    r = x
    # r = x - 2 ^ (n - 1)
    for (e in (n - 1):1) {

      t = r - 2 ^ e
      if (t >= 0) {

        b[e + 1] = 1
        r = t

      }

    }

  }

  return(rev(b))

}

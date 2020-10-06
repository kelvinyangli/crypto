#' Bit-reverse copy of polynomial coefficients for FFT
#'
#' This function returns the bit-reverse copy of polynomial coefficients
#' for the use of efficient FFT implementation. For example, given a
#' polynomial's coefficients a=(1,2,3,4), this function will return
#' A=(1,3,2,4).
#' @param a A vector of polynomial coefficients from the smallest to the
#' highest power. That is, a=(a_0,a_1, ..., a_n-1).
#' @export

bit_reverse = function(a) {

  n = length(a)
  A = a

  for (k in 1:(n - 2)) {

    kBinary = integer2binary(k)
    if (length(kBinary) < log2(n)) {

      kBinary = c(rep(0, log2(n) - length(kBinary)), kBinary)

    }

    jBinary = rev(kBinary)
    j = binary2integer(jBinary)
    A[j + 1] = a[k + 1]

  }

  return(A)

}



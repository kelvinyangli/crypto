#
#' Primality test
#'
#' This function uses the Miller-Rabin test for composite numbers. The test will either return TRUE indicates
#' n is a composite number of NULL indicates unsure.
#' @param n An integer to be tested for primality.
#' @param a Another integer, called a Miller-Rabin witness for the compositeness of n.
#' @export

miller_rabin_primarily_test = function(n, a) {

  if ((n %% 2 == 0) || !coprime(a, n)) {

    return(TRUE) # composite

  }

  k = 1
  repeat {

    q = (n - 1) / (2 ^ k)

    # if q is an odd integer then stop
    if ((q %% 2) == 1) break

    k = k + 1

  }

  # a = a^q mod n, use modular exponentiation to compute
  a = modular_exponentiation_binary(a, q, n)

  if (a %% n == 1) return(NULL) # test fails

  for (i in 1:k) {

    if ((a + 1) %% n == 0) return(NULL)
    a = modular_exponentiation_binary(a, 2, n)

  }

  return(TRUE) # composite

}



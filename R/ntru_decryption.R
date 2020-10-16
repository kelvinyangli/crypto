#
#' NTRU decryption
#'
#' This function conducts NTRU decryption of a plaintext using a secret key.
#' @param N A prime integer for the degree of the modulus polynomial.
#' @param p A prime integer.
#' @param q An integer s.t. gcd(p,q)=gcd(N,q)=1 and q>(6d+1)p to ensure
#' decryption works correctly.
#' @param f A secret key. It is a vector of polynomial coefficients.
#' @param Fp A secret key. It is a vector of polynomial coefficients.
#' @param e The ciphertext. It is a vector of polynomial coefficients.
#' @export

ntru_decryption = function(N, p, q, f, Fp, e) {

  a = fft_polynomial_convolution(f, e, N) %% q
  # the proof of NTRU says the largest coefficient of
  # a is < 0.5*q, so there is no difference if a
  # is center lifted to R
  b = fft_polynomial_convolution(Fp, a, N) %% p
  b = omit_zero(b)

  return(b)

}

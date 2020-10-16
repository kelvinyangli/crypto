#
#' NTRU encryption
#'
#' This function conducts NTRU encryption of a plaintext using a public key.
#' @param N A prime integer for the degree of the modulus polynomial.
#' @param p A prime integer.
#' @param q An integer s.t. gcd(p,q)=gcd(N,q)=1 and q>(6d+1)p to ensure
#' decryption works correctly.
#' @param d An integer for the number of 1s and -1s in the chosen polynomials
#' that are used as private keys.
#' @param h A coefficient vector for the public key for encryption.
#' @param m A coefficient vector for the plaintext polynomial in R_p.
#' Then centerlifted to R to ensure decryption works correctly.
#' So the coefficients should in the range (-0.5p, 0.5p].
#' @export

ntru_encryption = function(N, p, q, d, h, m) {

  # check if m is centerlifted
  if ((max(m) > (q / 2)) || (min(m) <= (-q / 2))) stop("Plaintext is not center lifted!")

  # randomly sample an ephemeral key r
  r = sample_ternary_polynomial(N, d, d)

  # compute ciphertext in R_q
  e = (p * fft_polynomial_convolution(r, h, N)) %% q
  e = polynomial_addition(e, m) %% q
  e = omit_zero(e)

  return(e)

}

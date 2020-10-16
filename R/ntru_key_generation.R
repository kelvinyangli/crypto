#
#' NTRU key generation
#'
#' This function generates random keys for the NTRU cryptosystem
#' based on the chosen parameters.
#' @param N A prime integer for the degree of the modulus polynomial.
#' @param p A prime integer.
#' @param q An integer s.t. gcd(p,q)=gcd(N,q)=1 and q>(6d+1)p to ensure
#' decryption works correctly.
#' @param d An integer for the number of 1s and -1s in the chosen polynomials
#' that are used as private keys.
#' @export

ntru_key_generation = function(N, p, q, d) {

  # check parameter condition
  if (q <= (6 * d + 1) * p) stop("Parameters are incorrect!")

  # randomly sample f,g
  # f is private key
  f = sample_ternary_polynomial(N, d + 1, d)
  # F_p is private key
  Fp = polynomial_modular_multiplicative_inverse(f, N, p)
  # F_q is used to generate public key
  # it should be private too, for otherwise knowing F_q
  # and the public key h will be able to figure out g?
  Fq = polynomial_modular_multiplicative_inverse(f, N, q)
  # g is private, it is used to generate public key
  g = sample_ternary_polynomial(N, d, d)
  # h is public key
  h = fft_polynomial_convolution(Fq, g, N) %% q

  ls = list("private f" = f, "private Fp" = Fp, "public h" = h)

  return(ls)

}

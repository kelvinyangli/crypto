#
#' Diffie-Hellman key exchange
#'
#' This function implements the Diffie-Hellman key exchange procedure. The security of this procedure is based on the hardness assumption of the
#' discrete logarithm problem. This is not a complete cryptosystem because it only allows two parties to exchange a secrete key on a public
#' channel securely.
#' @param p A large prime number that is used to define the group of units \eqn{\mathbb{F}_p^*} in the finite filed \eqn{F_p}. In practice, p should be
#' a prime of at least 1000 bits to maintain the security of the key exchange procedure.
#' @param g An integer in \eqn{\mathbb{F}_p^*}. For various of reasons, an ideal g should have a large prime order in the filed. Some guidelines suggest
#' g's order is approximately p/2.
#' @param a A secret integer used by Alice to calculate \eqn{g^a} modulo p.
#' @param b A secret integer used by Bob to calculate \eqn{g^b} modulo p.
#' @export

diffie_hellman_key_exchange = function(p, g, a, b) {

  A = modular_exponentiation_binary(g, a, p)
  B = modular_exponentiation_binary(g, b, p)
  C = modular_exponentiation_binary(A, b, p)

  return(C)

}

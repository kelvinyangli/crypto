#
#' Polynomial long division
#'
#' This function divides a polynomial by another using long division. It
#' may not be efficient. The output is a list of quotient and remainder
#' polynomials, stored in their coefficient formats. To ensure the
#' quotient and remainder polynomials to have integer coefficients,
#' modular arithmics are needed in the ring Z/pZ. If p is prime,
#' integer coefficients exist. Otherwise, integer coefficients may not
#' exist.
#' @param a A vector of polynomial coefficients from the smallest to the
#' largest term.
#' @param b A vector of polynomial coefficients from the smallest to
#' the largest term. The polynomial has to be non-zero. It does not need
#' to be monic.
#' @param p An integer. The polynomial coefficients are taken from
#' Z/pZ. If p is prime, integer coefficients of the quotient and
#' remainder polynomials exist. Otherwise, integer coefficient may not
#' exist. By default, it is NULL.
#' @export

polynomial_long_division = function(a, b, p = NULL) {

  da = polynomial_degree(a)
  db = polynomial_degree(b)

  if (db > da) stop("The a is smaller than b!")

  # omit 0s at the back to avoid issues
  a = omit_zero(a)
  b = omit_zero(b)

  if (length(b) < length(a)) b = c(b, rep(0, length(a) - length(b)))

  q = rep(0, da + 1)
  r = a
  dr = da
  bLeadingTerm = b[db + 1]
  if (bLeadingTerm != 1) {

    # find bLeadingTerm's multiplicative inverse in mod p
    bLeadingTermInverse =
      integer_modular_multiplicative_inverse(bLeadingTerm, p)

  }

  # sum(r) != 0) && degree r >= degree b
  while ((sum(r) != 0) && (dr >= db)) {

    t = rep(0, da + 1)
    rLeadingTerm = r[dr + 1]
    # if p is not null and the leading term of the remainder is NOT
    # divisible by the leading term of b then...
    if (!is.null(p) && !is_divisible(rLeadingTerm, bLeadingTerm)) {

      t[dr - db + 1] = (rLeadingTerm * bLeadingTermInverse) %% p

    } else {

      t[dr - db + 1] = rLeadingTerm / bLeadingTerm

    }

    q = q + t
    # t*b
    tb = fft_polynomial_multiplication(t, b)[1:(da + 1)]

    if (!is.null(p)) {

      tb = tb %% p

    }

    r = r - tb
    if (!is.null(p)) {

      # q = q %% p
      r = r %% p

    }

    dr = polynomial_degree(r)

  } # end while

  dq = polynomial_degree(q)
  dr = polynomial_degree(r)
  ls = list("quotient" = q[1:(dq + 1)], "remainder" = r[1:(dr + 1)])
  return(ls)

}





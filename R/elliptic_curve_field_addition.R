#
#' Elliptic curve addition in field
#'
#' This function adds two points P and Q on an elliptic curve E: Y^2 = X^3 + AX + B over a field F_p. The two points are not symmetric against the x-axis.
#' None of them is the "origin" of E. If the coordinates of Q are not given, then the function calculates the sum P+P=2P.
#' @param A The coefficient of X in the elliptic curve E.
#' @param p The filed prime.
#' @param x1 The x-coordinate of the point P in the filed.
#' @param y1 The y-coordinate of the point P in the filed.
#' @param x2 The x-coordinate of the point Q in the filed. The default is NULL, indicating P=Q.
#' @param y2 The y-coordinate of the point Q in the filed. The default is NULL, indicating P=Q.
#' @export

elliptic_curve_field_addition = function(A, p, x1, y1, x2 = NULL, y2 = NULL) {


  if (is.null(x2) && is.null(y2)) {# when P = Q

    a = (2 * y1) %% p
    b = field_multiplicative_inverse(a, p)
    lambda = (3 * x1 ^ 2 + A) * b %% p
    x2 = x1

  } else {# when P != Q

    a = (x2 - x1) %% p
    b = field_multiplicative_inverse(a, p)
    lambda = (y2 - y1) * b %% p

  }

  x3 = (lambda ^ 2 - x1 - x2) %% p
  y3 = (lambda * (x1 - x3) - y1) %% p

  return(c(x3, y3))

}

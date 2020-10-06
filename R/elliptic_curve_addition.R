#
#' Elliptic curve addition
#'
#' This function adds two points P and Q on an elliptic curve E: Y^2 = X^3 + AX + B. The two points are not symmetric against the x-axis.
#' None of them is the "origin" of E. If the coordinates of Q are not given, then the function calculates the sum P+P=2P.
#' @param A The coefficient of X in the elliptic curve E.
#' @param x1 The x-coordinate of the point P.
#' @param y1 The y-coordinate of the point P.
#' @param x2 The x-coordinate of the point Q. The default is NULL, indicating P=Q.
#' @param y2 The y-coordinate of the point Q. The default is NULL, indicating P=Q.
#' @export

elliptic_curve_addition = function(A, x1, y1, x2 = NULL, y2 = NULL) {


  if (is.null(x2) && is.null(y2)) {# when P = Q

    lambda = (3 * x1 ^ 2 + A) / (2 * y1)
    x2 = x1

  } else {# when P != Q

    lambda = (y2 - y1) / (x2 - x1)

  }

  x3 = lambda ^ 2 - x1 - x2
  y3 = lambda * (x1 - x3) - y1

  return(c(x3, y3))

}

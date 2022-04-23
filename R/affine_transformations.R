
#'
#' Rotate a Matrix Through An Angle
#'
#' @param angle Numeric.  Angle in degrees.  Positive values give CCW rotations.
#' @return A numeric matrix that should be used to pre-multiply a matrix of the
#'         data points to be rotated.  The data matrix should have rows of `x`, `y`,
#'         and `rep(1), length(x)` (i.e. augmented to give homogeneous coordinates).
#'
#' @author David T. Harvey, Bryan A. Hanson
#' @noRd
#'
#' @examples
#'
#' # common example data
#' # easy to visualize set of points centered on origin "The Big Dipper"
#' tm1 <- matrix(c(
#'   -1.0, 0.5, 0.25, -0.75,
#'   0.5, 1.0, -0.75, -0.5,
#'  rep(1.0, 4)), byrow = TRUE, nrow = 3)
#' # same set of points, but not centered on the origin
#' tm2 <- tm1 + matrix(c(
#'   rep(2, ncol(tm1)),
#'   rep(2, ncol(tm1)),
#'   rep(0.0, ncol(tm1))), # note 0.0 don't want to modify 3rd row
#'   byrow = TRUE, nrow = 3)
#' 
#' # Rotate a Matrix Through An Angle
#'
#' # graphical demo using tm1, centered on origin
#' plot(-5:5, -5:5, type = "n")
#' grid()
#' points(tm1[1, ], tm1[2, ], col = "black")
#' points(0, 0, col = "black", pch = 4)
#' segments(tm1[1, ], tm1[2, ], tm1[1, c(2:4, 1)], tm1[2, c(2:4, 1)])
#' 
#' res <- LearnPCA:::.rot_mat_2D(30) %*% tm1
#' points(res[1, ], res[2, ], col = "red")
#' segments(res[1, ], res[2, ], res[1, c(2:4, 1)], res[2, c(2:4, 1)], col = "red")
#' 
#' # graphical demo using tm2, centered on 2,2
#' plot(-2:4, -2:4, type = "n")
#' grid()
#' points(tm2[1, ], tm2[2, ], col = "black")
#' points(2, 2, col = "black", pch = 4)
#' segments(tm2[1, ], tm2[2, ], tm2[1, c(2:4, 1)], tm2[2, c(2:4, 1)])
#' 
#' res <- LearnPCA:::.rot_mat_2D(15) %*% tm2
#' points(res[1, ], res[2, ], col = "pink") # demonstrates rotation is about 0,0
#' segments(res[1, ], res[2, ], res[1, c(2:4, 1)], res[2, c(2:4, 1)], col = "pink")
#' segments(0, 0, tm2[1, 3], tm2[2, 3], col = "black", lty = 2)
#' segments(0, 0, res[1, 3], res[2, 3], col = "pink", lty = 2)
#' 
#' # translate to 0,0, rotate, translate back to origin
#' res <- LearnPCA:::.trans_mat_2D(2, 2) %*% LearnPCA:::.rot_mat_2D(45) %*%
#'   LearnPCA:::.trans_mat_2D(-2, -2) %*% tm2
#' points(res[1, ], res[2, ], col = "red")
#' segments(res[1, ], res[2, ], res[1, c(2:4, 1)], res[2, c(2:4, 1)], col = "red")
#' 

.rot_mat_2D <- function(angle) {
  theta <- angle * pi / 180
  mat <- matrix(c(
    cos(theta), sin(theta), 0.0,
    -sin(theta), cos(theta), 0.0,
    0.0, 0.0, 1.0
  ),
  ncol = 3
  )
}

#'
#' Translate a Matrix By a Given x, y Change
#'
#' @param x Numeric.  Change in x-coordinate.
#' @param y Numeric.  Change in y-coordinate.
#' @return A numeric matrix that should be used to pre-multiply a matrix of the
#'         data points to be translated.  The data matrix should have rows of `x`, `y`,
#'         and `rep(1), length(x)` (i.e. augmented to give homogeneous coordinates).
#'
#' @author David T. Harvey, Bryan A. Hanson
#' @noRd
#'
#' @examples
#'
#' # common example data
#' # easy to visualize set of points centered on origin "The Big Dipper"
#' tm1 <- matrix(c(
#'   -1.0, 0.5, 0.25, -0.75,
#'   0.5, 1.0, -0.75, -0.5,
#'  rep(1.0, 4)), byrow = TRUE, nrow = 3)
#' # same set of points, but not centered on the origin
#' tm2 <- tm1 + matrix(c(
#'   rep(2, ncol(tm1)),
#'   rep(2, ncol(tm1)),
#'   rep(0.0, ncol(tm1))), # note 0.0 don't want to modify 3rd row
#'   byrow = TRUE, nrow = 3)
#'
#' # Translate a Matrix By a Given x, y Change
#'
#' # graphical demo using tm1, centered on origin
#' plot(-5:5, -5:5, type = "n")
#' grid()
#' points(tm1[1, ], tm1[2, ], col = "black") # original data
#' points(0, 0, col = "black", pch = 4)
#' segments(tm1[1, ], tm1[2, ], tm1[1, c(2:4, 1)], tm1[2, c(2:4, 1)])
#' 
#' res <- LearnPCA:::.trans_mat_2D(2, 2) %*% tm1
#' points(res[1, ], res[2, ], col = "red")
#' points(2, 2, col = "red", pch = 4) # mark manually computed "center"
#' segments(res[1, ], res[2, ], res[1, c(2:4, 1)], res[2, c(2:4, 1)], col = "red")
#' 
#' res <- LearnPCA:::.trans_mat_2D(-4, 2) %*% tm1
#' points(res[1, ], res[2, ], col = "blue")
#' points(-4, 2, col = "blue", pch = 4)
#' segments(res[1, ], res[2, ], res[1, c(2:4, 1)], res[2, c(2:4, 1)], col = "blue")

#' res <- LearnPCA:::.trans_mat_2D(0, -3) %*% tm1
#' points(res[1, ], res[2, ], col = "orange")
#' points(0, -3, col = "orange", pch = 4)
#' segments(res[1, ], res[2, ], res[1, c(2:4, 1)], res[2, c(2:4, 1)], col = "orange")
#' 
#' # graphical demo using tm2, not centered on origin
#' plot(-5:5, -5:5, type = "n")
#' grid()
#' points(tm2[1, ], tm2[2, ], col = "black")
#' points(2, 2, col = "black", pch = 4) # original data
#' segments(tm2[1, ], tm2[2, ], tm2[1, c(2:4, 1)], tm2[2, c(2:4, 1)])
#' 
#' res <- LearnPCA:::.trans_mat_2D(-2, -2) %*% tm2 # translate back to 0, 0
#' points(res[1, ], res[2, ], col = "gray")
#' points(0, 0, col = "gray", pch = 4) # mark manually computed "center"
#' segments(res[1, ], res[2, ], res[1, c(2:4, 1)], res[2, c(2:4, 1)], col = "gray")
#' 
#' res <- LearnPCA:::.trans_mat_2D(2, 2) %*% tm2
#' points(res[1, ], res[2, ], col = "red")
#' points(4, 4, col = "red", pch = 4)
#' segments(res[1, ], res[2, ], res[1, c(2:4, 1)], res[2, c(2:4, 1)], col = "red")
#' 
#' res <- LearnPCA:::.trans_mat_2D(-4, 2) %*% tm2
#' points(res[1, ], res[2, ], col = "blue")
#' points(-2, 4, col = "blue", pch = 4)
#' segments(res[1, ], res[2, ], res[1, c(2:4, 1)], res[2, c(2:4, 1)], col = "blue")
#' 
#' res <- LearnPCA:::.trans_mat_2D(0, -3) %*% tm2
#' points(res[1, ], res[2, ], col = "orange")
#' points(2, -1, col = "orange", pch = 4)
#' segments(res[1, ], res[2, ], res[1, c(2:4, 1)], res[2, c(2:4, 1)], col = "orange")
#' 

.trans_mat_2D <- function(x, y) {
  mat <- matrix(c(
    1.0, 0.0, 0.0,
    0.0, 1.0, 0.0,
    x, y, 1.0
  ),
  ncol = 3
  )
}

#'
#' Project a Data Set Onto a Line
#'
#' https://math.stackexchange.com/a/62718/177376 or any number of other places
#'
#' @param point_on_line Numeric one column matrix containing one x,y value on the line.  It is
#'        assumed that the line goes through 0,0. Do not augment the matrix.
#' @param data Numeric matrix.  x,y in columns.  These points will be projected
#'        onto the line described by `point`. Do not augment the matrix.
#' @return A numeric matrix with x,y in columns.
#'
#' @author David T. Harvey, Bryan A. Hanson
#' @noRd
#'
#' @examples
#'
#' # Project a Data Set Onto a Line
#'
#' set.seed(1)
#' p <- matrix(c(3, 5), ncol = 1)
#' v <- matrix(rnorm(20), nrow = 2)
#' tst <- LearnPCA:::.project_data_onto_line(p, v)
#' 
#' plot(v[1, ], v[2, ], type = "p", pch = 20, asp = 1)
#' abline(v = 0, h = 0, col = "gray")
#' abline(coef = c(0.0, 5 / 3))
#' points(tst[1, ], tst[2, ], col = "red")
#' segments(v[1, ], v[2, ], tst[1, ], tst[2, ])
#' 

.project_data_onto_line <- function(point_on_line, data) {
  # the line is defined by passing through 0,0 & point
  if (!inherits(point_on_line, "matrix")) stop("point_on_line must be a matrix")
  if (ncol(point_on_line) != 1L) stop("point_on_line must be a matrix with one column")
  if (!inherits(data, "matrix")) stop("data must be a matrix")
  if (nrow(data) != 2L) stop("data must be a matrix with two rows")
  vec <- (tcrossprod(point_on_line) %*% data) / c(crossprod(point_on_line))
}


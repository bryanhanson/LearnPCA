#' Do not delete this file!

#' Tests for the affine_transformations functions
#'
#' Common test data
#'
#' Easy to visualize set of points centered on origin "The Big Dipper"
tm1 <- matrix(c(
  -1.0, 0.5, 0.25, -0.75,
  0.5, 1.0, -0.75, -0.5,
  rep(1.0, 4)), byrow = TRUE, nrow = 3)
#' The Big Dipper, but not centered on the origin
tm2 <- tm1 + matrix(c(
  rep(2, ncol(tm1)),
  rep(2, ncol(tm1)),
  rep(0.0, ncol(tm1))), # note 0.0 don't want to modify 3rd row
  byrow = TRUE, nrow = 3)
#' 
#' Rotate a Matrix Through An Angle
#'
#' Check for net zero rotation by chaining calls
tst1 <- LearnPCA:::.rot_mat_2D(15) %*% LearnPCA:::.rot_mat_2D(-15) %*% tm1
expect_true(all.equal(tm1, tst1))
tst2 <- LearnPCA:::.rot_mat_2D(15) %*% LearnPCA:::.rot_mat_2D(-15) %*% tm2
expect_true(all.equal(tm2, tst2))
#' Check for rotation by 360 gives the same matrix back
tst3 <- LearnPCA:::.rot_mat_2D(360) %*% tm1
expect_true(all.equal(tst3, tm1))
tst4 <- LearnPCA:::.rot_mat_2D(360) %*% tm2
expect_true(all.equal(tst4, tm2))
#'
#' Translate a Matrix By a Given x, y Change
#'
#' Check for net zero translation by chaining calls
tst5 <- LearnPCA:::.trans_mat_2D(-2, -2) %*% LearnPCA:::.trans_mat_2D(2, 2) %*% tm1
expect_true(all.equal(tm1, tst5))
tst6 <- LearnPCA:::.trans_mat_2D(1.5, -2.5) %*% LearnPCA:::.trans_mat_2D(-1.5, 2.5) %*% tm2
expect_true(all.equal(tm2, tst6))
#' Move tm1 to tm2 (tm2 is offset by 2,2, i.e. the answer is known)
tst7 <- LearnPCA:::.trans_mat_2D(2, 2) %*% tm1
expect_true(all.equal(tm2, tst7))
#'
#' Project a Point Onto a Line
#'
hline <- matrix(c(5, 0), ncol = 1)
p <- matrix(c(2.5, 2.5), nrow = 2)
tst8 <- LearnPCA:::.project_data_onto_line(hline, p)
expect_true(all.equal(tst8, matrix(c(2.5, 0), ncol = 1)))
vline <- matrix(c(0, 5), ncol = 1)
tst9 <- LearnPCA:::.project_data_onto_line(vline, p)
expect_true(all.equal(tst9, matrix(c(0.0, 2.5), ncol = 1)))



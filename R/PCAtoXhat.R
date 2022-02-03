#'
#' Use PCA Results to Reconstruct All or Part of the Original Data Set
#'
#' This function allows one to reconstruct an approximation (\code{Xhat}) of the
#' original data
#' using some or all of the principal components, starting from the results of PCA.
#' Inspired by and follows \url{https://stackoverflow.com/a/23603958/633251} very closely.
#' We are grateful for this post by StackOverflow contributor "Marc in the box."
#' 
#' @param pca An object of class \code{prcomp} or \code{princomp} (automatically detected).  #'        The results of data reduction by PCA.  
#'
#' @param ncomp Integer.  The number of principal components to use in reconstructing
#'        the data set.  Must be no larger than the number of variables.  If not
#'        specified, all the components are used and the original data set is 
#'        reconstructed.
#'
#' @return A matrix with the same dimensions as \code{pca$x} (the dimensions of the
#'         original data set).
#'
#' @export
#'
#' @tests tinytest
#' # Data from ?prcomp
#' C <- chol(S <- toeplitz(.9 ^ (0:31)))
#' set.seed(17)
#' X <- matrix(rnorm(32000), 1000, 32)
#' Z <- X %*% C
#' pca <- prcomp(Z)
#' prin <- princomp(Z)
#' 
#' # Test that original data set is returned when ncomp = ncol(pca$x)
#' tst1 <- PCAtoXhat(pca)
#' expect_true(all.equal(tst1, Z, check.attributes = FALSE))
#' tst2 <- PCAtoXhat(prin)
#' expect_true(all.equal(tst2, Z, check.attributes = FALSE))
#' 
#' # Test for handling of bad arguments
#' expect_error(PCAtoXhat(pca, 35))
#' expect_error(PCAtoXhat("test"))
#' 
#' @examples
#' # Example data from ?prcomp (see discussion at Stats.StackExchange.com/q/397793)
#' C <- chol(S <- toeplitz(.9 ^ (0:31)))
#' set.seed(17)
#' X <- matrix(rnorm(32000), 1000, 32)
#' Z <- X %*% C
#'
#' pcaz <- prcomp(Z)
#' tst <- PCAtoXhat(pcaz)
#' all.equal(tst, Z, check.attributes = FALSE)
#'
#' # Plot to show the effect of increasing ncomp
#' 
#' ntests <- ncol(Z)
#' rmsd <- rep(NA_real_, ntests)
#' for (i in 1:ntests) {
#' 	ans <- XtoPCAtoXhat(X, i, sd)
#' 	del<- ans - X
#' 	rmsd[i] <- sqrt(sum(del^2)/length(del)) # RMSD
#' }
#' plot(rmsd, type = "b",
#'   main = "Root Mean Squared Deviation\nReconstructed - Original Data",
#'   xlab = "No. of Components Retained", ylab = "RMSD")
#' abline(h = 0.0, col = "pink")
#' 
PCAtoXhat <- function(pca, ncomp = NULL) {

  if (inherits(pca, "prcomp")) {
    if (!is.null(ncomp)) if (ncomp > ncol(pca$x)) stop("ncomp cannot be larger than ncol(pca$x)")
    if (is.null(ncomp)) ncomp <- ncol(pca$x)
    Xhat <- pca$x[, 1:ncomp] %*% t(pca$rotation[, 1:ncomp])
    if(pca$scale[1] != FALSE) Xhat <- scale(Xhat, center = FALSE, scale = 1/pca$scale)
    if(pca$center[1] != FALSE) Xhat <- scale(Xhat, center = -pca$center, scale = FALSE)
  }

  if (inherits(pca, "princomp")) {
    if (!is.null(ncomp)) if (ncomp > ncol(pca$scores)) stop("ncomp cannot be larger than ncol(pca$scores)")
    if (is.null(ncomp)) ncomp <- ncol(pca$scores)
    Xhat <- pca$scores[, 1:ncomp] %*% t(pca$loadings[, 1:ncomp])
    if(pca$scale[1] != FALSE) Xhat <- scale(Xhat, center = FALSE, scale = 1/pca$scale)
    if(pca$center[1] != FALSE) Xhat <- scale(Xhat, center = -pca$center, scale = FALSE)
  }


  Xhat
}

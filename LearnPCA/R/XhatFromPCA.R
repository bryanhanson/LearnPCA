#'
#' Reconstruct Original Data from PCA Results
#'
#' This function allows one to reconstruct an approximation of a data set
#' that has been reduced by PCA, by using a limited number of principal components.
#' Inspired by \url{https://stats.stackexchange.com/q/229092/26909}. We are grateful
#' for this post by StackOverflow contributor Amoeba.
#' 
#' @param X A matrix of data, or a structure which can be coerced to a matrix.
#'        Samples should be in rows, and variables in columns.
#'
#' @param ncomp Integer.  The number of principal components to use in reconstructing
#'        the data set.  Must be no larger than the number of variables.
#'
#' @param scale.fun A function to use to scale the data.  If \code{NULL} no scaling
#'        will be done.
#'
#' @return A matrix with the same dimensions as \code{X}.
#'
#' @importFrom stats prcomp
#'
#' @export
#'
#' @examples
#' # Example data from ?prcomp (see discussion at Stats.StackExchange.com/q/397793)
#' C <- chol(S <- toeplitz(.9 ^ (0:31)))
#' set.seed(17)
#' X <- matrix(rnorm(32000), 1000, 32)
#' Z <- X %*% C
#'
#' tst <- XhatFromPCA(Z)
#' mean(tst - Z)
#'
#' # Plot to show the effect of increasing ncomp
#' 
#' ntests <- ncol(Z)
#' val1 <- rep(NA_real_, ntests)
#' val2 <- rep(NA_real_, ntests)
#' for (i in 1:ntests) {
#' 	ans <- XhatFromPCA(X, i, sd)
#' 	del <- ans - X
#' 	val1[i] <- sqrt(sum(del^2)/length(del)) # RMSD
#' 	val2[i] <- mean(del)
#' }
#' plot(val1, type = "b", ylim = c(0.0, max(val1)),
#'   main = "Root Mean Squared Deviation", xlab = "No. of Components Retained", ylab = "RMSD")
#' abline(h = 0.0, col = "pink")
#' me <- .Machine$double.eps
#' plot(val2, type = "b", ylim = c(-me, me), main = "Residual Error",
#'   xlab = "No. of Components Retained", ylab = "Mean Error")
#' thres <- c(-me, 0.0, me)
#' abline(h = thres, col = "pink") # all values below .Machine$double.eps
#' 
XhatFromPCA <- function(X, ncomp = 3, scale.fun = NULL) {
  # Check arguments
  if (ncomp > ncol(X)) stop("ncomp cannot be larger than ncol(X)")
  if (!is.matrix(X)) X <- as.matrix(X)

  # Determine scaling action
  if (!is.null(scale.fun)) {
    if (!inherits(scale.fun, "function")) stop("scale.fun was not a function")
    Xfac <- apply(X, 2, scale.fun)
  }
  if (is.null(scale.fun)) Xfac <- rep(1.0, ncol(X))

  # Center the data
  Xmu <- colMeans(X)
  Xscl <- scale(X, scale = Xfac) # center = TRUE

  # Carry out PCA
  Xpca <- prcomp(Xscl, center = FALSE) # center FALSE since inbound data was already centered

  # Compute Xhat
  Xhat <- Xpca$x[,1:ncomp] %*% t(Xpca$rotation[,1:ncomp])

  # Undo scaling
  Xhat <- scale(Xhat, center = FALSE, scale = 1/Xfac)

  # Undo centering
  Xhat <- scale(Xhat, center = -Xmu, scale = FALSE)
}



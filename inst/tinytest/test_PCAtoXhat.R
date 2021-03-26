# File created by roxut; edit the function definition file, not this file
 
# Data from ?prcomp
C <- chol(S <- toeplitz(.9 ^ (0:31)))
set.seed(17)
X <- matrix(rnorm(32000), 1000, 32)
Z <- X %*% C
pca <- prcomp(Z)

# Test that original data set is returned when ncomp = ncol(pca$x)
tst1 <- PCAtoXhat(pca)
expect_true(all.equal(tst1, Z, check.attributes = FALSE))

# Test for handling of bad arguments
expect_error(PCAtoXhat(pca, 35))
expect_error(PCAtoXhat("test"))

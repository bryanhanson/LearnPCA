# Do not delete this file!
 
# Data from ?prcomp
C <- chol(S <- toeplitz(.9 ^ (0:31)))
set.seed(17)
X <- matrix(rnorm(32000), 1000, 32)
Z <- X %*% C

# Test that each function used as intended returns the same result
tst1 <- XtoPCAtoXhat(Z, 32)
tst2 <- PCAtoXhat(prcomp(Z), 32)
expect_true(all.equal(tst1, tst2, check.attributes = FALSE))


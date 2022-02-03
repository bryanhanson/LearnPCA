# File created by roxut; edit the function definition file, not this file

# Test found in XtoPCAtoXhat.R:25 (file:line)
  
# Data from ?prcomp
C <- chol(S <- toeplitz(.9 ^ (0:31)))
set.seed(17)
X <- matrix(rnorm(32000), 1000, 32)
Z <- X %*% C

# Test that passing of quoted or not quoted scale.fun gives the same answer
tst1 <- XtoPCAtoXhat(Z, 5, "sd")
tst2 <- XtoPCAtoXhat(Z, 5, sd)
expect_true(all.equal(tst1, tst2))

# Test that scale.fun is parsed correctly when specified in various ways
test_fun <- function(x) {mean(x)}
tst3 <- XtoPCAtoXhat(Z, 5, test_fun)
tst4 <- XtoPCAtoXhat(Z, 5, function(x) mean(x))
expect_true(all.equal(tst4, tst4))

# Test that original data set is returned when ncomp = ncol(X) & no scaling
tst5 <- XtoPCAtoXhat(Z, ncol(Z))
expect_true(all.equal(tst5, Z, check.attributes = FALSE))

# Test that original data set is returned when ncomp = ncol(X) & no scaling
tst6 <- XtoPCAtoXhat(Z, ncol(Z), sd)
expect_true(all.equal(tst6, Z, check.attributes = FALSE))

# Test for handling of bad arguments
expect_error(XtoPCAtoXhat(Z, 35))
expect_error(XtoPCAtoXhat(Z, 3, "nonsense"))

[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
![R-CMD-build-check](https://github.com/bryanhanson/LearnPCA/workflows/R-CMD-build-check/badge.svg)

## What is LearnPCA?

`LearnPCA` is an `R` package in development by David T. Harvey and Bryan A. Hanson.  For a sense of the project, install the package and do `browseVignettes("LearnPCA")`.

### Installing LearnPCA from Github:

````r
install.packages("remotes")
library("remotes")
install_github(repo = "bryanhanson/LearnPCA@master")
library("LearnPCA")
````

If you use `@some_other_branch` you can download other branches that might be available.  They may or may not pass CRAN checks and thus may not install automatically using the method above.  Check the NEWS file to see what's up.

### To view the Vignettes:

````r
browseVignettes("LearnPCA")
````

### License Information

`LearnPCA` is distributed under the GPL-3 license, as stated in the DESCRIPTION file.  For more info, see the [GPL site.](https://www.gnu.org/licenses/gpl.html)

Questions?  hanson@depauw.edu or harvey@depauw.edu

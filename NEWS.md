# LearnPCA 0.3.0 2024-04-23
* Branch master renamed to main, with corresponding changes to documentation and GHA scripts.
* Added pkgs `rpart`, `class`, `nnet` to Depends: (found during stress testing, needed by `chemometrics` and somehow not automatically present).
* Fix navigation error in top_matter.md, found by DTH.

# LearnPCA 02.3 2022-08-03
* New material on NIPALS in Vig_06.  Not on CRAN.

# LearnPCA 0.2.1 2022-07-18
* Fixed a problem with cross-references to figures and tables in Vig_06. Affects documentation only, not CRAN version.

# LearnPCA 0.2.0 2022-05-01
* Added shiny app in function `PCsearch`.

# LearnPCA 0.1.4 2022-03-31
* Removed use of `kableExtra` as CRAN had detected problems there, and no fix is imminent.

# LearnPCA 0.1.3 2022-03-12
* Vignettes are now properly ordered in all locations where they are listed, thanks to a PR by Sergio Oller.

# LearnPCA 0.1.0 2022-01-31
* First release to CRAN.

### NOTE ABOUT `bootstrap` option for confidence intervals

verisr uses the `binom` package for simple bootstrapped confidence intervals (as a faster alternative to MCMC generated ones.).  However, the `binom` package has a bug in the `binom.bayes` function (the one we use).  An example can be seen below

``` r
test2 <- dput(data.frame(x=c(0,6,151,4), n=c(1699, 1699, 1699, 1699)))
#> structure(list(x = c(0, 6, 151, 4), n = c(1699, 1699, 1699, 1699
#> )), class = "data.frame", row.names = c(NA, -4L))
binom::binom.bayes(test2$x, test2$n)
#> Warning in binom::binom.bayes(test2$x, test2$n): 1 confidence interval failed to converge (marked by '*').
#>   Try changing 'tol' to a different value.
#> Error in `$<-.data.frame`(`*tmp*`, "method", value = structure(c(1L, 1L, : replacement has 3 rows, data has 4
```


Looking through the `binom.bayes` function (`binom` version 1.1-1), it appears to be the use of `ci$error` rather than `error` in the line 
```
res$method <- factor(sprintf("bayes%s", ifelse(ci$error, "*", "")))
```
is to blame.

`ci$error` has had the ends removed from it while `error` is the full length of the input vector.

Unfortunately, this fix will require the package author to update the package (or me to clone it).  The package dates from January 2014, and so while I have asked for an update from the package author, I can't be sure it'll be received and acted upon.

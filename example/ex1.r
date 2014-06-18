##   Please replace "NotImplemented" with working code per the specification
##   in the comment after the function header.  To get an idea of whether
##   your function implementation works, you can run the tests at the bottom
##   of the file by calling this file with `Rscript` from a Bash prompt.

mySD <- function(x) {
    ## Take in a vector of numbers and return its standard deviation.
    ##m = myMean(x)
    ##squareddifferences = 0
    ##for (i in x){
	##x[i] = (x[i]-m)^2}
    ##return(sqrt(sum(x)/length(x)))
    return(sqrt(var(x)))
    ##m = myMean(x)
    ##x = (x - m)^2
    ##return(sqrt(sum(x)/length(x)))
}

myMean <- function(x) {
    ## Take in a vector of numbers and return its mean.
    ## return(sum(x)/length(x))
    x.sum = 0
    x.length = length(x)
    for (i in x){
	x.sum = x.sum + x[i]}
    return(x.sum/x.length)
}

## tests
library(RUnit)
errmsg <- function(err) {
     print(paste("MY_ERROR:  ",err))
}

x = c(1:100)
x.sd = 29.011491975882016447
x.mean = 50.5
tryCatch(checkEquals(mySD(x), x.sd),
         error = function(err) errmsg(err))
tryCatch(checkEquals(myMean(x), x.mean),
         error = function(err) errmsg(err))

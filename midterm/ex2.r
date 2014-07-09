library(RUnit)
errMsg <- function(err) print(err)


# (3 points) Implement the num.factors function
# 
# Takes the following arguments:
#   d: a dataframe
#
# Returns the number of factor vectors in the dataframe.

num.factors = function(d) {
	ans <-sum(mapply(is.factor,unname(d)))
	return(ans)
}

first = data.frame(x=rnorm(10), y=as.factor(1:10))
tryCatch(checkEquals(1, num.factors(first)),
         error=function(err) errMsg(err))

second = data.frame(x=as.factor(1:2), y=as.factor(c(T,F)))
tryCatch(checkEquals(2, num.factors(second)),
         error=function(err) errMsg(err))

# (3 points) Implement the num.factors2 function
# 
# Takes the following arguments:
#   l: a list of dataframes
#
# Returns the total number of factor vectors in
# all of the dataframes in the list.
#
# Hint:  Use the previous function num.factors

num.factors2 = function(l) {
	ans <-sum(unlist(lapply(l,num.factors)))
	return(ans)
}

list1 = list(first, second)
tryCatch(checkEquals(3, num.factors2(list1)),
         error=function(err) errMsg(err))

third = data.frame(z=as.factor(c("f1", "f2")))
list2 =  list(first, second, third)
tryCatch(checkEquals(4, num.factors2(list2)),
         error=function(err) errMsg(err))


# (4 points) Implement the sum.or.product function
# 
# Takes the following arguments:
#   x: a numeric vector
#   y: a numeric vector
#
# Returns a numerical vector where
#  if every element of x is greater than the
#  corresponding element of y,
#    then return x+y
#  else return x*y

sum.or.product = function(x, y=10) {
	logical <- x >y
	ans <- vector(,length= length(logical))
	for(i in (1:length(logical))){
		if(logical[i]){
			ans[i] = x[i] + y[i]}
		else{
			ans[i] = x[i]*y[i]}}
	return(ans) 
}

x=1:10
y=x
tryCatch(checkEquals(x*y, sum.or.product(x,y)),
         error=function(err) errMsg(err))

x=2*y
tryCatch(checkEquals(x+y, sum.or.product(x,y)),
         error=function(err) errMsg(err))



# (4 points) Implement the odd function
# 
# Takes the following arguments:
#   x: a numeric vector
#   flip: TRUE/FALSE 
#
# Returns a logical vector where
#  if flip is FALSE, then each element is
#     TRUE when the number is odd
#     FALSE when the number is even
#  if flip is TRUE, then each element is
#     TRUE when the number is even
#     FALSE when the number is odd

odd = function(x, flip=FALSE) {
	ans <- vector(,length = length(x))
	if(flip){
		for(i in (1:length(x))){
			if(x[i] %%2 == 1){
				ans[i] = FALSE}
			else{
				ans[i] = TRUE}}}
	else{
        for(i in (1:length(x))){
            if(x[i] %%2 == 0){
                ans[i] = FALSE}
            else{
                ans[i] = TRUE}}
}
return(ans)}

xs = c(rep(1,5),rep(2,5))
os = c(rep(TRUE,5),rep(FALSE,5))
tryCatch(checkEquals(os, odd(xs)),
         error=function(err) errMsg(err))

tryCatch(checkEquals(!os, odd(xs, flip=TRUE)),
         error=function(err) errMsg(err))


# (4 points) Implement the sum.adm function
#
# Takes the following arguments:
#   x: a numeric vector
#   na.rm: TRUE/FALSE 
#
# Returns the sum of the absolute deviations from
# the median for the input vector x.  If na.rm is
# TRUE, the function removes all the NAs from the
# computation of the return value.

sum.adm = function(x, na.rm=FALSE){
}

x = 1:3
tryCatch(checkEquals(2, sum.adm(x)),
         error=function(err) errMsg(err))

x = c(x,NA)
tryCatch(checkTrue(is.na(sum.adm(x))),
         error=function(err) errMsg(err))

tryCatch(checkEquals(2, sum.adm(x, na.rm=TRUE)),
         error=function(err) errMsg(err))


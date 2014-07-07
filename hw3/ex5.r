library(RUnit)
errMsg <- function(err) print(err)
load('ex5-tests.rda')

# Implement the function "firstLast". Your function should take the
# following arguments:
#
# <data>: any dataframe
#
# Your function should return the following:
#
# <first.last>: a dataframe with dimensions 10 x num.cols (where num.cols is
#   the number of columns from the data frame). The first five rows of this
#   matrix should be the first five rows of <data> while the last
#   five should be the final 5 rows of <data>

firstLast <- function(data) {
	first.last <- data[-c(6:(nrow(data)-5)),]
    return(first.last)
# your code here *
}

tryCatch(checkEquals(first.last.t, firstLast(iris)), error=function(err)
         errMsg(err))


# Implment the function "npRatio". Your function should take the following
# arguments:
#
# <data>: any matrix or dataframe
#
# Your function should return the following:
#
# <np.ratio>: the number of variables (columns) divided by the number of
# observations (rows)

npRatio <- function(data) {
	np.ratio <- ncol(data)/nrow(data)
	return(np.ratio)
    # your code here *
}

tryCatch(checkEquals(np.ratio.t, npRatio(iris)), error=function(err)
         errMsg(err))

# Implement the function "numericSummary". Your function should take the
# following arguments:
#
# <data>: a data frame containing numeric variables and factor levels
#
# Your function should return the following:
#
# <numeric.summary>: a 6 x n.numeric (where n.numeric is the number of
#   numeric variables) matrix containing the minimum, 1st Quartile, Median,
#   Mean, 3rd Quartile, and Max values for any numeric variable in <data>

numericSummary <- function(data) {
	nums <- sapply(data,is.numeric)
	only.nums <- data[,nums]
	numeric.summary <- matrix(,ncol = ncol(only.nums),nrow = 6)
	for (i in 1:ncol(only.nums)){
		numeric.summary[1,i] = min(only.nums[,i])
		numeric.summary[2,i] = quantile(only.nums[,i],.25)
		numeric.summary[3,i] = median(only.nums[,i])
		numeric.summary[4,i] = round(mean(only.nums[,i]),3)
		numeric.summary[5,i] = quantile(only.nums[,i],.75)
		numeric.summary[6,i] = max(only.nums[,i])}
	return(numeric.summary)
    # your code here **
}

tryCatch(checkEquals(numeric.summary.t, unname(numericSummary(ex5.test1))),
         error=function(err) errMsg(err))

# Implement the function "getClass". Your function should take the
# following arguments:
#
# <data>: a data frame
#
# Your function should return the following:
#
# <var.classes>: a character vector giving the class of each variable
#   (column) of the data frame

getClass <- function(data) {
	var.classes <- unlist(lapply(data,class))
	return(var.classes)
    # your code here *
}

tryCatch(checkEquals(get.class.t, unname(getClass(ex5.test1))), error=function(err)
         errMsg(err))

library(RUnit)
errMsg <- function(err) print(err)
load('ex3-tests.rda')

# Implement the function "sumNA". Your function should take the following
# arguments:
#
# <data.matrix>: a numeric matrix of any dimensions, possibly containing NA
#   values
#
# Your function should return the following:
#
# <numNAs>: a list containing 3 elements. The first should be a numeric
# vector giving the number of NA values in each row, the second a numeric
# vector giving the numeber of NA values in each column, and the third a
# numeric giving the total number of NA values in the matrix.

sumNA <- function(data.matrix) {
	element1 <- rowSums(is.na(data.matrix))
	element2 <- colSums(is.na(data.matrix))
	element3 <- sum(element2)
	numNAs <- list(element1,element2,element3)
	return(numNAs)
    # your code here **
}

tryCatch(checkEquals(sum.na.t, sumNA(ex3.test1)), error=function(err)
         errMsg(err)) 

    
# Implement the function "simulateNormals". Your function should take the
# following arguments:
#
# <n>: a numeric constant giving the number of normal variables drawn in
#   each simulation (constant across simulations)
# <sim.mean>: a numeric constant giving the mean of the normal variables
#   (constant across simulations)
# <sim.var>: a numeric constant giving the variance of the normal variables
#   (constant across simulations)
# <k> the total number of simulations
#
# Your function should return the following:
#
# <simulations>: a <n> x <k> matrix of simulated normal variables

simulateNormals <- function(n, sim.mean=0, sim.var=1, k=10) {
	nums <- rnorm(k*n,mean = sim.mean,sd = sqrt(sim.var))
	simulations <- matrix(nums,nrow = n,ncol = k)
	return(simulations)
    # your code here *
}

set.seed(47)
tryCatch(checkEquals(simulate.normals.t, simulateNormals(100, 5, 4, 5)),
         error=function(err) errMsg(err))


# Implement the function "listLengths". Your function should take the
# follwoing arguments:
#
# <data.list>: a list whose elements are vectors of varying length
#
# Your function should return the following:
#
# <element.lengths>: a numeric vector whose entries are the lengths of each
#   element of <data.list>

listLengths <- function(data.list) {
	element.lengths <- unlist(lapply(data.list,length))
	return(element.lengths)
    # your code here *
}

tryCatch(checkEquals(list.lengths.t, listLengths(ex3.test2)),
         error=function(err) errMsg(err))


# Implement the function "matrixListMeans". Your function should take the
# following arguments:
#
# <matrix.list>: a list of square matrices all with the same dimensions
#
# Your function should return:
#
# <matrix.row.means>: a nxk matrix (where n is the dimension of the
#   matrices and k is the length of the list). The jth column of this
#   matrix should correspond to the row means of the jth list element.

matrixListMeans <- function(matrix.list) {
	matrix.row.means <- sapply(matrix.list,rowMeans)
	return(matrix.row.means)
    # your code here *
}

tryCatch(checkEquals(matrix.list.means.t, matrixListMeans(ex3.test3)),
         error=function(err) errMsg(err))


# Implement the function "standMatrixVariables". Your function should take
# the folowing arguments:
#
# <data.matrix>: a numeric matrix whose columns correspond to variables
#
# Your function should return the following:
#
# <standardized.matrix>: an nxn matrix (where n is the number of variables
#   i.e. columns of <data.matrix). Entry (i,j) of this matrix should contain
#   the following value:
#
#      (mean(col.i) - mean(col.j)) / sd(col.i, col.j)
#
# where sd(col.i, col.j) is the standard deviation of all values from both
# column i and j.

standMatrixVariables <- function(data.matrix) {
	standardized.matrix <- matrix(,nrow = ncol(data.matrix),ncol = ncol(data.matrix))
	for (i in 1:ncol(standardized.matrix)){
		for(j in 1:nrow(standardized.matrix)){
			standardized.matrix[i,j]  = (mean(data.matrix[,i])-mean(data.matrix[,j]))/sd(c(data.matrix[,i],data.matrix[,j]))}}
	return(-standardized.matrix)
    # your code here ***
}

tryCatch(checkEquals(stand.matrix.variables.t,
                     standMatrixVariables(ex3.test4)),
         error=function(err) errMsg(err))

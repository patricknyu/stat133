###################################
## Plot simulated data
###################################

#Helper function to plot data in d1 vs d2 space
plotSim1 <- function(x,y,title = 'Cluster Analysis'){
	plot(x,y,pch = 10,col = 'blue', main = paste(title, 'variance'))
	text(x,y,labels=as.character(1:n$points),pos = 1, offset = .5)
}

#Helper function to do the same thing with labels
  
tSim = function(x,y,title = 'Cluster Analysis'){
	plot(x,y,pch = 19,col - col[grps], mian = paste(title,'variance'))
	text(x,y,labels = as.character(1:n$points),pos = 1,offset = .5)
}


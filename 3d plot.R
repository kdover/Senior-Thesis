#import libraries
library(kernlab)
library(devtools)
library(scatterplot3d)
library(seewave)
library(fitdistrplus)

#import data
testData <- read.csv("WIKI-AAPL.csv",stringsAsFactors=FALSE)

#reorder data
tempData =testData
#need to reverse the data
data = tempData[nrow(tempData),]
for(i in 2:nrow(tempData)){
	data = rbind(data,tempData[((nrow(tempData)+1)-i),])
}

#draw out time, price, volume, diff
time = c(1:nrow(data))
price = data[,2]
volume = data[,6]
diff = ((data[,5] - data[,2])/data[,5])*100

#plot the data with respect to time, price, volume
pdf("3dScatterPlot.pdf")
myPlot = scatterplot3d(time, price, volume,main = "3D scatterplot",xlab = "time", ylab = "price", zlab = "volume",pch=16,color="steelblue")
dev.off()


### do the different distribution calculations
plot(time,price)
#for every 10 days, calc the distribution
#distribution for the first price/time
left = price
right = volume
mean <- c()
stc <- c()
distances <- c()
myData <- data.frame(left, right)
last = fitdistcens(myData[1:50,],"norm")
mean = c(last$estimate[1])
std = c(last$estimate[2])
olddist <- rnorm(50,mean,std)
for(i in 2:176){
	#calc the distribution over this time, compare to the last
	start = i*50
	end = (i+1)*50
	new = fitdistcens(myData[start:end,],"norm")
	mean = c(mean,new$estimate[1])
	std = c(std,new$estimate[2])
	##now, lets calc the diff:
	newdist <- rnorm(50,new$estimate[1],new$estimate[2])
	distances = c(distances,kl.dist(olddist,newdist))
	#save the new one
	last = new
}
index <- c()
for(i in 1:176){
	index = c(index, 50*i)
}
#plot the means and standard devs for each step
pdf("meanPlot.pdf")
plot(index,mean,xlab="index",ylab="mean",main="Mean of the distribution at every 10 points")
dev.off()
pdf("stdPlot.pdf")
plot(index,std,xlab="index",ylab="standard deviation",main="Standard deviation of the distribution at every 10 points")
dev.off()














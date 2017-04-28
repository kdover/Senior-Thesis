library(kernlab)
library(devtools)
library(plot3D)
library(ggplot2)
library(plotly)
library(scatterplot3d)
library(spatial)
library(MASS)
library(lattice)
library(rsm)

appleData <- read.csv("apple.csv",stringsAsFactors=FALSE)
msftData <- read.csv("msft.csv",stringsAsFactors=FALSE)
spData <- read.csv("sp500.csv",stringsAsFactors=FALSE)
#visaData <- read.csv("visa.csv",stringsAsFactors=FALSE)
testData <- read.csv("WIKI-AAPL.csv",stringsAsFactors=FALSE)

tempData =testData
#need to reverse the data
data = tempData[nrow(tempData),]
for(i in 2:nrow(tempData)){
	data = rbind(data,tempData[((nrow(tempData)+1)-i),])
}


time = c(1:nrow(data))
price = data[,2]
volume = data[,6]
diff = ((data[,5] - data[,2])/data[,5])*100



tempData = cbind(price,time,volume)
colnames(tempData) = c("x", "y", "z")

pData = cbind(time,price,volume,diff)
start = 1
end = length(time)

#surf3D(x = time, y = volume, z = price, main="test")
pdf("3dScatterPlot.pdf")
myPlot = scatterplot3d(time, price, volume,main = "3D scatterplot",xlab = "time", ylab = "price", zlab = "volume",pch=16,color="steelblue")
dev.off()

#fit a surface to the data
tempData.kr <- surf.ls(3,time[start:end],price[start:end],volume[start:end])
trsurf <- trmat(tempData.kr,20,70,0,1400,1400)
png(filename="fittedSurface.png")
persp3D(trsurf$x,trsurf$y,trsurf$z,theta=50,phi=20,axes=TRUE,nticks=5,ticktype="detailed",xlab="Price",ylab="Time",zlab="Volume",main="3D fit") 
dev.off()

#do the 3d gaussian process
variance = 0.001
#do the particle modeling
pTest <- predict(gausspr(time[start:end],price[start:end],type="regression",var= variance))
plot(pTest)
points(time,price)

#vTest <- predict(lm(volume ~ time))
vTest <- predict(gausspr(time[start:end],volume[start:end],type="regression",var= 4))
plot(vTest)
points(time,volume)


#dTest <- predict(lm(diff ~ time))
dTest <- predict(gausspr(time[start:end],diff[start:end],type="regression",var= 4))
plot(dTest)
points(time,diff)


plotPData = cbind(pTest,vTest,dTest)
pdf("parametricCurve.pdf")
scatterplot3d(plotPData,type="l",lwd=3,xlab="Price",ylab = "volume", zlab="difference")
dev.off()


### do the different distribution calculations
library(seewave)
library(fitdistrplus)
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
pdf("meanPlot.pdf")
plot(index,mean,xlab="index",ylab="mean",main="Mean of the distribution at every 10 points")
dev.off()
pdf("stdPlot.pdf")
plot(index,std,xlab="index",ylab="standard deviation",main="Standard deviation of the distribution at every 10 points")
dev.off()














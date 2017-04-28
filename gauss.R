library(kernlab)
library(devtools)
#get the data


appleData <- read.csv("apple.csv",stringsAsFactors=FALSE)
msftData <- read.csv("msft.csv",stringsAsFactors=FALSE)
spData <- read.csv("sp500.csv",stringsAsFactors=FALSE)
#visaData <- read.csv("visa.csv",stringsAsFactors=FALSE)
testData <- read.csv("WIKI-AAPL.csv",stringsAsFactors=FALSE)

#stockData = testData
time <- c(1:nrow(appleData))

# #predict on the training set
variance <- 4
test <- gausspr(time,appleData[,3],type="regression",var= variance)
ytest <- predict(test,time)

#need to reverse the data
data = testData[nrow(testData),]
for(i in 2:nrow(testData)){
	data = rbind(data,testData[((nrow(testData)+1)-i),])
}

predictHS <- function(data,step,variance,error){
	temp = data[,5]
	endPoint = length(temp)
	i = 1
	while((endPoint-i*step) > step){
		#get the data we want and make our file name
		start = (i-1)*step + 1
		end = i*step
		stockData = temp[start:end]
		time = c(1:step)
		name = paste(c("apple",i*step,".png"),collapse="")
		#make the gaussian process
		test <- gausspr(time,stockData,type="regression",var= variance)
		ytest <- predict(test,time)
		#find the w's
		print("step")
		print(start)
		drawHS(stockData,time,ytest,error,name)
		i = i+1		
	}
	#get the data we want and make our file name
	start = (i-1)*step + 1
	end = endPoint
	stockData = temp[start:end]
	time = c(1:(1+(endPoint-start)))
	name = paste(c("apple",i*step,".png"),collapse="")
	#make the gaussian process
	test <- gausspr(time,stockData,type="regression",var= variance)
	ytest <- predict(test,time)
	#find the w's
	print("step")
	print(start)
	drawHS(stockData,time,ytest,error,name)
}

drawW<- function(data,t,fit,eps){
	#do the initial plot
	plot(t,data[,3],xlab="time",ylab="price",type="l")
	lines(t,fit,col="red",lwd=5)
	##make a list of all the relative max/min and order them
	maxs <- findMax(fit[,1])
	mins <- findMin(fit[,1])
	allMaxMin <- c(maxs,mins)
	allMaxMin <- sort(allMaxMin)
	for(i in 1:length(allMaxMin)){
		points(allMaxMin[i],fit[allMaxMin[i],1],col="blue",lwd=5)
	}
	#now that we have them ordered, try to find "w" sequences, which should be the following:
	#max/min/max/min/max
	#so take 5 at a time if possible, will assume the list is longer than 5 	
	#points, so make the end points length-4 since the index starts at 1
	end = length(allMaxMin) - 4
	for(i in 1:end){
		tempList <- allMaxMin[i:(i+4)]
		if(isPossibleW(tempList,maxs)==TRUE){
			check <- isRealW(tempList,fit,eps)
			if(check[1]==1){
				ourW <- c()
				for(i in 2:length(check)){
					if(i%%2 != 0){
						ourW = c(ourW,check[i])	
					}
				}
				lines(tempList,ourW,col="green",lwd=5)
				print(tempList)
				print(ourW)
			}
		
		}
	}
}
drawM<- function(data,t,fit,eps){
	#do the initial plot
	plot(t,data[,3],type="l")
	lines(t,fit,col="red",lwd=5)
	##make a list of all the relative max/min and order them
	maxs <- findMax(fit[,1])
	mins <- findMin(fit[,1])
	allMaxMin <- c(maxs,mins)
	allMaxMin <- sort(allMaxMin)
	for(i in 1:length(allMaxMin)){
		points(allMaxMin[i],fit[allMaxMin[i],1],col="blue",lwd=5)
	}
	#now that we have them ordered, try to find "w" sequences, which should be the following:
	#max/min/max/min/max
	#so take 5 at a time if possible, will assume the list is longer than 5 	
	#points, so make the end points length-4 since the index starts at 1
	end = length(allMaxMin) - 4
	for(i in 1:end){
		tempList <- allMaxMin[i:(i+4)]
		if(isPossibleM(tempList,maxs)==TRUE){
			check =isRealM(tempList,fit,eps) 
			if(check[1]==1){
				ourM <- c()
				for(i in 2:length(check)){
					if(i%%2 != 0){
						ourM = c(ourM,check[i])
					}
				}
				lines(tempList,ourM,col="green",lwd=5)
			}
		
		}
	}
}

drawHS<- function(data,t,fit,eps,name){
	#do the initial plot
	png(filename=name)
	plot(t,data,type="l")
	lines(t,fit,col="red",lwd=5)
	##make a list of all the relative max/min and order them
	maxs <- findMax(fit[,1])
	mins <- findMin(fit[,1])
	allMaxMin <- c(maxs,mins)
	allMaxMin <- sort(allMaxMin)
	for(i in 1:length(allMaxMin)){
		points(allMaxMin[i],fit[allMaxMin[i],1],col="blue",lwd=5)
	}
	#now that we have them ordered, try to find head and shoulder sequences, which should be the following:
	#min/max/min/max/min/max/min
	#so take 7 at a time if possible, will assume the list is longer than 7 	
	#points, so make the end points length-6since the index starts at 1
	end = length(allMaxMin) - 6
	for(i in 1:end){
		tempList <- allMaxMin[i:(i+6)]
		if(isPossibleHS(tempList,maxs)==TRUE){
			check <- isRealHS(tempList,fit,eps)
			if(check[1]==1){
				ourHS <- c()
				for(i in 2:length(check)){
					if(i%%2 != 0){
						ourHS = c(ourHS,check[i])
					}
				}
				lines(tempList,ourHS,col="green",lwd=5)
				print(tempList)
				print(ourHS)
			}
		
		}
	}
	dev.off()
}

#want to make a function that checks if something has the potential for a correct W sequence
isPossibleW <- function(proposedShape,maxs){
	#to be the right sequence, needs to be 10101
	correct <- c(1,0,1,0,1)
	temp = c()
	for(i in 1:length(proposedShape)){
		temp = c(temp,isMax(proposedShape[i],maxs))
	}
	return(identical(correct,temp))
}
#want to make a function that checks if something has the potential for a correct M sequence
isPossibleM <- function(proposedShape,maxs){
	#to be the right sequence, needs to be 01010
	correct <- c(0,1,0,1,0)
	temp = c()
	for(i in 1:length(proposedShape)){
		temp = c(temp,isMax(proposedShape[i],maxs))
	}
	return(identical(correct,temp))
}
#want to make a function that checks if something has the potential for a correct head and shoulder sequence
isPossibleHS <- function(proposedShape,maxs){
	#to be the right sequence, needs to be 0101010
	correct <- c(0,1,0,1,0,1,0)
	temp = c()
	for(i in 1:length(proposedShape)){
		temp = c(temp,isMax(proposedShape[i],maxs))
	}
	return(identical(correct,temp))
}
isRealW <- function(points,fit,eps){
	max = c(points[1],points[3],points[5])
	min = c(points[2],points[4])
	#define important points
	q0 = c(max[1],fit[max[1],1])
	q1 = c(max[2],fit[max[2],1])
	q2 = c(max[3],fit[max[3],1])
	#make heights of qo and q2 the same
	if(height(q0) > height(q2)){
		q0[2] = q2[2]
	}
	if(height(q2) > height(q0)){
		q2[2] = q0[2]
	}
	p1 = c(min[1],fit[min[1],1])
	p2 = c(min[2],fit[min[2],1])
	o = c((p2[1]+p1[1])/2,(p2[2]+p1[2])/2)
	#now make them into vectors:
	P1P2 = p2 - p1
	Q0Q2 = q2 - q0
	Q1O = q1 - o
	#now find out if they are perp/
	perp = almostPerp(Q1O,P1P2,eps)
	parr = almostParr(P1P2,Q0Q2,eps)
	#return the result!
	z=0
	if(perp == 1 && parr == 1 && height(q1) < height(q0)){
		z=1
	}
	return(c(z,q0,p1,q1,p2,q2))
}
isRealM <- function(points,fit,eps){
	max = c(points[2],points[4])
	min = c(points[1],points[3],points[5])
	#get the points
	q0 = c(max[1],fit[max[1],1])
	q1 = c(max[2],fit[max[2],1])
	p1 = c(min[1],fit[min[1],1])
	p2 = c(min[2],fit[min[2],1])
	p3 = c(min[3],fit[min[3],1])
	#make the end bottom points the same height
	if(height(p1) < height(p3)){
		p1[2] = p3[2]
	}
	if(height(p3) < height(p1)){
		p3[2] = p1[2]
	}
	o = c((q0[1]+q1[1])/2, (q0[2]+q1[2])/2)
	#now make them into vectors:
	Q0Q1 = q1 - q0
	P1P3 = p3 - p1
	OP2 = p2 - o
	#now find out if they are per/parr
	perp = almostPerp(Q0Q1,OP2,eps)
	parr = almostParr(Q0Q1,P1P3,eps)
	#return the result!
	z=0
	if(perp == 1 && parr ==1 && height(p2) > height(p1)){
		z = 1
	}
	return(c(z,p1,q0,p2,q1,p3))
}
isRealHS <- function(points,fit,eps){
	max = c(points[2],points[4],points[6])
	min = c(points[1],points[3],points[5],points[7])
	#find the min for q0 and q2:
	#get the points
	q0 = c(max[1],fit[max[1],1])
	q1 = c(max[2],fit[max[2],1])
	q2 = c(max[3],fit[max[3],1])
	p1 = c(min[1],fit[min[1],1])
	p2 = c(min[2],fit[min[2],1])
	p3 = c(min[3],fit[min[3],1])
	p4 = c(min[4],fit[min[4],1])
	# #make the p1=p4
	# if(height(p1) > height(p4)){
		# #p4[2] = p1[2]
		# for(i in max[3]:min[4]){
			# if(abs(fit[i,1] - p1[2]) < 0.1){
				# p4[2] = fit[i,1]
				# p4[1] = i
				# break
			# }
		# }
	# }
	# if(height(p4) > height(p1)){
		# #p1[2] = p4[2]
		# for(i in min[1]:max[1]){
			# if(abs(fit[i,1] - p4[2]) < 0.1){
				# p1[2] = fit[i,1]
				# p1[1] = i
				# break
			# }
		# }
	# }
	o = c((p2[1]+p3[1])/2,(p2[2]+p3[2])/2)
	#make a new list of points to return later
	myPoints <- c(p1,q0,p2,q1,p3,q2,p4)
	#now make them into vectors:
	P1P4 = p4 - p1
	P2P3 = p3 - p2
	Q0Q2 = q2 - q0
	OQ1 = q1 - o
	#now find out if they are perp/
	perp = almostPerp(OQ1,P2P3,eps)
	parr1 = almostParr(P1P4,P2P3,eps)
	parr2 = almostParr(P1P4,Q0Q2,eps)
	#return the result!
	z=0
	if(perp == 1 && parr1 == 1 && parr2 == 1 && height(q1) > height(q2) && height(q1) > height(q0) && height(p1)< height(q0) && height(p3) < height(q2)){
		z=1
	}
	return(c(z,myPoints))	
}
#now check if the the vectors made are correct
#find the data entry
isMax <- function(value,maxs){
	for(i in 1:length(maxs)){
		if(maxs[i] == value){
			return(1)
		}
	}
	return(0)
}
linearCombo <- function(v,z,w){
	b = w[2] - ((w[1]*v[2])/v[1] )/(z[2]-(z[1]*v[2])/v[1])
	a = (w[1]/v[1]) - b*(z[1]/v[1])
	return(c(a,b))
}
almostParr <- function(u,w,eps){
	c = innerProd(u,w)/(norm(as.matrix(u),"f")*norm(as.matrix(w),"f"))
	theta = acos(c)
	z = 0
	if(abs(theta)<eps){
		z = 1
	}
	return(z)
}

innerProd <- function(u,w){
	z = u %*% w
	return(z)
}
almostPerp <- function(u,w,eps){
	c = innerProd(u,w)/(norm(as.matrix(u),"f")*norm(as.matrix(w),"f"))
	theta = acos(c)
	z = 0
	if(abs(theta-(pi/2))<eps){
		z = 1
	}
	return(z)
}
#find local mins and maxs
findMax <- function(x,thresh=0){
	pks <- which(diff(sign(diff(x,na.pad=FALSE)),na.pad=FALSE) < 0) + 1
	if(!missing(thresh)){
		pks[x[pks-1]-x[pks] > thresh]
	}
	else pks
}

findMin <- function(x,thresh=0){
	pks <- which(diff(sign(diff(x,na.pad=FALSE)),na.pad=FALSE) > 0) + 1
	if(!missing(thresh)){
		pks[x[pks-1]-x[pks] > thresh]
	}
	else pks	
}
height <- function(v){
	return(v[2])
}
##find the right slopes and lengths
# x = c(532, 573, 617, 685, 753, 778, 866)
# y = c(38.93814, 42.00841, 36.43340, 46.92232, 36.84181, 37.83827, 25.41525)
# slopes = c()
# lengths = c()
# for(i in 1:(length(x)-1)){
	# top = y[i+1] - y[i]
	# bottom = x[i+1] - x[i]
	# first = top*top
	# second = bottom*bottom
	# slopes = c(slopes,top/bottom)
	# lengths = c(lengths,sqrt(first+second))
# }
variance <- 0.001
test <- gausspr(time,appleData[,3],type="regression",var= variance)
ytest <- predict(test,time)
pdf("dataProgression001.pdf")
par(mfrow=c(1,2))
maxs <- findMax(ytest[,1])
mins <- findMin(ytest[,1])
allMaxMin <- c(maxs,mins)
allMaxMin <- sort(allMaxMin)
plot(time,appleData[,3],xlab="Time",ylab="Price",type="l")
lines(time,ytest,col="red",lwd=5)
for(i in 1:length(allMaxMin)){
	points(allMaxMin[i],ytest[allMaxMin[i],1],col="blue",lwd=5)
}
end = length(allMaxMin) - 4
for(i in 1:end){
	tempList <- allMaxMin[i:(i+4)]
	if(isPossibleW(tempList,maxs)==TRUE){
		check <- isRealW(tempList,ytest,1)
		if(check[1]==1){
			ourW <- c()
			for(i in 2:length(check)){
				if(i%%2 != 0){
					ourW = c(ourW,check[i])	
				}
			}
			lines(tempList,ourW,col="green",lwd=5)
			print(tempList)
			print(ourW)
		}
		
	}
}
test <- gausspr(time,appleData[,3],type="regression",var= variance)
ytest <- predict(test,time)
maxs <- findMax(ytest[,1])
mins <- findMin(ytest[,1])
allMaxMin <- c(maxs,mins)
allMaxMin <- sort(allMaxMin)
plot(time,appleData[,3],xlab="Time",ylab="Price",type="l")
lines(time,ytest,col="red",lwd=5)
for(i in 1:length(allMaxMin)){
	points(allMaxMin[i],ytest[allMaxMin[i],1],col="blue",lwd=5)
}
end = length(allMaxMin) - 4
for(i in 1:end){
	tempList <- allMaxMin[i:(i+4)]
	if(isPossibleW(tempList,maxs)==TRUE){
		check <- isRealW(tempList,ytest,1)
		if(check[1]==1){
			ourW <- c()
			for(i in 2:length(check)){
				if(i%%2 != 0){
					ourW = c(ourW,check[i])	
				}
			}
			lines(tempList,ourW,col="green",lwd=5)
			print(tempList)
			print(ourW)
		}
		
	}
}
dev.off()
############################################
pdf("dataProgression4.pdf")
par(mfrow=c(2,2))
#first plot
plot(time,appleData[,3],xlab="Time",ylab="Price",type="l")
#second plot
plot(time,appleData[,3],xlab="Time",ylab="Price",type="l")
lines(time,ytest,col="red",lwd=5)
#third plot
maxs <- findMax(ytest[,1])
mins <- findMin(ytest[,1])
allMaxMin <- c(maxs,mins)
allMaxMin <- sort(allMaxMin)
plot(time,appleData[,3],xlab="Time",ylab="Price",type="l")
lines(time,ytest,col="red",lwd=5)
for(i in 1:length(allMaxMin)){
	points(allMaxMin[i],ytest[allMaxMin[i],1],col="blue",lwd=5)
}
	#points, so make the end points length-4 since the index starts at 1
#make the 4th plot
plot(time,appleData[,3],xlab="Time",ylab="Price",type="l")
lines(time,ytest,col="red",lwd=5)
for(i in 1:length(allMaxMin)){
	points(allMaxMin[i],ytest[allMaxMin[i],1],col="blue",lwd=5)
}
end = length(allMaxMin) - 4
for(i in 1:end){
	tempList <- allMaxMin[i:(i+4)]
	if(isPossibleW(tempList,maxs)==TRUE){
		check <- isRealW(tempList,ytest,1)
		if(check[1]==1){
			ourW <- c()
			for(i in 2:length(check)){
				if(i%%2 != 0){
					ourW = c(ourW,check[i])	
				}
			}
			lines(tempList,ourW,col="green",lwd=5)
			print(tempList)
			print(ourW)
		}
		
	}
}
dev.off()


##################
variance <- 4
test <- gausspr(time,appleData[,3],type="regression",var=variance)
ytest <- predict(test,time)
pdf("error1.pdf")
drawW(appleData,time,ytest,1)
dev.off()
library(kernlab)
library(devtools)

#calibration for comparing slopes/lengths
calibration = 100

#get the data
disney <- read.csv("disney.csv",stringsAsFactors=FALSE)
msft <- read.csv("msft.csv",stringsAsFactors=FALSE)
nike <- read.csv("nike.csv",stringsAsFactors=FALSE)
walmart <- read.csv("walmart.csv",stringsAsFactors=FALSE)
apple <- read.csv("WIKI-AAPL.csv",stringsAsFactors=FALSE)
nasdaq <- read.csv("nasdaq.csv",stringsAsFactors=FALSE)
dow <- read.csv("sp500.csv",stringsAsFactors=FALSE)

reverseData <- function(data){
	newData = data[nrow(data),]
	for(i in 2:nrow(data)){
		newData = rbind(newData,data[((nrow(data)+1)-i),])
	}
	return(newData)
}

#need to reverse the data
disneyData = reverseData(disney)
msftData = reverseData(msft)
nikeData = reverseData(nike)
walmartData = reverseData(walmart)
appleData = reverseData(apple)
nasdaqData = reverseData(nasdaq)
dowData = reverseData(dow)




disneyPQ = predictM(disneyData,500,0.001,1)
msftPQ = predictM(msftData,500,0.001,1)
nikePQ = predictM(nikeData,500,0.001,1)
walmartPQ = predictM(walmartData,500,0.001,1)
applePQ = predictM(appleData,500,0.001,1)
nasdaqPQ = predictM(nasdaqData,500,0.001,1)

kSlopeReturn <- function(returns){
	return(returns[1:(length(returns)/4)])
}
endSlopeReturn <- function(returns){
	return(returns[(length(returns)/4 + 1):(length(returns)/2)])
}
kLengthReturn <- function(returns){
	return(returns[(length(returns)/2 + 1):(3*length(returns)/4)])
}
endLengthReturn <- function(returns){
	return(returns[(3*length(returns)/4+1):(length(returns))])
}

allKSlopes = c(kSlopeReturn(disneyK),kSlopeReturn(msftK),kSlopeReturn(walmartK),kSlopeReturn(appleK),kSlopeReturn(nasdaqK))
allKLengths = c(kLengthReturn(disneyK),kLengthReturn(msftK),kLengthReturn(walmartK),kLengthReturn(appleK),kLengthReturn(nasdaqK))
allEndSlopes = c(endSlopeReturn(disneyK),endSlopeReturn(msftK),endSlopeReturn(walmartK),endSlopeReturn(appleK),endSlopeReturn(nasdaqK))
allEndLengths = c(endLengthReturn(disneyK),endLengthReturn(msftK),endLengthReturn(walmartK),endLengthReturn(appleK),endLengthReturn(nasdaqK))

#go through and remove NA
for(i in 1:length(allKSlopes)){
	if(is.na(allKSlopes[i])){
		allKSlopes[i] = 0
	}
	if(is.na(allKLengths[i])){
		allKLengths[i] = 0
	}
	if(is.na(allEndSlopes[i])){
		allEndSlopes[i] = 0
	}
	if(is.na(allEndLengths[i])){
		allEndLengths[i] = 0
	}
}

predS <- avg(allKSlopes,allEndSlopes)
predL <- avg(allKLengths,allEndLengths)

#now predict for the next dataset
dowK = predictM(dowData,500,0.001,1)
dowKSlopes = kSlopeReturn(dowK)
dowEndSlopes = endSlopeReturn(dowK)
dowKLengths = kLengthReturn(dowK)
dowEndLengths = endLengthReturn(dowK)

#go through and remove NA
for(i in 1:length(dowKSlopes)){
	if(is.na(dowKSlopes[i])){
		dowKSlopes[i] = 0
	}
	if(is.na(dowEndSlopes[i])){
		dowEndSlopes[i] = 0
	}
	if(is.na(dowKLengths[i])){
		dowKLengths[i] = 0
	}
	if(is.na(dowEndLengths[i])){
		dowEndLengths[i] = 0
	}
}

#do the expected:
expectedSlopes <- c()
expectedLengths <- c()

for(i in 1:length(dowKSlopes)){
	expectedSlopes = c(expectedSlopes,(predS/dowKSlopes[i]))
	expectedLengths = c(expectedLengths,(predL/dowKLengths[i]))
}
min(abs(expectedSlopes - dowEndSlopes))
max(abs(expectedSlopes - dowEndSlopes))
min(abs(expectedLengths - dowEndLengths))
max(abs(expectedLengths - dowEndLengths))
predSdow <- avg(dowKSlopes,dowEndSlopes)
predLdow <- avg(dowKLengths,dowEndLengths)



#my W shape comparison
Mslope <- c(0.9973514, -1.3009444, 1.1545217, -0.8897556)
Mlength <- c(52.25665, 29.53567, 35.12997, 60.23389)

predictM <- function(data,step,variance,error){
	temp = data[,5]
	endPoint = length(temp)
	i = 1
	kSlopes = c()
	endSlopes = c()
	kLengths = c()
	endLengths = c()
	while((endPoint-i*step) > step){
		#get the data we want and make our file name
		start = (i-1)*step + 1
		end = i*step
		stockData = temp[start:end]
		time = c(1:step)
		name = paste(c("apple",i*step,".pdf"),collapse="")
		#make the gaussian process
		test <- gausspr(time,stockData,type="regression",var= variance)
		ytest <- predict(test,time)
		#find the w's
		results = drawM(stockData,time,ytest,error,name)
		if(length(results) > 0){
			endingPoints = length(results)/4
			kSlopes = c(kSlopes,results[1:endingPoints])
			endSlopes = c(endSlopes,results[(endingPoints+1):(endingPoints*2)])
			kLengths = c(kLengths, results[(endingPoints*2 + 1):(endingPoints*3)])
			endLengths = c(endLengths,results[(endingPoints*3 + 1):length(results)])
		}
		i = i+1		
	}
	#get the data we want and make our file name
	start = (i-1)*step + 1
	end = endPoint
	stockData = temp[start:end]
	time = c(1:(1+(endPoint-start)))
	name = paste(c("apple",i*step,".pdf"),collapse="")
	#make the gaussian process
	test <- gausspr(time,stockData,type="regression",var= variance)
	ytest <- predict(test,time)
	#find the w's
	results = drawM(stockData,time,ytest,error,name)
	if(length(results) > 0){
		endingPoints = length(results)/4
		kSlopes = c(kSlopes,results[1:endingPoints])
		endSlopes = c(endSlopes,results[(endingPoints+1):(endingPoints*2)])
		kLengths = c(kLengths, results[(endingPoints*2 + 1):(endingPoints*3)])
		endLengths = c(endLengths,results[(endingPoints*3 + 1):length(results)])
	}
	print("overall prediction")
	predS <- avg(kSlopes,endSlopes)
	predL <- avg(kLengths,endLengths)
	print(c(predS,predL))
	return(c(kSlopes,endSlopes,kLengths,endLengths))
}
predictM2 <- function(data,step,variance,error){
	temp = data[,5]
	endPoint = length(temp)
	i = 1
	qPoints <- c()
	pPoints <- c()
	while((endPoint-i*step) > step){
		#get the data we want and make our file name
		start = (i-1)*step + 1
		end = i*step
		stockData = temp[start:end]
		time = c(1:step)
		name = paste(c("apple",i*step,".pdf"),collapse="")
		#make the gaussian process
		test <- gausspr(time,stockData,type="regression",var= variance)
		ytest <- predict(test,time)
		#find the w's
		results = drawM2(stockData,time,ytest,error,name)
		if(length(results) > 0){
			qPoints = c(qPoints,results[1:(length(results)/2)])
			pPoints = c(pPoints,results[(length(results)/2 + 1):length(results)])
		}
		i = i+1		
	}
	#get the data we want and make our file name
	start = (i-1)*step + 1
	end = endPoint
	stockData = temp[start:end]
	time = c(1:(1+(endPoint-start)))
	name = paste(c("apple",i*step,".pdf"),collapse="")
	#make the gaussian process
	test <- gausspr(time,stockData,type="regression",var= variance)
	ytest <- predict(test,time)
	#find the w's
	results = drawM2(stockData,time,ytest,error,name)
	if(length(results) > 0){
		qPoints = c(qPoints,results[1:(length(results)/2)])
		pPoints = c(pPoints,results[(length(results)/2 + 1):length(results)])
	}
	print("all points")
	print(qPoints)
	print(pPoints)
	return(c(qPoints,pPoints))
}

drawM<- function(data,t,fit,eps,name){
	#do the initial plot
	pdf(name)
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
	#now that we have them ordered, try to find "w" sequences, which should be the following:
	#min/max/min/max/min
	#so take 5 at a time if possible, will assume the list is longer than 5 	
	#points, so make the end points length-4 since the index starts at 1
	end = length(allMaxMin) - 4
	myShapes <- c()
	kSlopes <- c()
	kLengths <- c()
	endLengths <- c()
	endSlopes <- c()
	for(i in 1:end){
		tempList <- allMaxMin[i:(i+4)]
		if(isPossibleM(tempList,maxs)==TRUE){
			check =isRealM(tempList,fit,eps) 
			if(check[1]==1){
				#start is i
				possibleSL <- checkShape(i,allMaxMin,fit)
				if(length(possibleSL) != 1){
					ourM <- c()
					for(i in 2:length(check)){
						if(i%%2 != 0){
							ourM = c(ourM,check[i])
						}
					}
					lines(tempList,ourM,col="cyan",lwd=5)
					myShapes = c(myShapes,possibleSL)
					#print(myShapes)
					if(i != end){
						#obtain the next segment, save it, use it for avg
						nextL <- findLength(i,allMaxMin,fit)
						endLengths = c(endLengths,nextL)
						nextS <- findSlope(i,allMaxMin,fit)
						endSlopes = c(endSlopes,nextS)
						ks <- miniShape(Mslope,myShapes[1:4])
						kSlopes = c(kSlopes,ks)
						kl <- miniShape(Mlength,myShapes[5:8])
						kLengths = c(kLengths,kl)
						#print(c(nextS,nextL))
					}
				}
			}
		}
	}
	#do the predictions:
	print("step predictions")
	predS <- avg(kSlopes,endSlopes)
	predL <- avg(kLengths,endLengths)
	print(c(predS,predL))
	dev.off()
	return(c(kSlopes,endSlopes,kLengths,endLengths))
}

drawM2<- function(data,t,fit,eps,name){
	#do the initial plot
	pdf(name)
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
	#now that we have them ordered, try to find "w" sequences, which should be the following:
	#min/max/min/max/min
	#so take 5 at a time if possible, will assume the list is longer than 5 	
	#points, so make the end points length-4 since the index starts at 1
	end = length(allMaxMin) - 4
	myShapes <- c()
	kSlopes <- c()
	kLengths <- c()
	endLengths <- c()
	endSlopes <- c()
	qPoints <- c()
	pPoints <- c()
	for(i in 1:end){
		tempList <- allMaxMin[i:(i+4)]
		if(isPossibleM(tempList,maxs)==TRUE){
			check =isRealM(tempList,fit,eps) 
			if(check[1]==1){
				#start is i
				possibleSL <- checkShape(i,allMaxMin,fit)
				if(length(possibleSL) != 1){
					ourM <- c()
					for(i in 2:length(check)){
						if(i%%2 != 0){
							ourM = c(ourM,check[i])
						}
					}
					lines(tempList,ourM,col="cyan",lwd=5)
					myShapes = c(myShapes,possibleSL)
					#print(myShapes)
					if(i != end){
						#obtain the next segment, save it, use it for avg
						qPoints = c(qPoints,c(allMaxMin[i],fit[allMaxMin[i],1]))
						pPoints = c(pPoints,c(allMaxMin[i+1],fit[allMaxMin[i+1],1]))
						#print(c(nextS,nextL))
					}
				}
			}
		}
	}
	#do the predictions:
	print("...work...")
	print(qPoints)
	print(pPoints)
	dev.off()
	return(c(qPoints,pPoints))
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

checkShape <- function(start,allMaxMin,fit){
	#now find the slopes and lengths for first 5 points
	slopes <- c()
	lengths <- c()
	for(i in start:(start+3)){
		slopes = c(slopes,findSlope(i,allMaxMin,fit))
		lengths = c(lengths,findLength(i,allMaxMin,fit))
	}	
	#compare to our learned shape:
	tempS = Mslope - slopes
	tempL = Mlength - lengths
	if(abs(magnitudeOfVec(tempS)) < calibration){
		# xData <- c()
		# yData <- c()
		# for(i in start:(start+4)){
			# xData = c(xData,allMaxMin[i]) 
			# yData = c(yData,fit[allMaxMin[i],1])
		# }
		# lines(xData,yData,col="cyan",lwd=5)
		return(c(slopes,lengths))
	}
	return(c(0))
}

compareShape <- function(length){
	k <- miniShape(Mlength,length)
	return(k)
}

avg <- function(k,list){
	return(sum(k*list)/length(k))
}

miniShape <- function(length1,length2){
	f <- function (x,a) sum(abs(length1 - x*a))
	xmin <- optimize(f,c(0,1),tol = 0.0001,a=length2)
	return(xmin$minimum)
}

findSlope <- function(index,list,fit){
	top = fit[list[index+1],1] - fit[list[index],1]
	bottom = list[index+1] - list[index]
	return(c(top/bottom))
}
findLength <- function(index,list,fit){
	first = (list[index+1] - list[index])^2
	second = (fit[list[index+1],1] - fit[list[index],1])^2
	return(c(sqrt(first+second)))
}
magnitudeOfVec <- function(x){
	sum = 0
	for(i in 1:length(x)){
		sum = sum + x[i]^2
	}
	return(sqrt(sum))
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
# disneyPQ = predictM2(disneyData,500,0.001,1)
# msftPQ = predictM2(msftData,500,0.001,1)
# nikePQ = predictM2(nikeData,500,0.001,1)
# walmartPQ = predictM2(walmartData,500,0.001,1)
# applePQ = predictM2(appleData,500,0.001,1)
# nasdaqPQ = predictM2(nasdaqData,500,0.001,1)

# getQPoints <- function(points){
	# return(points[1:(length(points)/2)])
# }
# getPPoints <- function(points){
	# return(points[(length(points)/2 + 1):(length(points))])
# }

# qPoints <- c(getQPoints(disneyPQ),getQPoints(msftPQ),getQPoints(nikePQ),getQPoints(walmartPQ),getQPoints(applePQ),getQPoints(nasdaqPQ))
# pPoints <- c(getPPoints(disneyPQ),getPPoints(msftPQ),getPPoints(nikePQ),getPPoints(walmartPQ),getPPoints(applePQ),getPPoints(nasdaqPQ))

# #go through and remove NA
# allQPoints <- c()
# allPPoints <- c()
# for(i in 1:length(qPoints)){
	# if(!(is.na(pPoints[i]))){
		# allPPoints = c(allPPoints,pPoints[i])
		# allQPoints = c(allQPoints,qPoints[i])
	# }
# }

# #normalize vectors and add them all together:
# diffPQ = allPPoints - allQPoints
# normed <- c()
# for(i in 1:(length(diffPQ)/2)){
	# second = diffPQ[(2*i)]^2
	# first = diffPQ[(2*i -1)]^2
	# factor = 1/sqrt(first + second)
	# normed = c(normed,c(factor*diffPQ[(2*i -1)], factor*diffPQ[(2*i)]))
# }

# xCoor = 0
# yCoor = 0
# for(i in 1:(length(normed)/2)){
	# xCoor = xCoor + normed[(2*i -1)]
	# yCoor = yCoor + normed[(2*i)]
# }

# predPQ = c((1/(length(normed)/2))*xCoor, (1/(length(normed)/2))*yCoor) 

# #now compare to data?
# dowK = predictM2(dowData,500,0.001,1)
# dowQPoints = getQPoints(dowK)
# dowPPoints = getPPoints(dowK)

# #go through and remove NA
# newQPoints <- c()
# newPPoints <- c()
# for(i in 1:length(dowPPoints)){
	# if(!(is.na(dowPPoints[i]))){
		# newQPoints = c(newQPoints,dowQPoints[i])
		# newPPoints = c(newPPoints,dowPPoints[i])
	# }
# }

# #normalize vectors
# dowDiffPQ = newPPoints - newQPoints
# normed <- c()
# for(i in 1:(length(dowDiffPQ)/2)){
	# second = dowDiffPQ[(2*i)]^2
	# first = dowDiffPQ[(2*i -1)]^2
	# factor = 1/sqrt(first + second)
	# normed = c(normed,c(factor*dowDiffPQ[(2*i -1)], factor*dowDiffPQ[(2*i)]))
# }

# #now compare each to the prediction: find angle between each i and the predPQ

# angles <- c()
# for(i in 1:(length(normed)/2)){
	# experimental = c(normed[(2*i - 1)],normed[(2*i)])
	# c = innerProd(predPQ,experimental)/(norm(as.matrix(predPQ),"f")*norm(as.matrix(experimental),"f"))
	# theta = acos(c)
	# angles = c(angles,theta)
# }
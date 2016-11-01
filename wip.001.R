library(data.table)
library(ggplot2)
inFileName = list()
inFileName[[1]] = "C:/user/MYDOCS/MY TSX Projects/2016/Measure the Culture/Champion Survey/CSV/SurveyData.csv"
inData = fread(inFileName[[1]], header = TRUE, select=c("Survey_id","Q1","Q2","Q3","Q4","Q5","Q6","Q7"))
inTime = fread(inFileName[[1]], header = TRUE, select=c("Timestamp"))
inDay = inTime
inDay$Day = inTime$Timestamp
inDay$Timestamp = NULL
for (i in 1:nrow(inTime))
{
  z = unclass(as.POSIXct(inTime$Timestamp[i]))[1]
  inDay[i,1] = as.numeric(floor(unclass(z)/86400))
}
inDay = as.data.frame(inDay)
inDay = as.numeric(unlist(inDay))
inData = as.data.frame(inData)
inQuest = fread("../Champion Survey/CSV/Questions.csv", header = TRUE)
inQuest = as.data.frame(inQuest)
outResponse = fread("../Champion Survey/CSV/Response.csv", header = TRUE)
outResponse = as.data.frame(outResponse)
# Here to do: View the previous data
plot(rowMeans(inData[,2:8])); title("Global Mean")
for (i in 1:7) {plot(inData[,i+1]); title(c("Question: ",names(inData[i+1])))}

# MA:
# filter(inData[,2],filter=rep(0.25,4),method="convolution",side=1)
#for (i in 1:7) {plot(filter(inData[,i+1],filter=rep(0.25,4),method="convolution")); title(c("MA Question: ",names(inData[i+1])));}

par(mfrow=c(2,2))
for (i in unique(inData$Survey_id))
{
  inD = inData[which(inData$Survey_id == i),-c(1,2)]
  plot(unlist(inD));title(c("Naive View", "Survey ",i))
}

par(mfrow=c(2,2))
for (i in unique(inData$Survey_id))
{
  inD = inData[which(inData$Survey_id == i),-c(1,2)]
  hist(x=unlist(inD), main=c("Naive histogram, Survey ",i),breaks=0:8)
}


par(mfcol=c(5,7));
for (j in 2:ncol(inData))
{
  quest = names(inData)[j]
  for (i in unique(inData$Survey_id))
  {
    inD = inData[which(inData$Survey_id == i),]
    hist(x=inD[,j], main=paste(c("Hist, Surv/Quest: ",i, " / ",quest),sep="",collapse=""),breaks=0:8)
  }
}
# By visual analysis of the above:
#1. Q3~Q4
#2. Q3 and Q7 seems to carry the most information
inQuest[3,2]
inQuest[4,2]
inQuest[7,2]
# At hand conclusion: Q7 shows a trend of DECREASING trust ...

# Do some ploting for Q7 vs. Q3
par(mfcol=c(1,1))
p <- ggplot()
survey = 1
idx1 = which(inData$Survey_id==survey)
p = p + geom_point(aes(x=rnorm(1,1,0.01)*inData$Q3[idx1],y=rnorm(1,1,0.1)*inData$Q7[idx1]),color=survey,size=5,alpha=0.2)
survey = 2
idx2 = which(inData$Survey_id==survey)
p = p + geom_point(aes(x=rnorm(1,1,0.01)*inData$Q3[idx2],y=rnorm(1,1,0.1)*inData$Q7[idx2]),color=survey,size=5,alpha=0.2)
survey = 3
idx3 = which(inData$Survey_id==survey)
p = p + geom_point(aes(x=rnorm(1,1,0.01)*inData$Q3[idx3],y=rnorm(1,1,0.1)*inData$Q7[idx3]),color=survey,size=5,alpha=0.2)
survey = 4
idx4 = which(inData$Survey_id==survey)
p = p + geom_point(aes(x=rnorm(1,1,0.01)*inData$Q3[idx4],y=rnorm(1,1,0.1)*inData$Q7[idx4]),color=survey,size=5,alpha=0.2)
survey = 5
idx5 = which(inData$Survey_id==survey)
p = p + geom_point(aes(x=rnorm(1,1,0.01)*inData$Q3[idx5],y=rnorm(1,1,0.1)*inData$Q7[idx5]),color=survey,size=5,alpha=0.2)
p


par(mfcol=c(1,1))
p <- ggplot()
for (survey in 1:5)
{
  idx = which(inData$Survey_id==survey)
  for (j in idx)
  {
    x1 = rnorm(10,inData$Q3[j],0.5)
    y1 = rnorm(10,inData$Q7[j],0.5)
    p <- p + geom_point(aes(x=x1,y=y1),color=survey)
  }
}
p

# Plot how each question evolved across surveys:
par(mfcol=c(1,1))
for ( q in 2:ncol(inData))
{
  set.seed(1000)
  print(length(x))
  x = inData[[1]]+rnorm(length(inData[[1]]),0,0.2)
  y = inData[[q]]+rnorm(length(inData[[q]]),0,0.2)
  plot(x,y,pch = 16, cex = 1.3, col = "blue")
  title(c("Question history",q-1,inQuest[q-1,2]))

  # Draw the linear regression line
  abline(lm(y ~ x))
  # Draw the polynomial interpolation of the 2 halves (under and above the regression line)
  idxtmp = which(y<lm(y~x)$fitted.values)
  ptsL1 <- loess.smooth(x[idxtmp], y[idxtmp], span=1/5)
  lines(ptsL1, lwd=2, col="red")
  idxtmp = which(y>=lm(y~x)$fitted.values)
  ptsL1 <- loess.smooth(x[idxtmp], y[idxtmp], span=1/5)
  lines(ptsL1, lwd=2, col="green")
}

# Combine the answers with some hand-crafted weights
par(mfcol=c(1,1))
y = 0*(1:nrow(inData))
coef = c(1,1,1,1,10,1,1)
coef = coef/sum(coef)
for ( q in 2:ncol(inData))
{
  set.seed(1000)
  x = inData[[1]]+rnorm(length(inData[[1]]),0,0.2)
  y = y+coef[q-1]*inData[[q]]+rnorm(length(inData[[q]]),0,0.2)
}
plot(x,y,pch = 16, cex = 1.3, col = "blue")
title(c("Question history (equal weighting)"," "))

# Draw the linear regression line
abline(lm(y ~ x))
# Draw the polynomial interpolation of the 2 halves (optimists and pessimists)
idxtmp1 = which(y<lm(y~x)$fitted.values)
ptsL1 <- loess.smooth(x[idxtmp1], y[idxtmp1], span=1/5)
lines(ptsL1, lwd=2, col="red")
idxtmp2 = which(y>=lm(y~x)$fitted.values)
ptsL2 <- loess.smooth(x[idxtmp2], y[idxtmp2], span=1/5)
lines(ptsL2, lwd=2, col="green")

# Draw the stock prices:
title(c("Question history (equal weighting)","Stock Price (scaled)"))
stp = outResponse$StockPrice
stp = (stp - min(stp))*10/(max(stp)-min(stp))
stpL = loess.smooth(x[idxtmp], stp[idxtmp], span=1/5)
lines(stpL, lwd=2, col="magenta")

# Draw some combinations of Optimists/Pessimists
c_index_x = 0.5*(ptsL2$x + ptsL1$x)
c_index_y = sqrt(ptsL2$y * ptsL1$y)
lines(c_index_x,c_index_y, lwd=5, col="blue")

# Draw again only the index:
plot(c_index_x,c_index_y , lwd=1, col="blue")
lines(c_index_x,c_index_y, lwd=5, col="blue")
title("Index, v0.1")

# Rescale the time-line:
c_index_t = sort((loess.smooth(x[idxtmp1], (inDay[idxtmp1]-inDay[1]), span=1/5))$y)
# Draw again the index on time scale
plot(c_index_t,c_index_y , lwd=1, col="blue")
#lines(c_index_t,c_index_y, lwd=5, col="blue")
title("Index, Time scale, v0.1")
# Smoth a little:
stpT = loess.smooth(c_index_t, c_index_y, span=1/5)
lines(stpT$x,stpT$y, lwd=5, col="blue")
grid()


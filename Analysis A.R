library(mosum)
source("jcp/R/jcp.r")
source("jcp/R/plot.jcp.r")
source("jcp/R/summary.jcp.r")
library(changepoint)
changeData = function(data, cpts){
  N=length(data)
  num = length(cpts)
  newData=c()
  if (num== 0){
    currMean=mean(data)
    newData=c(newData, (data-currMean)^2)}
  else
  {for (x in 1:num){
    if (x!=1){
      currentData = data[cpts[x-1]:(cpts[x]-1)]
      currMean=mean(currentData)
      newData=c(newData, (currentData-currMean)^2)
    }
    
    else{
      currentData= data[1:(cpts[1]-1)]
      currMean=mean(currentData)
      newData=c(newData,(currentData-currMean)^2)
    }
  }
    currentData = data[cpts[num]:N]
    currMean = mean(currentData)
    newData = c(newData,(currentData-currMean)^2)}
  return (log(newData))
}

td <-  testData(lengths = c(150,190,50,90), means = c(4,4,1,1),sds = c(1,2,2,4),seed = 333)
X = td$x

mosum = multiscale.localPrune(changeData(X,multiscale.localPrune(X,var.est.method="mosum.max",alpha=0.05)$cpts))$cpts


PELT=cpt.var(X,method="PELT")
BinSEg=cpt.var(X,method="BinSeg")
Joint=jcp(td$x)

plot(X, type="l", main="Variance Change Point Estimators", xlab="Time",ylab="",lwd=1.2)
abline(v=c(150,390),col="red",lwd=1.5)
abline(v=c(340),col="red",lty=2,lwd=1.5)
points(x=mosum,y=c(4,4),col="turquoise4",pch=16,cex=1.75)
points(x=PELT@cpts,y=c(8,8,8), col="olivedrab", pch=16,cex=1.75)
points(x=BinSEg@cpts,y=c(10,10,10),col="slategray",pch=16,cex=1.75)
points(x=Joint$changepoints,y=c(6,6),col="royalblue",pch=16,cex=1.75)
legend("bottomleft",        
       legend = c('AdjData','PELT','BinSeg','jcp'), 
       col = c("turquoise4","olivedrab","slategray","royalblue"),      
       pch = 16,          
       cex = .6) 

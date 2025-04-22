source("jcp/R/jcp.r")
source("jcp/R/plot.jcp.r")
source("jcp/R/summary.jcp.r")
library(mosum)
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

RemoveDup = function(arr){
  arr = sort(arr)
  i=2
  while (i <= length(arr)) {
    # Remove current item if it is within 30 of the next time
    if (abs(arr[i] - arr[i - 1]) <= 30) {
      arr = arr[-i]  
    } 
    i = i + 1  
  }
  
  return(arr)
}

combine <- function(array1, array2, min_diff = 30) {
  # Combine and sort the two arrays
  combined_array = sort(c(array1, array2))
  
  # Initialize the result array with the first element
  result = RemoveDup(combined_array)
  
  return(result)
}

set.seed(111)
m = c() #holds all change points identified by mosum
j= c() #holds all change points identified by joint detection
p = c() #holds all change points identified by PELT
b = c() #holds all change points identified by Binary Segmentation

for (x in 1:500){
  td = testData(lengths = c(200, 60, 240, 100, 110),means=c(-2,3,8,8,8),sds=c(6, 6, 2, 1, 4))
  X = td$x
  
  #Using Joint detection
  joint=jcp(X)
  j=c(j,joint$changepoints)
  
  #Using two step mosum
  momu= multiscale.localPrune(X,var.est.method="mosum.max",alpha=0.05)
  mosd = multiscale.localPrune(changeData(X,momu$cpts),var.est.method="mosum",alpha=0.05)
  mocom = combine(mosd$cpts,momu$cpts)
  m=c(m,mocom)

  #Using PELT
  pelt = cpt.meanvar(X,method="PELT",pen.value = 0.05)
  pe=RemoveDup(pelt@cpts)
  p = c(p,pe)
  
  #Using Binary Segmentation
  bin = cpt.meanvar(X,method="BinSeg",pen.value = 0.05)
  bi = RemoveDup(bin@cpts)
  b = c(b,bi)
}
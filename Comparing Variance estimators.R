library(mosum)

#######################################
#Results may vary slightly between runs as no random seed was set. 
#Nonetheless, these differences do not affect the overall conclusions drawn.
#######################################################

set.seed(33)

#Time series with 1 change point

min1=c() # holds change points found using B
max1=c() # holds change points found using C
mo=c() # holds change points found using A
for (T in 1:500){
  x = sample(100:350,1)
  td = testData(lengths=c(x,400-x),means=c(1,2),sds=sqrt(c(1,0.5)))
  mp_max = multiscale.localPrune(td$x,var.est.method="mosum.max")
  max1=c(max1,mp_max$cpts)
  mp_min = multiscale.localPrune(td$x,var.est.method="mosum.min")
  min1=c(min1,mp_min$cpts)
  mp_mo = multiscale.localPrune(td$x,var.est.method="mosum")
  mo=c(mo,mp_mo$cpts)
}

#Time series with 2 change points

min2=c() # holds change points found using B
max2=c() # holds change points found using C
mo2=c() # holds change points found using A
for (T in 1:500){
  x = sample(100:200,1)
  y = sample(100:200,1)
  td = testData(lengths=c(x,y,400-x-y),means=c(1,2,0.5),sds=sqrt(c(1,0.5,2)))
  mp_max = multiscale.localPrune(td$x,var.est.method="mosum.max")
  max2=c(max2,mp_max$cpts)
  mp_min = multiscale.localPrune(td$x,var.est.method="mosum.min")
  min2=c(min2,mp_min$cpts)
  mp_mo = multiscale.localPrune(td$x,var.est.method="mosum")
  mo2=c(mo2,mp_mo$cpts)
}

#Time series with 3 change points

min3=c() # holds change points found using B
max3=c() # holds change points found using C
mo3=c() # holds change points found using A
for (T in 1:500){
  x = sample(50:132,1)
  y = sample(50:132,1)
  z= sample(50:132,1)
  td = testData(lengths=c(x,y,z,400-x-y-z),means=c(1,2,0.5,3),sds=sqrt(c(1,0.5,2,1.5)))
  mp_max = multiscale.localPrune(td$x,var.est.method="mosum.max")
  max3=c(max3,mp_max$cpts)
  mp_min = multiscale.localPrune(td$x,var.est.method="mosum.min")
  min3=c(min3,mp_min$cpts)
  mp_mo = multiscale.localPrune(td$x,var.est.method="mosum")
  mo3=c(mo3,mp_mo$cpts)
}



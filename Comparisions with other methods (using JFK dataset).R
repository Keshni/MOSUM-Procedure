library(mscp)
library(wbs)
library(changepoint)
library(mosum)

#get dataset
source('./load_dataset.R')
df <- load.dataset('jfk_passengers.json')
df$`Number of Passengers`=df$`Number of Passengers`/100000


start =  Sys.time()
mosum = multiscale.localPrune(df$`Number of Passengers`, var.est.method="mosum.max")
end = Sys.time()
print("time for mosum")
print(end-start)

start =  Sys.time()
mscp = mscp(df$`Number of Passengers`)
end = Sys.time()
print("time for mscp")
print(end-start)

start =  Sys.time()
wbsStat= wbs(df$`Number of Passengers`)
wbs = changepoints(wbsStat)
end = Sys.time()
print("time for wbs")
print(end-start)


start =  Sys.time()
pelt = cpt.mean(df$`Number of Passengers`, method="PELT",minseglen = 60)
end = Sys.time()
print("time for pelt")
print(end-start)

start =  Sys.time()
segNeigh = cpt.mean(df$`Number of Passengers`,method='SegNeigh',penalty="None")
end = Sys.time()
time=c(time,end-start)
print("time for segneigh")
print(end-start)


plot(mosum,xlab="Time", type="l",display="data", shaded="none")
abline(v = mosum$cpts,col="red")
points(mscp$cp, y=50,pch=16, col="purple",cex=1.5)
points(wbs$cpt.ic$bic.penalty, y=rep(45,length(wbs$cpt.ic$bic.penalty)),pch=16, col="darkgreen",cex=1.5)
points(pelt@cpts, y=rep(40,length(pelt@cpts)),pch=16, col="navy",cex=1.5)
points(segNeigh@cpts, y=rep(35,length(segNeigh@cpts)),pch=16, col="peru",cex=1.5)
legend("bottomright", legend = c('MSCP','WBS','PELT','SegNeigh'), 
       col = c("purple","darkgreen","navy","peru"), pch=16,cex=0.4)

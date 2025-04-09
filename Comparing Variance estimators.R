library(mosum)

#######################################
#Results may vary slightly between runs as no random seed was set. 
#Nonetheless, these differences do not affect the overall conclusions drawn.
#######################################################

#getting blocks time series
td= testData(model="blocks")

#running MOSUM with different variances
A=mosum(td$x,G=70)
B=mosum(td$x,G=70,var.est.method = "mosum.min")
C= mosum(td$x,G=70,var.est.method = "mosum.max")

print("True change points")
print(td$cpts)
print("change points located with A")
print(A$cpts)
print("change points located with B")
print(B$cpts)
print("change points located with C")
print(C$cpts)


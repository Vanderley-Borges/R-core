#AMMI
library(agricolae)
data(plrv)
plrv
attach(plrv)
model<- AMMI(Locality, Genotype, Rep, Yield)
model
detach(plrv)
# biplot PC2 vs PC1
plot(model)
## plot PC1 vs Yield
plot(model,0,1,gcol="blue",ecol="green")
## triplot PC 2,3,4
plot(model,first=2,second=3,third=4, type=2,number=TRUE)
# biplot with influence genotype in pc3 vs pc2
plot(model,first=2,second=3, type=3,number=TRUE,icol="green")
#Genetic analysis using the sommer package Giovanny Covarrubias-Pazaran. 2016-07-25
#Marker and non-marker based heritability calculatio
#install.packages("sommer")
library(sommer) 
data(h2) 
head(h2)
ans1 <- mmer2(y~1, random=~Name + Env + Name:Env + Block,data=h2, silent = TRUE) 
vc <- ans1$var.comp 
V_E <- vc[2,1]
V_GE <- vc[3,1]
V_G <- vc[1,1];Ve <- vc[5,1]

n.env <- length(levels(h2$Env)) 
h2 <- V_G/(V_G + V_GE/n.env + Ve/(2*n.env)) #the 2 is a reference for block h2


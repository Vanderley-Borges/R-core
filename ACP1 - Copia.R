#===========================================================
# Exemplo 1
#===========================================================
SIGMA  <- matrix(c( 1,-2,0,-2,5,0,0,0,2),3,3,T)
W      <- eigen(SIGMA)   
lambda <- W$values  # autovalores
P      <- W$vectors # autovetores 
diag(lambda) # matriz diagonal dos autovalores 
P            # matriz com autovetores por coluna  
lambda/sum(lambda) # Proporcao acumulada da variacao total

# correlacao de Y_j com as variaveis X's
sigma <- sqrt(diag(SIGMA))  # desvios padrao de X_1, X_2 e X_3
P[,1]*sqrt(lambda[1])/sigma #Correl. de Y_1 com X1, X2 ou X_3
P[,2]*sqrt(lambda[2])/sigma #Correl. de Y_2 com X1, X2 ou X_3
P[,3]*sqrt(lambda[3])/sigma #Correl. de Y_3 com X1, X2 ou X_3

rm(list=ls())

#===========================================================
# Exemplo 2
#===========================================================
# matriz de covariancias
SIGMA  <- matrix(c( 1,4,4,100),2,2,T)
W      <- eigen(SIGMA)   
lambda <- W$values  # autovalores
P      <- W$vectors # autovetores 
diag(lambda) # matriz diagonal dos autovalores 
P            # matriz com autovetores por coluna  
lambda/sum(lambda) # Proporcao acumulada da variacao total

# correlacao de Y_j com as variaveis X's
sigma <- sqrt(diag(SIGMA))  # desvios padrao de X1 e X_2
P[,1]*sqrt(lambda[1])/sigma #Correl. de Y_1 com X1 ou X2
P[,2]*sqrt(lambda[2])/sigma #Correl. de Y_2 com X1 ou X2

#----------------------
# matriz de correlacoes
Rho    <- cov2cor(SIGMA)
U      <- eigen(Rho)   
delta  <- U$values  # autovalores
Q      <- U$vectors # autovetores 
diag(delta)  # matriz diagonal dos autovalores 
Q            # matriz com autovetores por coluna  
delta/sum(delta) # Proporcao acumulada da variacao total

# correlacao de Y1 com as variaveis X's
Q[,1]*sqrt(delta[1])/1 #Correl. de Y_1 com X1 ou X2
Q[,2]*sqrt(delta[2])/1 #Correl. de Y_2 com X1 ou X2

rm(list=ls())

#===========================================================
# Exemplo 3
#===========================================================
# Dados socioeconomicos: tabela T8-5.dat

dados  <- read.table("../JohnsonWichernDataSets/T8-5.DAT",header=F,sep="	",
          col.names=c("pop","prof","emprego","funcpub","casa"))
dados
round(apply(dados,2,"mean"),digits=2)
round(cov(dados), digits=3)
round(cor(dados), digits=2)

#library(xtable)               # matrizes para o LaTeX
#xtable(cov(dados), digits=3)  # matriz de covariancias para o LaTeX
#xtable(cor(dados), digits=2)  # matriz de correlacoes para o LaTeX 

#------------------------------------------------------------
# Analise de componentes principais via funcao 'prcomp' do R

?prcomp  # Ajuda

pcvar  <- prcomp(dados,scale=F)
P      <- pcvar$rotation 
sdev   <- pcvar$sdev  # raiz quadrada dos autovalores
#xtable(P,digits=4)
pcvar
summary(pcvar)

# correlacao de Y_j com as variaveis X's
s  <- apply(dados,2,"sd") # desvios padrao de X_1,...,X_5 
P[,1]*sdev[1]/s   # Correl. de Y_1 com X1,..., ou X_5
P[,2]*sdev[2]/s   # Correl. de Y_2 com X1,..., ou X_5
P[,3]*sdev[3]/s   # Correl. de Y_3 com X1,..., ou X_5
P[,4]*sdev[4]/s   # Correl. de Y_3 com X1,..., ou X_5
P[,5]*sdev[5]/s   # Correl. de Y_3 com X1,..., ou X_5

# screeplot
#pdf(file="screeplot_exemplo3.pdf")
par(mfrow=c(1,1),lwd=2.0,cex.lab=1.5,cex.axis=1.5,
     lab=c(5,7,5),mar=c(4.5,4.5,1,1),xpd=T,cex.main=2.0)
plot(1:5,sdev^2,type="l",xlab=expression(j),ylab="Variâncias")
points(1:5,sdev^2,pch=15)
#dev.off()

rm(list=ls())

#===========================================================
# Exemplo 4
#===========================================================
# Dados da bolsa de valores de 'New York': tabela T8-4.dat

dados  <- read.table("../JohnsonWichernDataSets/T8-4.DAT",header=F,sep="	",
          col.names=c("JP","Citi","Fargo","Royal","Exxon"))
dados
round(apply(dados,2,"mean"),digits=5)
round(cor(dados), digits=2)

dados
round(apply(dados,2,"mean"),digits=5)
round(cov(dados), digits=3)
round(cor(dados), digits=3)


pcvar  <- prcomp(dados,scale=T)
P      <- pcvar$rotation 
sdev   <- pcvar$sdev  # raiz quadrada dos autovalores
#xtable(P,digits=4)
pcvar
summary(pcvar)

# correlacao de Y_j com as variaveis X's
s  <- apply(dados,2,"sd") # desvios padrao de X_1,...,X_5 
P[,1]*sdev[1]   # Correl. de Y_1 com X1,..., ou X_5
P[,2]*sdev[2]   # Correl. de Y_2 com X1,..., ou X_5
P[,3]*sdev[3]   # Correl. de Y_3 com X1,..., ou X_5
P[,4]*sdev[4]   # Correl. de Y_3 com X1,..., ou X_5
P[,5]*sdev[5]   # Correl. de Y_3 com X1,..., ou X_5

# screeplot
#pdf(file="screeplot_exemplo4.pdf")
par(mfrow=c(1,1),lwd=2.0,cex.lab=1.5,cex.axis=1.5,
     lab=c(5,7,5),mar=c(4.5,4.5,1,1),xpd=T,cex.main=2.0)
plot(1:5,sdev^2,type="l",xlab=expression(j),ylab="Variâncias")
points(1:5,sdev^2,pch=15)
#dev.off()

# Graficos de dispersao dos dois primeiros CP's
y1 <-  as.matrix(dados)%*%pcvar$rotation[,1]
y2 <-  as.matrix(dados)%*%pcvar$rotation[,2]
x11()
par(mfrow=c(2,2))
plot(y1,y2,pch=15,col=2,lwd=2)
qqnorm(y2,main="Normal Q-Q Plot y_2")
qqnorm(y1,main="Normal Q-Q Plot y_1")

# Graficos de dispersao dos dois ultimos CP's
y4 <-  as.matrix(dados)%*%pcvar$rotation[,4]
y5 <-  as.matrix(dados)%*%pcvar$rotation[,5]
x11()
par(mfrow=c(2,2))
plot(y4,y5,pch=15,col=2,lwd=2)
qqnorm(y5,main="Normal Q-Q Plot y_5")
qqnorm(y4,main="Normal Q-Q Plot y_4")

x11()
par(mfrow=c(2,2))
boxplot(y5,main="Boxplot y_5")
boxplot(y4,main="Boxplot y_4")




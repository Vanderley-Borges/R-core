#==================================================================
# Exemplo 1
#==================================================================
SIGMA <- matrix(c(19,30,2,12,30,57,5,23,2,5,38,47,12,23,47,68),4,4,T)
L     <- matrix(c(4,1,7,2,-1,6,1,8),4,2,T)
PSI   <- diag(c(2,4,1,3))
SIGMA
L
PSI
L%*%t(L)+PSI             # Verificando os valores 
SIGMA - (L%*%t(L) + PSI) # Todos iguais a zero

# Comunalidades 
sum2 <- function(x){return(sum(x^2))}
h    <- apply(L,1,"sum2")
h

rm(list=ls())
#==================================================================
# Exemplo 2
#==================================================================
# A matriz RHO e' positiva definida
RHO    <- matrix(c(1,0.9,0.7,0.9,1,0.4,0.7,0.4,1),3,3,T)

# Note que e' possivel obter uma solucao via CP, mas ela 
# nao e' exata. So' seria exata para m=p=3
dec    <- eigen(RHO)
delta  <- dec$values
P      <- dec$vectors
ell.1  <- sqrt(delta[1])*P[,1]
ell.2  <- sqrt(delta[2])*P[,2]
ell.3  <- sqrt(delta[3])*P[,3]
L      <- cbind(ell.1,ell.2,ell.3)
RHO-ell.1%*%t(ell.1) # Se considerar m=1 a solucao nao e' exata

# Aproveitamos o exemplo para verificar:
delta/3         # Proporcao da variabilidade
cumsum(delta)/3 # Proporcao da variabilidade acumulada

# Para conferir, note que delta_k = ell.1_k'*ell.1_k
# Logo, podemos ver a proporcao da variabilidade devida ao 
# k-esimo fator atraves da soma de quadrados dos 
# valores da k-esima coluna de L
delta
cbind((ell.1)%*%ell.1,(ell.2)%*%ell.2,(ell.3)%*%ell.3)

rm(list=ls())
#==================================================================
# Exemplo 3
#==================================================================
library(xtable)

# Correlacao amostral
R <- matrix(c(
   1, 0.02, 0.96, 0.42, 0.01,
0.02,    1, 0.13, 0.71, 0.85,
0.96, 0.13,    1, 0.50, 0.11,
0.42, 0.71, 0.50,    1, 0.79,
0.01, 0.85, 0.11, 0.79,    1),5,5,T)

#---------------------------------------------
# Analise fatorial via componentes principais
#---------------------------------------------
p       <- 5 # numero de variaveis
AF      <- eigen(R)
delta   <- AF$values
P       <- AF$vector
delta
P

delta/p         # Proporcao da variabilidade
cumsum(delta)/p # Proporcao da variabilidade acumulada
# Escolhemos m=2

# Estimacao da matriz de cargas dos fatores para m=2
ell.1 <- sqrt(delta[1])*P[,1]
ell.2 <- sqrt(delta[2])*P[,2]
L     <- cbind(ell.1,ell.2)
L

# Para conferir, note que delta_j = ell.1_j'*ell.1_j
# Logo, podemos ver a proporcao da variabilidade devida ao 
# k-esimo fator atraves da soma de quadrados dos 
# valores da k-esima coluna de L
delta
cbind((ell.1)%*%ell.1,(ell.2)%*%ell.2)

# Estimacao das comunalidades 
sum2 <- function(x){return(sum(x^2))}
h    <- apply(L,1,"sum2")
h

# Estimacao das variancias especificas
psi  <- 1 - h # variancia das variaveis padronizadas - comundalidades 
psi

# Para o latex
SAIDA  <- cbind(L,h,psi)
SAIDA
xtable(SAIDA,digits=3)

# Estimacao/Recomposicao de R
R.est  <- L%*%t(L)+diag(psi)
xtable(R.est,digits=3)

# Matriz residual
U <- R- R.est
xtable(U,digits=3)

#---------------------------------------------------
# Rotacao de fatores - Exemplo 3 (continuacao)
#---------------------------------------------------
# Ver esta parte so' apos estudar rotacao de fatores
L

# Vamos utilizar o metodo varimax
V.L <- varimax(L)
V.L
xtable(cbind(L,V.L$loadings[,1:2]),digits=3)

#x11()
pdf("rotacao_varimax.pdf")
par(mfrow=c(1,1),mar=c(5,5,1,1))
plot(L,pch=15,xlab=expression(hat(f)[1]),ylab=expression(hat(f)[2]),xlim=c(-1,0))
lines(c(-V.L$rotmat[1,1],0),c(-V.L$rotmat[2,1],0),col=2,lwd=2)
lines(c( V.L$rotmat[1,2],0),c( V.L$rotmat[2,2],0),col=3,lwd=2)
abline(v=0,col=1,lwd=2)
abline(h=0,col=1,lwd=2)
text(-0.58, 0.78,"1")
text(-0.75,-0.52,"2")
text(-0.68, 0.75,"3")
text(-0.91,-0.10,"4")
text(-0.77,-0.55,"5")
text(-0.45,-0.20,expression(hat(f)[1]^"*"))
text(-0.45, 0.60,expression(hat(f)[2]^"*"))
dev.off()

# Poderiamos trocar os sinais correspondente ao primeiro fator
# Utilizariamos outra rotacao
Q   <- matrix(c(-1,0,0,1),2,2,T)
Q
V.L$loadings[,1:2]%*%Q

rm(list=ls())
#==================================================================
# Exemplo 4 - Analise fatorial via componentes principais
#==================================================================
library(xtable)

#====================================
# Dados das ações da bolsa de valores
dados  <- read.table("../JohnsonWichernDataSets/T8-4.DAT",header=FALSE,sep="	",
       col.names=c("JP","Citi","Fargo","Royal","Exxon"))

n      <- dim(dados)[1]
p      <- dim(dados)[2]
n
p
R      <- cor(dados)

#---------------------------------------------
# Analise fatorial via componentes principais
#---------------------------------------------
AF      <- prcomp(dados,scale=T)
delta   <- AF$sdev^2
P       <- AF$rotation 
delta
P

delta/p         # Proporcao da variabilidade
cumsum(delta)/p # Proporcao da variabilidade acumulada
# Escolhemos m=2

# Estimacao da matriz de cargas dos fatores
ell.1 <- sqrt(delta[1])*P[,1]
ell.2 <- sqrt(delta[2])*P[,2]
L.1   <- ell.1               # um fator
L.2   <- cbind(ell.1,ell.2)  # dois fatores
L.1
L.2

# Estimacao das comunalidades 
sum2  <- function(x){return(sum(x^2))}
h.1   <- L.1^2                 # um fator
h.2   <- apply(L.2,1,"sum2")   # dois fatores
h.1
h.2

# Estimacao das variancias especificas
psi.1 <- 1 - h.1 # um fator
psi.2 <- 1 - h.2 # dois fatores
psi.1
psi.2

# Para o latex
SAIDA  <- cbind(L.1,psi.1,L.2,psi.2)
SAIDA
xtable(SAIDA,digits=3)

# Estimacao/Recomposicao de R para m=2
R.est.1  <- L.1%*%t(L.1)+diag(psi.1)
R.est.2  <- L.2%*%t(L.2)+diag(psi.2)
xtable(R.est.1,digits=3)
xtable(R.est.2,digits=3)

# Matriz residual
U <- R- R.est.2
xtable(U,digits=3)

# Comparacoes: note as entradas [1,3] das matrizes
R
R.est.1
R.est.2

rm(list=ls())
#==================================================================
# Exemplo 5 - Analise fatorial via maxima verossimilhanca
#==================================================================
library(xtable)

#====================================
# Dados das ações da bolsa de valores
dados  <- read.table("../JohnsonWichernDataSets/T8-4.DAT",header=FALSE,sep="	",
       col.names=c("JP","Citi","Fargo","Royal","Exxon"))

n      <- dim(dados)[1]
p      <- dim(dados)[2]
n
p
R      <- cor(dados)

#---------------------------------------------
# Analise fatorial via maxima verossimilhanca
#---------------------------------------------
AFMV.1 <- factanal(dados,factors=1,rotation="none")
AFMV.2 <- factanal(dados,factors=2,rotation="none")

# Estimacao da matriz de cargas dos fatores
L.1    <- AFMV.1$loadings[,1]
L.2    <- AFMV.2$loadings[,1:2]
L.1
L.2

# Estimacao das comunalidades 
sum2  <- function(x){return(sum(x^2))}
h.1   <- L.1^2                 # um fator
h.2   <- apply(L.2,1,"sum2")   # dois fatores
h.1
h.2

# Estimacao das variancias especificas
psi.1    <- AFMV.1$uniqueness
psi.2    <- AFMV.2$uniqueness
psi.1
psi.2

# Para o latex
SAIDA  <- cbind(L.1,psi.1,L.2,psi.2)
SAIDA
xtable(SAIDA,digits=3)

# Estimacao/Recomposicao de R para m=2
R.est.1  <- L.1%*%t(L.1)+diag(psi.1)
R.est.2  <- L.2%*%t(L.2)+diag(psi.2)
xtable(R.est.1,digits=3)
xtable(R.est.2,digits=3)

# Matriz residual
U <- R- R.est.2
xtable(U,digits=3)

# Comparacoes: note as entradas [1,3] das matrizes
R
R.est.1
R.est.2

# Soma de quadrado das cargas para o k-esimo fator
ssc.1 <- sum2(L.1)           # um fator
ssc.2 <- apply(L.2,2,"sum2") # dois fatores
ssc.1
ssc.2

# Proporcao da variabilidade total
ssc.1/p                    # um fator
ssc.2/p                    # dois fatores
prop.ac <- cumsum(ssc.2)/p # dois fatores
prop.ac 

# Para comparar os resultados acima, podemos ver diretamente:
AFMV.1
AFMV.2

# Matriz residual
U <- R - R.est.2
xtable(U,digits=3)

# Comparacoes: note as entradas [1,3] das matrizes
R
R.est.1
R.est.2

# Testes de adequacao
nu.1 <- ((p-1)^2-p-1)/2
nu.2 <- ((p-2)^2-p-2)/2
nu.1
nu.2

m1  <- 1
W1  <- (n-1-(2*p + 4*m1 + 5)/6)*( log(det(R.est.1))-log(det(R)))
W1
W1  > qchisq(0.95,nu.1) 
valorp.1 <- 1-pchisq(W1,nu.1)
valorp.1
# Compare com o resultado de 
AFMV.1

m2  <- 2
W2  <- (n-1-(2*p + 4*m2 + 5)/6)*( log(det(R.est.2))-log(det(R)))
W2
W2  > qchisq(0.95,nu.2) 
valorp.2 <- 1-pchisq(W2,nu.2)
valorp.2
# Compare com o resultado de 
AFMV.2

#---------------------------------------------------
# Rotacao de fatores - Exemplo 5 (continuacao)
#---------------------------------------------------
# Ver esta parte so' apos estudar rotacao de fatores
# Poderiamos utilizar a funcao "varimax" em L.2
# Mas utilizaremos a opcao "varimax" em "factanal"

AFMVmax.2 <- factanal(dados,factors=2,rotation="varimax")

# Estimacao da matriz de cargas dos fatores
Lmax.2    <- AFMVmax.2$loadings[,1:2]
Lmax.2

xtable(cbind(L.2,Lmax.2),digits=3)

#---------------------------------------------------
# Estimacao dos escores dos fatores 
# Exemplo 5 (continuacao)
#---------------------------------------------------
# Ver esta parte so' apos estudar estimacao dos 
# escores dos fatores

AFMV.2.Reg <- factanal(dados,factors=2,rotation="none",scores="regression")
AFMV.2.Bar <- factanal(dados,factors=2,rotation="none",scores="Bartlett")

par(mfrow=c(1,2))
plot(AFMV.2.Reg$scores[,1],AFMV.2.Bar$scores[,1])
plot(AFMV.2.Reg$scores[,2],AFMV.2.Bar$scores[,2])

xtable(cor(AFMV.2.Reg$scores,AFMV.2.Bar$scores),digits=10)

#---------------------------------------------------
# Verificando a estabilidade da solucao
# Dados completos e
# subconjuntos de 51 e 52 observacoes
#---------------------------------------------------

AFMVmax.2a <- factanal(dados[1:52,],factors=2,rotation="varimax")
AFMVmax.2b <- factanal(dados[53:n,],factors=2,rotation="varimax")
Lmax.2a    <- AFMVmax.2a$loadings[,1:2]
Lmax.2b    <- AFMVmax.2b$loadings[,1:2]

AFMVmax.2
AFMVmax.2a
AFMVmax.2b

# Para o latex
xtable(cbind(Lmax.2,Lmax.2a,Lmax.2b),digits=3)

#==================================================================
# Fim
#==================================================================

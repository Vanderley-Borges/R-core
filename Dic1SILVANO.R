#-------------------------------
#' Para localizar as libraries -
#'------------------------------
.libPaths()

#'-----------------------------------------------------
#' Para verificar os pacotes instalados nas libraries -
#'----------------------------------------------------- 
lapply(.libPaths(), dir)

#------------------------------------------------------------
# Dados sobre a reducao do colesterol (mg/dL) apos o uso de -
# quatro tipos de drogas.                                   -
#                                                           -
# Material apresentado nos slides da Patologia.             -
#------------------------------------------------------------
rm(list=ls())

(croqui = expand.grid(rep=1:5, trat=LETTERS[1:4]))

(colest = c(4.38,  4.87, 7.22,  8.42, 8.95,
            9.37, 10.10, 4.94,  8.41, 6.75,
            9.17,  7.12, 9.22, 10.67, 6.89,
            8.05,  9.80, 8.71,  9.04, 8.99))

(dados = data.frame(croqui, resp=colest))

#--------------------------
# Estatistica descritiva: -
#--------------------------
#'
# Geral
(soma = with(dados, sum(resp)))
(media = with(dados, mean(resp)))
(variancia = with(dados, var(resp)))
(desvio = with(dados, sd(resp)))
(CV = desvio / media * 100)

# Usando o pacote raster
require(raster)
with(dados, cv(resp))

# Por tratamento
(somas = with(dados, tapply(resp, trat, sum)))
(medias = with(dados, tapply(resp, trat, mean)))
(variancias = with(dados, tapply(resp, trat, var)))
(desvios = with(dados, tapply(resp, trat, sd)))
(cv = desvios/medias * 100)

with(dados, cv(resp, trat))


#------------
# Graficos: -
#------------
with(dados, stripchart(resp ~ trat, pch=20, vertical='T', las=1, ylab='Colesterol (mg/dL)',
                       xlab='Drogas', cex=1.5, col='blue'))

require(car)
with(dados, Boxplot(resp ~ trat, las=1, xlab='Drogas', ylab='Colesterol (mg/dL)', col='lightyellow'))
points(medias, pch='+', cex=1.8, col='red')

with(dados, stripchart(resp ~ trat, pch=20, vertical='T', las=1, ylab='Colesterol (mg/dL)',
                       xlab='Drogas', cex=1.5, col='blue', add=T))

#-----------------------
# Analise de variancia -
#-----------------------
mod = with(dados, aov(resp ~ trat))
summary(mod)

#---------------
# Pressupostos -
#---------------
#'
#---------------------------------
# Teste de normalidade dos erros -
#---------------------------------
#'
# Teste de Shapiro-Wilk
shapiro.test(mod$res)

# Teste de Kolmogorov-Smirnov
require(nortest)

lillie.test(mod$res)

# Teste de Anderson-Darling
ad.test(mod$res)

require(hnp)
hnp(mod, las=1, pch=19, halfnormal=F)
hnp(mod, las=1, pch=19, halfnormal=T)


#---------------------------------------
# Teste de homogeneidade de variancias -
#---------------------------------------
#'
# Teste de Bartlett
with(dados, bartlett.test(mod$res ~ trat))
with(dados, bartlett.test(resp ~ trat))

# Teste de Levene
with(dados, leveneTest(mod$res ~ trat, center=mean))
with(dados, leveneTest(mod$res ~ trat, center=median))

#--------------------------
# Independência dos erros -
#--------------------------
plot(mod$res, las=1, pch=19, col='red', ylab='Resíduos')

#----------------------------
# Usando o pacote easyanova -
#----------------------------
#
# E necessário excluir a coluna de repetições
(dados.t = dados[ , 2:3])

require(easyanova)

mod.1 = ea1(dados.t, design=1, plot=3)       # plot=1,2,3
names(mod.1)
mod.1




mod3 = aov(rating ~ portfolio, data=pf)
summary.aov(mod3, split=list(portfolio = list(
  agressiveness=1, balance=2, environ=3)))
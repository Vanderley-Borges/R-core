# Análise de experimento em delineamento de blocos casualizados (dbc) com repetições dentro
# da parcela, fator de níveis categóricos (balanceado), resposta contínua.
#==========================================================================================

#------------------------------------------------------------------------------------------
# sobre o modelo atribuído ao experimento
# suposição distribucional: Y|x ~ Normal(\mu(x), \sigma)
# suposição funcional:      \mu(x) = \mu+\alpha_i+\tau_j+E_ij  (i=2,...; j=2,...)
#                           E_ij ~ Normal(0, \sigma_e)

#------------------------------------------------------------------------------------------
# definições da sessão

require(lattice)
require(agricolae)
require(contrast)
require(multcomp)
require(doBy)
require(latticeExtra)
require(ExpDes)
require(plyr)

#------------------------------------------------------------------------------------------
# lendo arquivos de dados

da <- read.table("http://www.leg.ufpr.br/~walmes/data/ramalho_eucalipto.txt",
                 header=TRUE, sep="\t", colClasses=c("factor","factor",NA,NA))

# ver os dados
da
str(da)

#------------------------------------------------------------------------------------------


xtabs(~bloco+progenie, da) # medidas por parcela

xyplot(volume~progenie, groups=bloco, data=da)
xyplot(volume~progenie|bloco, groups=arvore, data=da)

#------------------------------------------------------------------------------------------
# ajuste do modelo com todas as observações

m0 <- lm(volume~bloco+progenie+bloco:progenie, data=da) # modelo errado, para ver resíduos

par(mfrow=c(2,2)); plot(m0); layout(1) # análise dos pressupostos

m0 <- aov(volume~bloco+progenie+Error(bloco:progenie), data=da) # modelo correto

summary(m0)    # quadro de análise de variância

#------------------------------------------------------------------------------------------
# quando se ajusta com aov() e usa-se o termo Error(), a classe do objeto é aovlist,
# que não possui muitos métodos disponíveis, então faremos análise de forma diferente,
# com os dados médios e usando o argumento weights.

db <- ddply(da, .(bloco, progenie), summarise, volume=mean(volume), n=length(volume))
str(db)

m1 <- lm(volume~bloco+progenie, db, weight=n)

par(mfrow=c(2,2)); plot(m1); layout(1) # análise dos pressupostos

anova(m1)   # note que é o mesmo quadro de análise de variância
summary(m0)

summary(m1)

#------------------------------------------------------------------------------------------
# médias ajustadas

pm <- as.data.frame(popMeans(m1, effect="progenie"))
pm

#------------------------------------------------------------------------------------------
# contrastes entre as médias ajustadas

source("funcoes.R")

pmc <- popMatrix(m1, effect="progenie")

allcontr <- apc(pmc)

c0 <- summary(glht(m1, linfct=allcontr), test=adjusted(type="fdr"))
c0

c0$focus <- "progenie"
cld(c0)

#------------------------------------------------------------------------------------------
# colocando resultados em um gráfico

pm$progenie <- factor(pm$progenie, levels=levels(db$progenie))
pm$cld <- cld(c0)$mcletters$Letters

segplot(reorder(progenie, Estimate)~Lower+Upper, centers=Estimate, data=pm,
        ylab="Progenie", xlab="Volume",
        draw.bands=FALSE, segments.fun=panel.arrows, ends="both",
        angle=90, length=1, unit="mm",
        panel=function(x, y, z, centers, subscripts, ...){
          panel.segplot(x, y, z, centers=centers, subscripts=subscripts, ...)
          panel.text(centers, z,
                     labels=paste(format(centers, digits=3), pm$cld[subscripts]),
                     pos=3)
        })

#------------------------------------------------------------------------------------------
# limpa a memória e importa dados

rm(list=ls())
ls()

da <- read.table("http://www.leg.ufpr.br/~walmes/data/ramalho_feijao1.txt",
                 header=TRUE, sep="\t", colClasses=c("factor","factor",NA,NA))
str(da)
da[is.na(da$producao),]
da <- na.omit(da)

#------------------------------------------------------------------------------------------
# ver os dados

xtabs(~repeticao+familia, da) # medidas por parcela

xyplot(producao~familia, groups=repeticao, data=da)
xyplot(producao~familia|repeticao, groups=planta, data=da)

#------------------------------------------------------------------------------------------
# ajuste do modelo com todas as observações

m0 <- lm(producao~repeticao+familia+repeticao:familia, data=da) # errado, para ver resíduos
par(mfrow=c(2,2)); plot(m0); layout(1) # análise dos pressupostos, hetero de var

MASS::boxcox(m0) # indica raiz cúbica para transformação

m0 <- lm(producao^(1/3)~repeticao+familia+repeticao:familia, data=da) # transformada
par(mfrow=c(2,2)); plot(m0); layout(1) # análise dos pressupostos, ok

m0 <- aov(producao^(1/3)~repeticao+familia+Error(repeticao:familia), data=da) # correto

summary(m0)    # quadro de análise de variância

#------------------------------------------------------------------------------------------
# fazendo a análise com as médias e os pesos

db <- ddply(da, .(repeticao, familia), summarise,
            producao=mean(producao), rproducao=mean(producao^(1/3)),
            n=length(producao))
str(db)

m1 <- lm(producao~repeticao+familia, db, weight=n)
par(mfrow=c(2,2)); plot(m1); layout(1) # não precisa transformar

m2 <- lm(rproducao~repeticao+familia, db, weight=n)
par(mfrow=c(2,2)); plot(m2); layout(1) # com a transformada

anova(m1)
anova(m2)  # não são exatamente iguais por causa da tranformação seguido de média
summary(m0)

summary(m2)

#------------------------------------------------------------------------------------------
# médias ajustadas

pm <- as.data.frame(popMeans(m2, effect="familia"))
pm

#------------------------------------------------------------------------------------------
# contrastes entre as médias ajustadas

source("funcoes.R")

pmc <- popMatrix(m2, effect="familia")

allcontr <- apc(pmc)

c0 <- summary(glht(m2, linfct=allcontr), test=adjusted(type="fdr"))
c0

c0$focus <- "familia"
cld(c0)

#------------------------------------------------------------------------------------------
# colocando resultados em um gráfico

pm$familia <- factor(pm$familia, levels=levels(db$familia))
pm$cld <- cld(c0)$mcletters$Letters

segplot(reorder(familia, Estimate)~Lower+Upper, centers=Estimate, data=pm,
        ylab="Familia", xlab="Raíz cúbica da produção",
        draw.bands=FALSE, segments.fun=panel.arrows, ends="both",
        angle=90, length=1, unit="mm",
        panel=function(x, y, z, centers, subscripts, ...){
          panel.segplot(x, y, z, centers=centers, subscripts=subscripts, ...)
          panel.text(centers, z,
                     labels=paste(format(centers, digits=3), pm$cld[subscripts]),
                     pos=2)
        })

#------------------------------------------------------------------------------------------
# limpa a memória e importa dados

rm(list=ls())
ls()

da <- read.table("http://www.leg.ufpr.br/~walmes/data/mudas.txt", header=TRUE, sep=";")
str(da)
da$bloc <- factor(da$bloc)
da <- na.omit(da)
str(da)

#------------------------------------------------------------------------------------------
# ver os dados

xtabs(~trat+bloc, da) # medidas por parcela

# Este é um caso em que começou-se com 20 plantas por parcela e algumas morreram.
# Para-se fazer a análise deve-se assumir que as perdas foram ao acaso
# (MAR: missing at random) e não podem de forma alguma estarem associadas aos níveis
# dos fatores pois enviesaria conclusões.

# psr: peso seco de raizes
xyplot(psr~trat, groups=bloc, data=da)
xyplot(psr~trat|bloc, groups=rep, data=da)

#------------------------------------------------------------------------------------------
# ajuste do modelo com todas as observações

m0 <- lm(psr~bloc+trat+bloc:trat, data=da) # errado, para ver resíduos
par(mfrow=c(2,2)); plot(m0); layout(1) # análise dos pressupostos, hetero de var

MASS::boxcox(m0) # indica raiz cúbica para transformação

m0 <- lm(psr^(1/3)~bloc+trat+bloc:trat, data=da) # transformada
par(mfrow=c(2,2)); plot(m0); layout(1) # análise dos pressupostos, ok

m0 <- aov(psr^(1/3)~bloc+trat+Error(bloc:trat), data=da) # correto

summary(m0)    # quadro de análise de variância

#------------------------------------------------------------------------------------------
# fazendo a análise com as médias e os pesos

db <- ddply(da, .(bloc, trat), summarise,
            psr=mean(psr), rpsr=mean(psr^(1/3)),
            n=length(psr))
str(db)

m1 <- lm(psr~bloc+trat, db, weight=n)
par(mfrow=c(2,2)); plot(m1); layout(1) # não precisa transformar

m2 <- lm(rpsr~bloc+trat, db, weight=n)
par(mfrow=c(2,2)); plot(m2); layout(1) # com a transformada

anova(m1)
anova(m2)  # não são exatamente iguais por causa da tranformação seguido de média
summary(m0)

summary(m2)

#------------------------------------------------------------------------------------------
# médias ajustadas

pm <- as.data.frame(popMeans(m2, effect="trat"))
pm

#------------------------------------------------------------------------------------------
# contrastes entre as médias ajustadas

source("funcoes.R")

pmc <- popMatrix(m2, effect="trat")

allcontr <- apc(pmc)

c0 <- summary(glht(m2, linfct=allcontr), test=adjusted(type="fdr"))
c0

c0$focus <- "trat"
cld(c0)

#------------------------------------------------------------------------------------------
# colocando resultados em um gráfico

pm$trat <- factor(pm$trat, levels=levels(db$trat))
pm$cld <- cld(c0)$mcletters$Letters

segplot(reorder(trat, Estimate)~Lower+Upper, centers=Estimate, data=pm,
        ylab="Substrato", xlab="Raíz cúbica do peso seco de raízes",
        draw.bands=FALSE, segments.fun=panel.arrows, ends="both",
        angle=90, length=1, unit="mm",
        panel=function(x, y, z, centers, subscripts, ...){
          panel.segplot(x, y, z, centers=centers, subscripts=subscripts, ...)
          panel.text(centers, z,
                     labels=paste(format(centers, digits=3), pm$cld[subscripts]),
                     pos=2)
        })

#------------------------------------------------------------------------------------------
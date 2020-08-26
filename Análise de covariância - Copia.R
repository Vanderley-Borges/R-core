Análise de covariância em experimentos.
#==========================================================================================

#------------------------------------------------------------------------------------------
# sobre o modelo atribuído ao experimento
# suposição distribucional: Y|x ~ Normal(\mu(x), \sigma)
# suposição funcional:      \mu(x) =

#------------------------------------------------------------------------------------------
# definições da sessão

require(lattice)
require(agricolae)
require(contrast)
require(multcomp)
require(doBy)
require(latticeExtra)
require(ExpDes)

#------------------------------------------------------------------------------------------
# lendo arquivos de dados

da <- read.table("http://www.leg.ufpr.br/~walmes/data/ramalho_consorcio.txt",
                 header=TRUE, sep="\t", colClasses=rep(c("factor",NA),c(4,2)))
str(da)

#------------------------------------------------------------------------------------------
# ver os dados

xtabs(~consorcio+bloco, da)
xtabs(~milho+feijao, da)    # é um fatorial
xtabs(~estande, da)         # 20 é a moda

xyplot(producao~consorcio, groups=bloco, data=da)
xyplot(producao~estande|bloco, groups=consorcio, data=da)

xyplot(producao~milho|feijao, groups=bloco, data=da,
       panel=function(x, y, subscripts, ...){
         panel.xyplot(x, y, subscripts=subscripts, ...)
         panel.text(x, y, da$estande[subscripts], pos=4, cex=0.6)
       })

#------------------------------------------------------------------------------------------
# ajuste do modelo

m0 <- lm(producao~bloco+estande+milho*feijao, data=da)
par(mfrow=c(2,2)); plot(m0); layout(1)

anova(m0)
summary(m0)

m1 <- lm(producao~bloco+estande+milho+feijao, data=da)
par(mfrow=c(2,2)); plot(m1); layout(1)
anova(m1)

#------------------------------------------------------------------------------------------
# médias ajustadas

pm <- as.data.frame(popMeans(m1, effect="milho", at=list(feijao="Carioca", estande=20)))
pm

#------------------------------------------------------------------------------------------
# contrastes entre as médias ajustadas

source("funcoes.R")

pmc <- popMatrix(m1, effect="milho", at=list(feijao="Carioca", estande=20))

allcontr <- apc(pmc)

c0 <- summary(glht(m1, linfct=allcontr), test=adjusted(type="single-step"))
c0

c0$focus <- "milho"
cld(c0)

#------------------------------------------------------------------------------------------
# colocando resultados em um gráfico

pm$milho <- factor(pm$milho, levels=levels(da$milho))
pm$cld <- cld(c0)$mcletters$Letters

segplot(reorder(milho, Estimate)~Lower+Upper, centers=Estimate, data=pm,
        ylab="Cultivar de milho", xlab="Produção",
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

da <- read.table("http://www.leg.ufpr.br/~walmes/data/ramalho_p2o5.txt",
                 header=TRUE, sep="\t", colClasses=rep(c("factor","numeric"),c(2,2)))
str(da)

#------------------------------------------------------------------------------------------
# ver os dados

xtabs(~cultivar+bloco, da)
summary(da$p2o5)

xyplot(producao~cultivar, groups=bloco, data=da)
xyplot(producao~p2o5|bloco, groups=cultivar, data=da)
xyplot(p2o5~cultivar|bloco, groups=cultivar, data=da)

xyplot(producao~cultivar|bloco, data=da,
       panel=function(x, y, subscripts, ...){
         panel.xyplot(x, y, subscripts=subscripts, ...)
         panel.text(x, y, da$p2o5[subscripts], pos=4, cex=0.6)
       })

#------------------------------------------------------------------------------------------
# ajuste do modelo

m0 <- lm(producao~bloco+p2o5+cultivar, data=da)
par(mfrow=c(2,2)); plot(m0); layout(1)

anova(m0)
summary(m0)

#------------------------------------------------------------------------------------------
# médias ajustadas

pm <- as.data.frame(popMeans(m0, effect="cultivar", at=list(p2o5=11)))
pm

#------------------------------------------------------------------------------------------
# contrastes entre as médias ajustadas

source("funcoes.R")

pmc <- popMatrix(m0, effect="cultivar", at=list(p2o5=11))

allcontr <- apc(pmc)

c0 <- summary(glht(m0, linfct=allcontr), test=adjusted(type="fdr"))
c0

c0$focus <- "cultivar"
cld(c0)

#------------------------------------------------------------------------------------------
# colocando resultados em um gráfico

pm$cultivar <- factor(pm$cultivar, levels=levels(da$cultivar))
pm$cld <- cld(c0)$mcletters$Letters

segplot(reorder(cultivar, Estimate)~Lower+Upper, centers=Estimate, data=pm,
        ylab="Cultivar de milho", xlab="Produção",
        draw.bands=FALSE, segments.fun=panel.arrows, ends="both",
        angle=90, length=1, unit="mm",
        panel=function(x, y, z, centers, subscripts, ...){
          panel.segplot(x, y, z, centers=centers, subscripts=subscripts, ...)
          panel.text(centers, z,
                     labels=paste(format(centers, digits=4), pm$cld[subscripts]),
                     pos=3)
        })

#------------------------------------------------------------------------------------------
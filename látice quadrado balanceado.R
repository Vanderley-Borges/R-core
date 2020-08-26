Análise de experimento em delineamento de látice quadrado balanceado e fator
# de níveis categóricos (balanceado), resposta contínua, blocos aleatórios.
#==========================================================================================

#------------------------------------------------------------------------------------------
# sobre o modelo atribuído ao experimento
# suposição distribucional: Y|x ~ Normal(\mu(x), \sigma)
# suposição funcional:      \mu(x) = \mu+\alpha_i+b_ij+\tau_k  (i=2,...; j=2,..., k=2,...)
#                           b ~ Normal(0, sigma_b)

#------------------------------------------------------------------------------------------
# definições da sessão

require(lattice)
require(agricolae)
require(contrast)
require(multcomp)
require(doBy)
require(latticeExtra)
require(ExpDes)
require(nlme)

#------------------------------------------------------------------------------------------
# lendo arquivos de dados

da <- read.table("http://www.leg.ufpr.br/~walmes/data/ramalho_sorgo1.txt",
                 header=TRUE, sep="\t", colClasses=rep(c("factor","numeric"),c(3,1)))
str(da)

#------------------------------------------------------------------------------------------
# ver os dados

xtabs(~repeticao+bloco, da)
xtabs(~repeticao+cultivar, da)

xyplot(producao~cultivar, groups=bloco, data=da)
xyplot(producao~cultivar|repeticao, groups=bloco, data=da)

#------------------------------------------------------------------------------------------
# ajuste do modelo de efeitos fixos para bloco dentro de repetição

m0 <- lm(producao~repeticao/bloco+cultivar, data=da)
par(mfrow=c(2,2)); plot(m0); layout(1)

anova(m0) # cultivar não ajustado para blocos

m0 <- lm(terms(producao~repeticao/bloco+cultivar, keep.order=TRUE), data=da)
anova(m0) # ordem adequada, cultivar ajustado para blocos

#------------------------------------------------------------------------------------------
# médias ajustadas

pm <- as.data.frame(popMeans(m0, effect="cultivar"))
pm

#------------------------------------------------------------------------------------------
# contrastes entre as médias ajustadas

source("funcoes.R")

pmc <- popMatrix(m0, effect="cultivar")

allcontr <- apc(pmc)

help(adjusted, help_type="html")

c0 <- summary(glht(m0, linfct=allcontr), test=adjusted(type="none"))
c0

c0$focus <- "cultivar"
cld(c0, level=0.01)

#------------------------------------------------------------------------------------------
# colocando resultados em um gráfico

pm$cultivar <- factor(pm$cultivar, levels=levels(da$cultivar))
pm$cld <- cld(c0, level=0.01)$mcletters$Letters

segplot(reorder(cultivar, Estimate)~Lower+Upper, centers=Estimate, data=pm,
        ylab="Cultivar", xlab="Produção",
        draw.bands=FALSE, segments.fun=panel.arrows, ends="both",
        angle=90, length=1, unit="mm",
        panel=function(x, y, z, centers, subscripts, ...){
          panel.segplot(x, y, z, centers=centers, subscripts=subscripts, ...)
          panel.text(centers, z,
                     labels=paste(format(centers, digits=3), pm$cld[subscripts]),
                     pos=3)
        })

#------------------------------------------------------------------------------------------
# ajuste do modelo de efeito aleatório de bloco

da$blocoin <- with(da, interaction(repeticao, bloco)) # cria bloco dentro de repetição

mm0 <- lme(producao~repeticao+cultivar, random=~1|blocoin, da) # repeticão como fixo

plot(mm0) # resíduos e ajustados
qqnorm(residuals(mm0, type="pearson")) # normalidade dos resíduos
qqnorm(ranef(mm0)[[1]])                # normalidade dos efeitos de bloco

anova(mm0)   # quadro de teste de Wald
summary(mm0) # estimativa dos parâmetros

#------------------------------------------------------------------------------------------
# médias ajustas

pmm <- as.data.frame(popMeans(mm0, effect="cultivar")) # intervalos individuais de 95%
pmm

#------------------------------------------------------------------------------------------
# contraste entre médias ajustadas

pmmc <- popMatrix(lm(producao~repeticao+cultivar, da), effect="cultivar")
str(pmmc)

allcontr <- apc(pmmc)

c0 <- summary(glht(mm0, linfct=allcontr), test=adjusted(type="none"))
c0

c0$focus <- "cultivar"

#------------------------------------------------------------------------------------------
# colocar no gráfico

pmm$cultivar <- factor(pmm$cultivar, levels=levels(da$cultivar))
pmm$cld <- cld(c0, level=0.01)$mcletters$Letters

segplot(reorder(cultivar, Estimate)~Lower+Upper, centers=Estimate, data=pmm,
        ylab="Cultivar", xlab="Produção",
        draw.bands=FALSE, segments.fun=panel.arrows, ends="both",
        angle=90, length=1, unit="mm",
        panel=function(x, y, z, centers, subscripts, ...){
          panel.segplot(x, y, z, centers=centers, subscripts=subscripts, ...)
          panel.text(centers, z,
                     labels=paste(format(centers, digits=3), pmm$cld[subscripts]),
                     pos=3)
        })

# por que as letras ficam confusas?

#------------------------------------------------------------------------------------------
# limpa a memória e importa dados

rm(list=ls())
ls()

da <- read.table("http://www.leg.ufpr.br/~walmes/data/ramalho_milho.txt",
                 header=TRUE, sep="\t", colClasses=rep(c("factor","numeric"),c(3,1)))
str(da)

#------------------------------------------------------------------------------------------
# ver os dados

xtabs(~repeticao+bloco, da)
xtabs(~repeticao+cultivar, da)

xyplot(producao~cultivar, groups=bloco, data=da)
xyplot(producao~cultivar|repeticao, groups=bloco, data=da)

#------------------------------------------------------------------------------------------
# ajuste do modelo de efeito aleatório de bloco

da$blocoin <- with(da, interaction(repeticao, bloco)) # cria bloco dentro de repetição

mm0 <- lme(producao~repeticao+cultivar, random=~1|blocoin, da) # repeticão como fixo

plot(mm0) # resíduos e ajustados
qqnorm(residuals(mm0, type="pearson")) # normalidade dos resíduos
qqnorm(ranef(mm0)[[1]])                # normalidade dos efeitos de bloco

anova(mm0)   # quadro de teste de Wald
summary(mm0) # estimativa dos parâmetros

#------------------------------------------------------------------------------------------
# médias ajustas

pmm <- as.data.frame(popMeans(mm0, effect="cultivar")) # intervalos individuais de 95%
pmm

#------------------------------------------------------------------------------------------
# contraste entre médias ajustadas

source("funcoes.R")

pmmc <- popMatrix(lm(producao~repeticao+cultivar, da), effect="cultivar")
str(pmmc)

allcontr <- apc(pmmc)

c0 <- summary(glht(mm0, linfct=allcontr), test=adjusted(type="fdr"))
c0

c0$focus <- "cultivar"

#------------------------------------------------------------------------------------------
# colocar no gráfico

pmm$cultivar <- factor(pmm$cultivar, levels=levels(da$cultivar))
pmm$cld <- cld(c0)$mcletters$Letters

segplot(reorder(cultivar, Estimate)~Lower+Upper, centers=Estimate, data=pmm,
        ylab="Cultivar", xlab="Produção",
        draw.bands=FALSE, segments.fun=panel.arrows, ends="both",
        angle=90, length=1, unit="mm",
        panel=function(x, y, z, centers, subscripts, ...){
          panel.segplot(x, y, z, centers=centers, subscripts=subscripts, ...)
          panel.text(centers, z,
                     labels=paste(format(centers, digits=3), pmm$cld[subscripts]),
                     pos=2, cex=0.8)
        })

#------------------------------------------------------------------------------------------
# limpa a memória e importa dados

rm(list=ls())
ls()

da <- read.table("http://www.leg.ufpr.br/~walmes/data/ramalho_sorgo2.txt",
                 header=TRUE, sep="\t", colClasses=rep(c("factor","numeric"),c(3,1)))
str(da)

#------------------------------------------------------------------------------------------
# ver os dados

xtabs(~repeticao+bloco, da)
xtabs(~repeticao+cultivar, da)

xyplot(producao~cultivar, groups=bloco, data=da)
xyplot(producao~cultivar|repeticao, groups=bloco, data=da)

#------------------------------------------------------------------------------------------
# ajuste do modelo de efeito aleatório de bloco

da$blocoin <- with(da, interaction(repeticao, bloco)) # cria bloco dentro de repetição

mm0 <- lme(producao~repeticao+cultivar, random=~1|blocoin, da) # repeticão como fixo

plot(mm0) # resíduos e ajustados
qqnorm(residuals(mm0, type="pearson")) # normalidade dos resíduos
qqnorm(ranef(mm0)[[1]])                # normalidade dos efeitos de bloco

anova(mm0)   # quadro de teste de Wald
summary(mm0) # estimativa dos parâmetros

#------------------------------------------------------------------------------------------
# médias ajustas

pmm <- as.data.frame(popMeans(mm0, effect="cultivar")) # intervalos individuais de 95%
pmm

#------------------------------------------------------------------------------------------
# contraste entre médias ajustadas

source("funcoes.R")

pmmc <- popMatrix(lm(producao~repeticao+cultivar, da), effect="cultivar")
str(pmmc)

c0 <- confint(glht(mm0, linfct=pmmc)) # intervalos de cobertura global de 95%
str(c0)
str(c0$confint)
pmm <- cbind(cultivar=levels(da$cultivar), as.data.frame(c0$confint))

allcontr <- apc(pmmc)

c1 <- summary(glht(mm0, linfct=allcontr), test=adjusted(type="bonferroni"))
c1

c1$focus <- "cultivar"

#------------------------------------------------------------------------------------------
# colocar no gráfico

pmm$cld <- cld(c1)$mcletters$Letters

segplot(reorder(cultivar, Estimate)~lwr+upr, centers=Estimate, data=pmm,
        ylab="Cultivar", xlab="Produção",
        draw.bands=FALSE, segments.fun=panel.arrows, ends="both",
        angle=90, length=1, unit="mm",
        panel=function(x, y, z, centers, subscripts, ...){
          panel.segplot(x, y, z, centers=centers, subscripts=subscripts, ...)
          panel.text(centers, z,
                     labels=paste(format(centers, digits=3), pmm$cld[subscripts]),
                     pos=2, cex=0.8)
        })

#------------------------------------------------------------------------------------------
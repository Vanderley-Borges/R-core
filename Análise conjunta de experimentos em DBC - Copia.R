 Análise conjunta de experimentos em DBC, fator de níveis categóricos (balanceado),
# resposta contínua, blocos fixos e aleatórios.
#==========================================================================================

#------------------------------------------------------------------------------------------
# sobre o modelo atribuído ao experimento
# suposição distribucional: Y|x ~ Normal(\mu(x), \sigma)
# suposição funcional:      \mu(x) = \mu+\alpha_i+\tau_j  (i=2,...; j=2,...)

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

da <- read.table("http://www.leg.ufpr.br/~walmes/data/ramalho_feijao2.txt",
                 header=TRUE, sep="\t", colClasses=rep(c("factor","numeric"),c(3,1)))
da
str(da)

#------------------------------------------------------------------------------------------
# ver os dados

xtabs(~bloco+linhagem+local, da) # mesmo experimento nos dois locais

xyplot(producao~linhagem|local, groups=bloco, data=da)

#------------------------------------------------------------------------------------------
# análise do DBC separado por local, já fazendo de forma automática

db <- split(da, f=da$local)
class(db)

m0 <- lapply(db, lm, formula=producao~bloco+linhagem) # ajusta pros dois

par(mfrow=c(2,4))
lapply(m0, plot) # análise de resíduos
layout(1)

res <- do.call(c, lapply(m0, residuals))
qqmath(~res, groups=da$local) # parecem ter mesma variância

#------------------------------------------------------------------------------------------
# análise conjunta

m1 <- lm(producao~local/bloco+local*linhagem, data=da)
par(mfrow=c(2,2)); plot(m1); layout(1)

anova(m1) # fora de ordem

m1 <- lm(terms(producao~local/bloco+local*linhagem, keep.order=TRUE), data=da)
anova(m1) # assume-se que todos os termos são de efeito fixo
          # interação local:linhagem, desdobrar

m2 <- aov(producao~local+linhagem+Error(local:(bloco+linhagem)), data=da)
summary(m2) # testes F corretos para termos que interagem com local

#------------------------------------------------------------------------------------------
# obtendo as médias ajustadas

pm <- popMeans(m1, effect=c("linhagem", "local"))
popMeans(m1, effect=c("linhagem"), at=list(local="Lavras")) # separando o resultado

#------------------------------------------------------------------------------------------
# contrastes entre as médias ajustadas

source("funcoes.R")

pmc1 <- popMatrix(m1, effect="linhagem", at=list(local="Lavras"))
pmc2 <- popMatrix(m1, effect="linhagem", at=list(local="Patos de Minas"))

allc1 <- apc(pmc1)
allc2 <- apc(pmc2)

c1 <- summary(glht(m1, linfct=allc1), test=adjusted(type="fdr"))
c2 <- summary(glht(m1, linfct=allc2), test=adjusted(type="fdr"))

c1$focus <- c2$focus <- "linhagem"
cld(c1)
cld(c2)

#------------------------------------------------------------------------------------------
# colocando resultados em um gráfico

pm$linhagem <- factor(pm$linhagem, levels=levels(da$linhagem))
pm$local <- factor(pm$local, levels=levels(da$local))
pm$cld <- c(cld(c1)$mcletters$Letters, cld(c2)$mcletters$Letters)

segplot(reorder(linhagem, Estimate)~Lower+Upper|local, centers=Estimate, data=pm,
        ylab="Linhagem", xlab="Produção", layout=c(1,2),
        draw.bands=FALSE, segments.fun=panel.arrows, ends="both",
        angle=90, length=1, unit="mm",
        panel=function(x, y, z, centers, subscripts, ...){
          panel.segplot(x, y, z, centers=centers, subscripts=subscripts, ...)
          panel.text(centers[subscripts], z[subscripts],
                     labels=paste(format(centers, digits=3), pm$cld[subscripts]),
                     pos=2, cex=0.8)
        })

#------------------------------------------------------------------------------------------
# ajuste do modelo de efeito aleatório de bloco

da$blocoin <- with(da, interaction(local, bloco)) # cria bloco dentro de local

mm0 <- lme(producao~local+linhagem+local:linhagem, random=~1|blocoin, da)

plot(mm0) # resíduos e ajustados
qqnorm(residuals(mm0, type="pearson")) # normalidade dos resíduos
qqnorm(ranef(mm0)[[1]])                # normalidade dos efeitos de bloco

anova(mm0)   # quadro de teste de Wald
summary(mm0) # estimativa dos parâmetros

#------------------------------------------------------------------------------------------
# médias ajustas

#pmm <- as.data.frame(popMeans(mm0, effect=c("linhagem","local"))) # intervalos de 95%
#pmm

X <- popMatrix(lm(formula(mm0), da), effect=c("linhagem","local"))
str(X)

pmm <- confint(glht(mm0, linfct=X), calpha=univariate_calpha())
plot(pmm)

str(pmm)
pmm <- cbind(data.frame(sapply(attr(X, "grid"), factor)), as.data.frame(pmm$confint))
names(pmm)[ncol(pmm)-1:0] <- c("Lower","Upper")
pmm

#------------------------------------------------------------------------------------------
# contraste entre médias ajustadas

pmmc1 <- popMatrix(lm(producao~local*linhagem, da),
                   effect="linhagem", at=list(local="Lavras"))
pmmc2 <- popMatrix(lm(producao~local*linhagem, da),
                   effect="linhagem", at=list(local="Patos de Minas"))

allc1 <- apc(pmmc1)
allc2 <- apc(pmmc2)

c1 <- summary(glht(mm0, linfct=allc1), test=adjusted(type="fdr"))
c2 <- summary(glht(mm0, linfct=allc2), test=adjusted(type="fdr"))

c1$focus <- c2$focus <- "linhagem"
cld(c1)
cld(c2)

#------------------------------------------------------------------------------------------
# colocando resultados em um gráfico

## pmm$linhagem <- factor(pmm$linhagem, levels=levels(da$linhagem))
## pmm$local <- factor(pmm$local, levels=levels(da$local))
pmm$cld <- c(cld(c1)$mcletters$Letters, cld(c2)$mcletters$Letters)

segplot(reorder(linhagem, Estimate)~Lower+Upper|local, centers=Estimate, data=pmm,
        ylab="Linhagem", xlab="Produção", layout=c(1,2),
        draw.bands=FALSE, segments.fun=panel.arrows, ends="both",
        angle=90, length=1, unit="mm",
        panel=function(x, y, z, centers, subscripts, ...){
          panel.segplot(x, y, z, centers=centers, subscripts=subscripts, ...)
          panel.text(centers[subscripts], z[subscripts],
                     labels=paste(format(centers, digits=3), pmm$cld[subscripts]),
                     pos=2, cex=0.8)
        })

#------------------------------------------------------------------------------------------
# limpa a memória e importa dados

rm(list=ls())
ls()

da <- read.table("http://www.leg.ufpr.br/~walmes/data/ramalho_feijao3.txt",
                 header=TRUE, sep="\t", colClasses=rep(c("factor","numeric"),c(3,1)))
str(da)

#------------------------------------------------------------------------------------------
# ver os dados

xtabs(~bloco+populacao+geracao, da) # diferente número de blocos

xyplot(producao~populacao|geracao, groups=bloco, data=da)

#------------------------------------------------------------------------------------------
# análise do DBC separado por local, já fazendo de forma automática

db <- split(da, f=da$geracao)
class(db)

m0 <- lapply(db, lm, formula=producao~bloco+populacao) # ajusta pros dois

par(mfcol=c(2,3))
lapply(m0, plot, which=c(2,3)) # análise de resíduos
layout(1)

res <- do.call(c, lapply(m0, residuals))
qqmath(~res, groups=da$geracao) # parecem ter mesma variância

lapply(m0, anova)

#------------------------------------------------------------------------------------------
# análise conjunta

m1 <- lm(producao~geracao/bloco+geracao*populacao, data=da)
par(mfrow=c(2,2)); plot(m1); layout(1)

anova(m1) # fora de ordem

m1 <- lm(terms(producao~geracao/bloco+geracao*populacao, keep.order=TRUE), data=da)
anova(m1) # assume-se que todos os termos são de efeito fixo

m2 <- aov(producao~geracao+populacao+Error(geracao:(bloco+populacao)), data=da)
summary(m2) # testes F corretos para termos que interagem com local

#------------------------------------------------------------------------------------------
# ajuste do modelo de efeito aleatório de bloco

da$blocoin <- with(da, interaction(geracao, bloco)) # cria bloco dentro de geracao

mm0 <- lme(producao~geracao*populacao, random=~1|blocoin, da)

plot(mm0) # resíduos e ajustados
qqnorm(residuals(mm0, type="pearson")) # normalidade dos resíduos
qqnorm(ranef(mm0)[[1]])                # normalidade dos efeitos de bloco

anova(mm0)   # quadro de teste de Wald
summary(mm0) # estimativa dos parâmetros

mm1 <- lme(producao~geracao+populacao, random=~1|blocoin, da)

#------------------------------------------------------------------------------------------
# médias ajustas

## pmm <- as.data.frame(popMeans(mm1, effect="populacao")) # intervalos de 95%
## pmm

X <- popMatrix(lm(formula(mm1), da), effect="populacao")
str(X)

pmm <- confint(glht(mm1, linfct=X), calpha=univariate_calpha())
plot(pmm)

str(pmm)
pmm <- cbind(data.frame(sapply(attr(X, "grid"), factor)), as.data.frame(pmm$confint))
names(pmm)[ncol(pmm)-1:0] <- c("Lower","Upper")
pmm

#------------------------------------------------------------------------------------------
# contraste entre médias ajustadas

source("funcoes.R")

allc <- apc(X)

c0 <- summary(glht(mm1, linfct=allc), test=adjusted(type="fdr"))

c0$focus <- "populacao"
cld(c0)

#------------------------------------------------------------------------------------------
# colocando resultados em um gráfico

#pmm$populacao <- factor(pmm$populacao, levels=levels(da$populacao))
pmm$cld <- cld(c0)$mcletters$Letters

segplot(reorder(populacao, Estimate)~Lower+Upper, centers=Estimate, data=pmm,
        ylab="Linhagem", xlab="Produção",
        draw.bands=FALSE, segments.fun=panel.arrows, ends="both",
        angle=90, length=1, unit="mm",
        panel=function(x, y, z, centers, subscripts, ...){
          panel.segplot(x, y, z, centers=centers, subscripts=subscripts, ...)
          panel.text(centers[subscripts], z[subscripts],
                     labels=paste(format(centers, digits=3), pmm$cld[subscripts]),
                     pos=2, cex=0.8)
        })

#------------------------------------------------------------------------------------------
# limpa a memória e importa dados

rm(list=ls())
ls()

da <- read.table("http://www.leg.ufpr.br/~walmes/data/ramalho_arroz2.txt",
                 header=TRUE, sep="\t", colClasses=rep(c("factor","numeric"),c(3,1)))
str(da)

#------------------------------------------------------------------------------------------
# ver os dados

xtabs(~bloco+cultivar+local, da) # mesmo experimento

xyplot(producao~cultivar|local, groups=bloco, data=da)

#------------------------------------------------------------------------------------------
# análise do DBC separado por local, já fazendo de forma automática

db <- split(da, f=da$local)
class(db)

m0 <- lapply(db, lm, formula=producao~bloco+cultivar) # ajusta pros dois

par(mfcol=c(2,3))
lapply(m0, plot, which=c(2,3)) # análise de resíduos
layout(1)

res <- do.call(c, lapply(m0, residuals))
qqmath(~res, groups=da$local) # parecem ter mesma variância

lapply(m0, anova)

#------------------------------------------------------------------------------------------
# análise conjunta

m1 <- lm(producao~local/bloco+local*cultivar, data=da)
par(mfrow=c(2,2)); plot(m1); layout(1)

anova(m1) # fora de ordem

m1 <- lm(terms(producao~local/bloco+local*cultivar, keep.order=TRUE), data=da)
anova(m1) # assume-se que todos os termos são de efeito fixo

m2 <- aov(producao~local+cultivar+Error(local:(bloco+cultivar)), data=da)
summary(m2) # testes F corretos para termos que interagem com local

#------------------------------------------------------------------------------------------
# ajuste do modelo de efeito aleatório de bloco

da$blocoin <- with(da, interaction(local, bloco)) # cria bloco dentro de local

mm0 <- lme(producao~local*cultivar, random=~1|blocoin, da)

plot(mm0) # resíduos e ajustados
qqnorm(residuals(mm0, type="pearson")) # normalidade dos resíduos
qqnorm(ranef(mm0)[[1]])                # normalidade dos efeitos de bloco

anova(mm0)   # quadro de teste de Wald
summary(mm0) # estimativa dos parâmetros

#------------------------------------------------------------------------------------------
# médias ajustas

## pmm <- as.data.frame(popMeans(mm0, effect=c("cultivar","local"))) # intervalos de 95%
## pmm

X <- popMatrix(lm(formula(mm0), da), effect=c("cultivar","local"))
str(X)

pmm <- confint(glht(mm0, linfct=X), calpha=univariate_calpha())
plot(pmm)

str(pmm)
pmm <- cbind(data.frame(sapply(attr(X, "grid"), factor)), as.data.frame(pmm$confint))
names(pmm)[ncol(pmm)-1:0] <- c("Lower","Upper")
pmm

#------------------------------------------------------------------------------------------
# contraste entre médias ajustadas, automatizando com lapply()

lloc <- levels(da$local)

pmmc <- lapply(lloc,
               function(l){
                 popMatrix(lm(formula(mm0), da), effect="cultivar", at=list(local=l))
               })

source("funcoes.R")

allc <- lapply(pmmc, apc)
str(allc)

c0 <- lapply(allc,
             function(l){
               c0 <- summary(glht(mm0, linfct=l), test=adjusted(type="fdr"))
               c0$focus <- "cultivar"; c0
             })

let <- lapply(c0, function(l) cld(l)$mcletters$Letters)
let

#------------------------------------------------------------------------------------------
# colocando resultados em um gráfico

## pmm$cultivar <- factor(pmm$cultivar, levels=levels(da$cultivar))
## pmm$local <- factor(pmm$local, levels=levels(da$local))
pmm$cld <- do.call(c, let)

segplot(reorder(cultivar, Estimate)~Lower+Upper|local, centers=Estimate, data=pmm,
        ylab="Linhagem", xlab="Produção", layout=c(1,3),
        draw.bands=FALSE, segments.fun=panel.arrows, ends="both",
        angle=90, length=1, unit="mm",
        panel=function(x, y, z, centers, subscripts, ...){
          panel.segplot(x, y, z, centers=centers, subscripts=subscripts, ...)
          panel.text(centers[subscripts], z[subscripts],
                     labels=paste(format(centers, digits=3), pmm$cld[subscripts]),
                     pos=2, cex=0.8)
        })

#------------------------------------------------------------------------------------------
# melhorando o gráfico colocando cultivares em ordem dentro dos locais

pmm$caso <- with(pmm, interaction(cultivar, as.numeric(local)))
pmm$caso <- factor(pmm$caso, levels=levels(pmm$caso)[with(pmm, order(local, Estimate))])
levels(pmm$caso)

segplot(caso~Lower+Upper|local, centers=Estimate, data=pmm,
        ylab="Cultivar", xlab="Produção", scales=list(y="free"),
        draw.bands=FALSE, segments.fun=panel.arrows, ends="both",
        angle=90, length=1, unit="mm", layout=c(1,3),
        strip=FALSE, strip.left=TRUE,
        yscale.components=yscale.components.dropend,
        panel=function(x, y, z, centers, subscripts, ...){
          panel.segplot(x, y, z, centers=centers, subscripts=subscripts, ...)
          panel.text(centers[subscripts], z[subscripts],
                     labels=paste(format(centers, digits=3), pmm$cld[subscripts]),
                     pos=2, cex=0.8)
        })

#------------------------------------------------------------------------------------------
# limpa a memória e importa dados

rm(list=ls())
ls()

da <- read.table("http://www.leg.ufpr.br/~walmes/data/grupoexperimentos.txt",
                 header=TRUE, sep="\t", na.string=".")
str(da)
da$bl <- factor(da$bl)
head(da)

sum(is.na(da$rend)) # valores de rendimento não observados
da <- da[!is.na(da$rend),]
str(da)

#------------------------------------------------------------------------------------------
# ver os dados

xtabs(~gen+bl+local, da) # 4 locais, 3 bl por local, todos

xyplot(rend~gen|local, groups=bl, data=da)

#------------------------------------------------------------------------------------------
# análise do DBC separado por local, já fazendo de forma automática

db <- split(da, f=da$local)
class(db)

m0 <- lapply(db, lm, formula=rend~bl+gen) # ajusta pros 4

par(mfcol=c(2,4))
lapply(m0, plot, which=c(2,3)) # análise de resíduos
layout(1)

res <- do.call(c, lapply(m0, residuals))
qqmath(~res, groups=da$local) # parecem ter mesma variância

lapply(m0, anova)

qmr <- sapply(m0, deviance)/sapply(m0, df.residual)
range(qmr) # quadrados médios extremos

#------------------------------------------------------------------------------------------
# ajuste do modelo de efeito aleatório de bloco

da$blocoin <- with(da, interaction(local, bl)) # cria bloco dentro de local

mm0 <- lme(rend~local*gen, random=~1|blocoin, da)

plot(mm0) # resíduos e ajustados
qqnorm(residuals(mm0, type="pearson")) # normalidade dos resíduos
qqmath(~residuals(mm0, type="pearson"), groups=da$local) # separados por local
qqnorm(ranef(mm0)[[1]])                # normalidade dos efeitos de bloco

anova(mm0)   # quadro de teste de Wald
summary(mm0) # estimativa dos parâmetros

#------------------------------------------------------------------------------------------
# médias ajustas

## pmm <- as.data.frame(popMeans(mm0, effect=c("gen","local"))) # intervalos de 95%
## pmm

X <- popMatrix(lm(formula(mm0), da), effect=c("gen","local"))
str(X)

pmm <- confint(glht(mm0, linfct=X), calpha=univariate_calpha()) # cobertura individual 95%
## pmm <- confint(glht(mm0, linfct=X), calpha=qnorm(1-0.05/choose(nlevels(da$gen),2)))
plot(pmm)

str(pmm)
pmm <- cbind(data.frame(sapply(attr(X, "grid"), factor)), as.data.frame(pmm$confint))
names(pmm)[ncol(pmm)-1:0] <- c("Lower","Upper")
pmm

#------------------------------------------------------------------------------------------
# contraste entre médias ajustadas, automatizando com lapply()

lloc <- levels(da$local)

pmmc <- lapply(lloc,
               function(l){
                 popMatrix(lm(formula(mm0), da), effect="gen", at=list(local=l))
               })

source("funcoes.R")

allc <- lapply(pmmc, apc)
str(allc)

c0 <- lapply(allc,
             function(l){
               c0 <- summary(glht(mm0, linfct=l), test=adjusted(type="fdr"))
               c0$focus <- "gen"; c0
             })

#let <- lapply(c0, function(l) cld(l)$mcletters$Letters)
#let

#------------------------------------------------------------------------------------------
# colocando resultados em um gráfico

## pmm$gen <- factor(pmm$gen, levels=levels(da$gen))
## pmm$local <- factor(pmm$local, levels=levels(da$local))
## pmm$cld <- do.call(c, let)

segplot(reorder(gen, Estimate)~Lower+Upper|local, centers=Estimate, data=pmm,
        ylab="Genótipo", xlab="Rendimento", layout=c(1,4),
        draw.bands=FALSE, segments.fun=panel.arrows, ends="both",
        angle=90, length=1, unit="mm",
        panel=function(x, y, z, centers, subscripts, ...){
          panel.segplot(x, y, z, centers=centers, subscripts=subscripts, ...)
          ## panel.text(centers[subscripts], z[subscripts],
          ##            labels=paste(format(centers, digits=3), pmm$cld[subscripts]),
          ##            pos=2, cex=0.8)
        })

#------------------------------------------------------------------------------------------
# melhorando o gráfico colocando cultivares em ordem dentro dos locais

pmm$caso <- with(pmm, interaction(gen, as.numeric(local)))
pmm$caso <- factor(pmm$caso, levels=levels(pmm$caso)[with(pmm, order(local, Estimate))])
levels(pmm$caso)

segplot(caso~Lower+Upper|local, centers=Estimate, data=pmm,
        ylab="Genótipo", xlab="Rendimento",
        scales=list(y=list(relation="free", rot=0, tck=0.5)),
        draw.bands=FALSE, segments.fun=panel.arrows, ends="both",
        angle=90, length=1, unit="mm", layout=c(2,2),
        strip=FALSE, strip.left=TRUE,
        yscale.components=yscale.components.dropend,
        between=list(x=NULL, y=1),
        panel=function(x, y, z, centers, subscripts, ...){
          panel.abline(v=seq(500,6000,500), h=1:nlevels(pmm$caso), lty=3, col="gray70")
          panel.segplot(x, y, z, centers=centers, subscripts=subscripts, ...)
        })

#------------------------------------------------------------------------------------------
# limpa a memória e importa dados

rm(list=ls())
ls()

da <- read.table("http://www.leg.ufpr.br/~walmes/data/farms.txt",
                 header=TRUE, sep="\t", colClasses=rep(c("factor","numeric"),c(3,1)))
str(da)

#------------------------------------------------------------------------------------------
# ver os dados

xtabs(~block+trt+farm, da) # diferente número de blocos

xyplot(resp~trt|farm, groups=block, data=da, type="o")

#------------------------------------------------------------------------------------------
# análise do DBC separado por local, já fazendo de forma automática

db <- split(da, f=da$farm)
class(db)

m0 <- lapply(db, lm, formula=resp~block+trt) # ajusta pros dois

par(mfcol=c(2,4))
lapply(m0, plot, which=c(2)) # análise de resíduos
layout(1)

res <- do.call(c, lapply(m0, residuals))
qqmath(~res, groups=da$farm) # parecem ter mesma variância

lapply(m0, anova)

#------------------------------------------------------------------------------------------
# análise conjunta

m1 <- lm(resp~farm/block+farm*trt, data=da)
par(mfrow=c(2,2)); plot(m1); layout(1)

anova(m1) # fora de ordem

m1 <- lm(terms(resp~farm/block+farm*trt, keep.order=TRUE), data=da)
anova(m1) # assume-se que todos os termos são de efeito fixo

m2 <- aov(resp~farm+trt+Error(farm:(block+trt)), data=da)
summary(m2) # testes F corretos para termos que interagem com local

#------------------------------------------------------------------------------------------
# ajuste do modelo de efeito aleatório de block

da$blockin <- with(da, interaction(farm, block)) # cria block dentro de farm

mm0 <- lme(resp~farm*trt, random=~1|blockin, da)

plot(mm0) # resíduos e ajustados
qqnorm(residuals(mm0, type="pearson")) # normalidade dos resíduos
qqnorm(ranef(mm0)[[1]])                # normalidade dos efeitos de block

anova(mm0)   # quadro de teste de Wald
summary(mm0) # estimativa dos parâmetros

#------------------------------------------------------------------------------------------
# médias ajustas

## pmm <- as.data.frame(popMeans(mm0, effect=c("trt","farm"))) # intervalos de 95%
## pmm

X <- popMatrix(lm(formula(mm0), da), effect=c("trt","farm"))
str(X)

## summary(glht(mm0, linfct=X))
pmm <- confint(glht(mm0, linfct=X), calpha=univariate_calpha()) # cobertura individual 95%
## pmm <- confint(glht(mm0, linfct=X), calpha=qnorm(1-0.05/choose(nlevels(da$gen),2)))
plot(pmm)

str(pmm)
pmm <- cbind(data.frame(sapply(attr(X, "grid"), factor)), as.data.frame(pmm$confint))
names(pmm)[ncol(pmm)-1:0] <- c("Lower","Upper")
pmm

#------------------------------------------------------------------------------------------
# contraste entre médias ajustadas, automatizando com lapply()

lloc <- levels(da$farm)

pmmc <- lapply(lloc,
               function(l){
                 popMatrix(lm(formula(mm0), da), effect="trt", at=list(farm=l))
               })

source("funcoes.R")

allc <- lapply(pmmc, apc)
str(allc)

c0 <- lapply(allc,
             function(l){
               c0 <- summary(glht(mm0, linfct=l), test=adjusted(type="fdr"))
               c0$focus <- "gen"; c0
             })

let <- lapply(c0, function(l) cld(l)$mcletters$Letters)
let

#------------------------------------------------------------------------------------------
# colocando resultados em um gráfico

## pmm$trt <- factor(pmm$trt, levels=levels(da$trt))
## pmm$farm <- factor(pmm$farm, levels=levels(da$farm))
pmm$cld <- do.call(c, let)

segplot(reorder(trt, Estimate)~Lower+Upper|farm, centers=Estimate, data=pmm,
        ylab="Treat", xlab="Resp", layout=c(1,8),
        strip=FALSE, strip.left=TRUE,
        draw.bands=FALSE, segments.fun=panel.arrows, ends="both",
        angle=90, length=1, unit="mm",
        panel=function(x, y, z, centers, subscripts, ...){
          panel.segplot(x, y, z, centers=centers, subscripts=subscripts, ...)
          panel.text(centers[subscripts], z[subscripts],
                     labels=paste(format(centers, digits=3), pmm$cld[subscripts]),
                     adj=c(0.5,-0.5), cex=0.8)
        })

#------------------------------------------------------------------------------------------
# existe certa incompatibilidade entre nlme e lme4

detach(package:nlme)
sessionInfo()
search()
require(lme4)

#------------------------------------------------------------------------------------------
# modelo apenas com trt fixox

mm1 <- lmer(resp~trt+(1|farm)+(1|farm:block)+(1|farm:trt), data=da)
summary(mm1)

#------------------------------------------------------------------------------------------
# obtendo os BLUPs

form <- c(~trt, ~-1+farm, ~-1+trt:farm)
X <- do.call(cbind, lapply(form, model.matrix, data=da))
dim(X)
colnames(X)

ref <- ranef(mm1)
names(ref)
beta <- c(fixef(mm1), ref[["farm"]][[1]], ref[["farm:trt"]][[1]])
length(beta)

X <- unique(X)
dim(X)

X%*%beta # são os BLUPs (mas o erro padrão vem da onde?)

popMeans(mm1, effect="trt")

#------------------------------------------------------------------------------------------
# ajustando com a lmer() o modelo anterior

mm2 <- lmer(resp~farm*trt+(1|farm:block), data=da)
summary(mm2)

popMeans(mm2, effect=c("trt","farm"))

#------------------------------------------------------------------------------------------
# limpa a memória e importa dados

rm(list=ls())
ls()

da <- read.table("http://www.leg.ufpr.br/~walmes/data/dkb.txt",
                 header=TRUE, sep="\t", colClasses=rep(c("factor","numeric"),c(5,2)))
str(da)

# são 31 experimentos. Nem todos eles compartilham os mesmos tratamentos, mas ao todo
# são 10 sementes (cultivares) avaliadas e todos possuem sementes tratadas ou não. Nem
# todos os experimentos apresentam as mesmas respostas avaliadas. Em cada experimento foram
# avaliadas sempre duas sementes. Portanto, em cada experimento tem-se um fatorial 2x2.

with(da, tapply(semente, experimento, unique))
with(da, tapply(inseticida, experimento, unique))
with(da, tapply(experimento, semente, unique))

#------------------------------------------------------------------------------------------
# ver os dados

# não existe conectividade!!
xtabs(~semente+inseticida+experimento, da)

xyplot(producao~semente|experimento, groups=inseticida, data=da, type="o")
xyplot(producao~estande, data=da)

aux <- droplevels(subset(da, semente%in%levels(da$semente)[9:10]))
xtabs(~experimento+semente, aux)
da <- aux

#------------------------------------------------------------------------------------------
# análise do DBC do fatorial e estande separado por local, já fazendo de forma automática

db <- split(da, f=da$experimento)
class(db)

m0 <- lapply(db, lm, formula=producao~repeticao+estande+semente*inseticida)

res <- do.call(c, lapply(m0, residuals))
qqmath(~res, groups=da$experimento) # parecem ter mesma variância

qmr <- sapply(m0, deviance)/sapply(m0, df.residual)
sort(qmr)
range(qmr)

lapply(m0, anova)

#------------------------------------------------------------------------------------------
# análise conjunta com efeito aleatório de experimento/blocos, sem considerar estande

require(nlme)

reptin <- with(da, interaction(repeticao, experimento))

mm0 <- lme(producao~semente*inseticida, random=~1|experimento/repeticao, data=da)
mm0 <- lme(producao~experimento*semente*inseticida, random=~1|reptin, data=da)

plot(mm0) # resíduos e ajustados
qqnorm(residuals(mm0, type="pearson"))   # normalidade dos resíduos
qqnorm(ranef(mm0)[["experimento"]][[1]]) # normalidade dos efeitos de experimento
qqnorm(ranef(mm0)[["repeticao"]][[1]])   # normalidade dos efeitos de repeticao
qqnorm(ranef(mm0)[[1]])   # normalidade dos efeitos de repeticao

anova(mm0)   # quadro de teste de Wald
summary(mm0) # estimativa dos parâmetros

mm0 <- lme(producao~(experimento+semente+inseticida)^2-experimento:inseticida,
           random=~1|reptin, data=da)

#------------------------------------------------------------------------------------------
# médias ajustas

## pmm <- as.data.frame(popMeans(mm0, effect=c("semente"), at=list(estande=65000)))
## pmm

X <- popMatrix(lm(formula(mm0), da), effect=c("semente","inseticida","experimento"))
str(X)

## summary(glht(mm0, linfct=X))
pmm <- confint(glht(mm0, linfct=X), calpha=univariate_calpha()) # cobertura individual 95%
## pmm <- confint(glht(mm0, linfct=X), calpha=qnorm(1-0.05/choose(nlevels(da$gen),2)))
plot(pmm)

str(pmm)
pmm <- cbind(data.frame(sapply(attr(X, "grid"), factor)), as.data.frame(pmm$confint))
names(pmm)[ncol(pmm)-1:0] <- c("Lower","Upper")
pmm

#------------------------------------------------------------------------------------------
# contraste entre médias ajustadas, automatizando com lapply()

source("funcoes.R")

dim(X)
pmmc <- split(as.data.frame(X), f=pmm$experimento:pmm$inseticida)
pmmc <- lapply(pmmc, as.matrix)
str(pmmc)

allc <- lapply(pmmc, apc, lev=levels(da$semente))
str(allc)

c0 <- summary(glht(mm0, linfct=allc[[1]]), test=adjusted(type="fdr"))
c0$focus <- "semente"
c0

com <- as.data.frame(do.call(cbind, c0$test[3:6]))
com <- com[order(com$pvalues),]
com$sig <- cut(com$pvalues, c(0,0.01,0.05,0.1,1), labels=c("**","*",".","ns"), i=TRUE)
head(com)

cld(c0)$mcletters$Letters

#------------------------------------------------------------------------------------------
# colocando resultados em um gráfico

## pmm$semente <- factor(pmm$semente, levels=levels(da$semente))
pmm$cld <- cld(c0)$mcletters$Letters

segplot(reorder(semente, Estimate)~Lower+Upper|experimento:inseticida, centers=Estimate, data=pmm,
        ylab="Semente", xlab="Producao", layout=c(1,24), strip=FALSE,
        draw.bands=FALSE, segments.fun=panel.arrows, ends="both",
        angle=90, length=1, unit="mm",
        panel=function(x, y, z, centers, subscripts, ...){
          panel.segplot(x, y, z, centers=centers, subscripts=subscripts, ...)
          panel.text(centers[subscripts], z[subscripts],
                     labels=paste(format(centers, digits=3), pmm$cld[subscripts]),
                     adj=c(0.5,-0.5), cex=0.8)
        })

#------------------------------------------------------------------------------------------
# Porque dois intervalos que se sobrepõem até o ponto médio têm médias diferentes?

es.m <- with(da, tapply(estande, semente, mean))
es.m # estande médio para cada semente

x <- contrast(mm0, type="average",
         list(semente="DKB330", estande=70225, inseticida=levels(da$inseticida)))
x$X
str(x$X)
x$X%*%fixef(mm0)
sqrt(x$X%*%vcov(mm0)%*%t(x$X))

summary(mm0)

x <- contrast(mm0, #type="average",
              list(semente=levels(da$semente)[1],
                   estande=0, inseticida=levels(da$inseticida)[1]))
x$X%*%fixef(mm0)
sqrt(x$X%*%vcov(mm0)%*%t(x$X))

summary(mm0)$tTable

popMeans(mm0, effect=c("semente","inseticida"), at=list(estande=0))

#popMeans(mm0, effect=c("semente"), at=list(estande=70225))
##    beta0  Estimate Std.Error  t.value  DF Pr(>|t|)     Lower    Upper  semente estande
## 7      0 10266.014  372.3876 27.56809 603        0  9534.680 10997.35   DKB330   70225
##   Contrast     S.E.    Lower   Upper    t  df Pr(>|t|)
## 1 10266.04 1215.258 7884.174 12647.9 8.45 600        0

y <- popMatrix(lm(formula(mm0), da), effect="semente", at=list(estande=70225))
y[7,]

cbind(y[7,], c(x$X))
contrast(mm0, type="average",
         list(semente="DKB330", estande=65000, inseticida=levels(da$inseticida)))



pmm

pmm$semente <- factor(pmm$semente, levels=levels(da$semente))

segplot(reorder(semente, Estimate)~Lower+Upper, centers=Estimate, data=pmm,
        ylab="Semente", xlab="Producao",
        draw.bands=FALSE, segments.fun=panel.arrows, ends="both",
        angle=90, length=1, unit="mm",
        panel=function(x, y, z, centers, subscripts, ...){
          panel.segplot(x, y, z, centers=centers, subscripts=subscripts, ...)
          ## panel.text(centers[subscripts], z[subscripts],
          ##            labels=paste(format(centers, digits=3), pmm$cld[subscripts]),
          ##            adj=c(0.5,-0.5), cex=0.8)
        })
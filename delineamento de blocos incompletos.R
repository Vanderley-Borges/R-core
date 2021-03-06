#An�lise de experimento em delineamento de blocos incompletos casualizados (bic) e fator
# de n�veis categ�ricos (balanceado), resposta cont�nua, blocos fixos e aleat�rios.
#==========================================================================================

# para gerar o delineamento em blocos incompletos balanceados
# http://www.wekaleamstudios.co.uk/posts/generating-balanced-incomplete-block-designs-bibd/

#------------------------------------------------------------------------------------------
# sobre o modelo atribu�do ao experimento
# suposi��o distribucional: Y|x ~ Normal(\mu(x), \sigma)
# suposi��o funcional:      \mu(x) = \mu+\alpha_i+\tau_j  (i=2,...; j=2,...)          fixo

# suposi��o funcional:      \mu(x) = \mu+\alpha_i+b_j  (i=2,...; j=2,...)        aleat�rio
#                           b ~ Normal(0, sigma_b)

#------------------------------------------------------------------------------------------
# defini��es da sess�o

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

da <- read.table("http://www.leg.ufpr.br/~walmes/data/pimentel_bib3.txt",
                 header=TRUE, sep="\t")
str(da)
da <- transform(da, bloco=factor(bloco), variedade=factor(variedade))
str(da)

#------------------------------------------------------------------------------------------
# ver os dados

xtabs(~bloco+variedade, da) # cada bloco t�m 3 variedades, ao todo 5 variedades
xtabs(~variedade, da)       # 6 repeti��es de cada variedade

xyplot(y~variedade, groups=bloco, data=da)

#------------------------------------------------------------------------------------------
# ajuste do modelo de efeito fixo de bloco

m0 <- lm(y~bloco+variedade, da)

par(mfrow=c(2,2)); plot(m0); layout(1) # an�lise dos pressupostos

anova(m0)   # quadro de an�lise de vari�ncia
summary(m0) # estimativa dos par�metros

#------------------------------------------------------------------------------------------
# obtendo as m�dias ajustadas

pm <- as.data.frame(popMeans(m0, effect="variedade")) # IC com cobertura individual de 95%
pm # o erro padr�o igual devido � balanceamento/equilibrio e igual n�mero de repeti��es

#------------------------------------------------------------------------------------------
# comparando as m�dias ajustadas (allpairwise)

pmc <- popMatrix(m0, effect="variedade")
str(pmc)

source("funcoes.R")

allcontr <- apc(pmc)

summary(glht(m0, linfct=allcontr)) # sigle-step demora para muitas compara��es
summary(glht(m0, linfct=allcontr), test=adjusted(type="bonferroni"))
summary(glht(m0, linfct=allcontr), test=adjusted(type="fdr"))

# o resultado � o mesmo devido ao equilibrio, se perder parcela melhor o de cima
c0 <- summary(glht(m0, linfct=mcp(variedade="Tukey")))
c0
cld(c0) # compact letter display, recomendado por terem erros padr�es iguais

#------------------------------------------------------------------------------------------
# obtendo as letras

c1 <- summary(glht(m0, linfct=allcontr)) # sigle-step demora para muitas compara��es
cld(c1) # d� erro porque n�o tem valor para "focus"
c1$focus <- "variedade" # adiciona valor
cld(c1)

#------------------------------------------------------------------------------------------
# colocando no gr�fico

pm$variedade <- factor(pm$variedade, levels=levels(da$variedade))
pm$cld <- cld(c1)$mcletters$Letters

segplot(reorder(variedade, Estimate)~Lower+Upper, centers=Estimate, data=pm,
        xlab="y", ylab="Variedade",
        draw.bands=FALSE, segments.fun=panel.arrows, ends="both",
        angle=90, length=1, unit="mm",
        panel=function(x, y, z, centers, subscripts, ...){
          panel.segplot(x, y, z, centers=centers, subscripts=subscripts, ...)
          panel.text(centers, z, pm$cld[subscripts], pos=3)
          panel.text(centers, z, format(centers, digits=3), pos=1)
        })

#------------------------------------------------------------------------------------------
# ajuste do modelo de efeito aleat�rio de bloco

mm0 <- lme(y~variedade, random=~1|bloco, da)

plot(mm0) # res�duos e ajustados
qqnorm(residuals(mm0, type="pearson")) # normalidade dos res�duos
qqnorm(ranef(mm0)[[1]])                # normalidade dos efeitos de bloco

anova(mm0)   # quadro de teste de Wald
summary(mm0) # estimativa dos par�metros

#------------------------------------------------------------------------------------------
# m�dias ajustas

pmm <- as.data.frame(popMeans(mm0, effect="variedade"))
pmm

#------------------------------------------------------------------------------------------
# contraste entre m�dias ajustadas

pmmc <- popMatrix(lm(y~variedade, da), effect="variedade")
pmmc

allcontr <- apc(pmmc)

summary(glht(mm0, linfct=allcontr))

#------------------------------------------------------------------------------------------
# outros dados na mesma estrutura

# http://www.leg.ufpr.br/~walmes/data/tastebibd.txt
# http://www.leg.ufpr.br/~walmes/data/dietbibd.txt
# http://www.leg.ufpr.br/~walmes/data/monovinylbibd.txt
# http://www.leg.ufpr.br/~walmes/data/wheatbibd.txt
# http://www.leg.ufpr.br/~walmes/data/chunlibibd.txt

#------------------------------------------------------------------------------------------
# limpa a mem�ria e importa dados

rm(list=ls())
ls()

db <- read.table("http://www.leg.ufpr.br/~walmes/data/tastebibd.txt",
                 header=TRUE, sep="\t")
str(db)
db <- transform(db, block=factor(block), formulation=factor(formulation))
str(db)

#------------------------------------------------------------------------------------------
# ver os dados

xtabs(~block+formulation, db) # cada bloco t�m 3 formula��o, ao todo 5 variedades
xtabs(~formulation, db)       # 6 repeti��es de cada formula��o

xyplot(rating~formulation, groups=block, data=db, jitter.x=TRUE)

#------------------------------------------------------------------------------------------
# ajuste do modelo com efeito aleat�rio de bloco

mm0 <- lme(rating~formulation, random=~1|block, db)

plot(mm0) # res�duos e ajustados
qqnorm(residuals(mm0, type="pearson")) # normalidade dos res�duos
qqnorm(ranef(mm0)[[1]])                # normalidade dos efeitos de bloco

anova(mm0)   # quadro de teste de Wald
summary(mm0) # estimativa dos par�metros

#------------------------------------------------------------------------------------------
# m�dias ajustas

pmm <- as.data.frame(popMeans(mm0, effect="formulation"))
pmm

#------------------------------------------------------------------------------------------
# contraste entre m�dias ajustadas

pmmc <- popMatrix(lm(formula(mm0), db), effect="formulation")
pmmc

allcontr <- apc(pmmc)

c0 <- summary(glht(mm0, linfct=allcontr))
c0

c0$focus <- "formulation"
cld(c0)

#------------------------------------------------------------------------------------------
# colocando resultados em um gr�fico

pmm$formulation <- factor(pmm$formulation, levels=levels(db$formulation))
pmm$cld <- cld(c0)$mcletters$Letters

segplot(reorder(formulation, Estimate)~Lower+Upper, centers=Estimate, data=pmm,
        xlab="Satisfa��o", ylab="Formula��o",
        draw.bands=FALSE, segments.fun=panel.arrows, ends="both",
        angle=90, length=1, unit="mm",
        panel=function(x, y, z, centers, subscripts, ...){
          panel.segplot(x, y, z, centers=centers, subscripts=subscripts, ...)
          panel.text(centers, z,
                     labels=paste(format(centers, digits=3), pmm$cld[subscripts]), pos=3)
        })

#------------------------------------------------------------------------------------------
# limpa a mem�ria e importa dados

rm(list=ls())
ls()

db <- read.table("http://www.leg.ufpr.br/~walmes/data/dietbibd.txt",
                 header=TRUE, sep="\t")
str(db)
db <- na.omit(db)
str(db)

#------------------------------------------------------------------------------------------
# ver os dados

xtabs(~age+fat, db)     # cada bloco t�m 2 n�veis
xtabs(~fat, db)         # 4 repeti��es

xyplot(lipids~fat, groups=age, data=db, jitter.x=TRUE)

#------------------------------------------------------------------------------------------
# ajuste do modelo com efeito aleat�rio de bloco

mm0 <- lme(lipids~fat, random=~1|age, db)

plot(mm0) # res�duos e ajustados
qqnorm(residuals(mm0, type="pearson")) # normalidade dos res�duos
qqnorm(ranef(mm0)[[1]])                # normalidade dos efeitos de bloco

anova(mm0)   # quadro de teste de Wald
summary(mm0) # estimativa dos par�metros

#------------------------------------------------------------------------------------------
# m�dias ajustas

pmm <- as.data.frame(popMeans(mm0, effect="fat"))
pmm

#------------------------------------------------------------------------------------------
# contraste entre m�dias ajustadas

pmmc <- popMatrix(lm(formula(mm0), db), effect="fat")
pmmc

allcontr <- apc(pmmc)

c0 <- summary(glht(mm0, linfct=allcontr))
c0

c0$focus <- "fat"
cld(c0)

#------------------------------------------------------------------------------------------
# colocando resultados em um gr�fico

pmm$fat <- factor(pmm$fat, levels=levels(db$fat))
pmm$cld <- cld(c0)$mcletters$Letters

segplot(reorder(fat, Estimate)~Lower+Upper, centers=Estimate, data=pmm,
        xlab="Lip�dios", ylab="Gordura",
        draw.bands=FALSE, segments.fun=panel.arrows, ends="both",
        angle=90, length=1, unit="mm",
        panel=function(x, y, z, centers, subscripts, ...){
          panel.segplot(x, y, z, centers=centers, subscripts=subscripts, ...)
          panel.text(centers, z,
                     labels=paste(format(centers, digits=3), pmm$cld[subscripts]), pos=3)
        })

#------------------------------------------------------------------------------------------
# lendo dados

rm(list=ls())
ls()

db <- read.table("http://www.leg.ufpr.br/~walmes/data/monovinylbibd.txt",
                 header=TRUE, sep="\t")
str(db)
db <- transform(db, run=factor(run), psi=factor(psi))
str(db)

#------------------------------------------------------------------------------------------
# ver os dados

xtabs(~run+psi, db)     # cada bloco t�m 3 n�veis
xtabs(~psi, db)         # 6 repeti��es

xyplot(monovinyl~psi, groups=run, data=db, jitter.x=TRUE)

#------------------------------------------------------------------------------------------
# ajuste do modelo com efeito aleat�rio de bloco

mm0 <- lme(monovinyl~psi, random=~1|run, db)

plot(mm0) # res�duos e ajustados
qqnorm(residuals(mm0, type="pearson")) # normalidade dos res�duos
qqnorm(ranef(mm0)[[1]])                # normalidade dos efeitos de bloco

anova(mm0)   # quadro de teste de Wald
summary(mm0) # estimativa dos par�metros

#------------------------------------------------------------------------------------------
# m�dias ajustas

pmm <- as.data.frame(popMeans(mm0, effect="psi"))
pmm

#------------------------------------------------------------------------------------------
# contraste entre m�dias ajustadas

pmmc <- popMatrix(lm(formula(mm0), db), effect="psi")
pmmc

allcontr <- apc(pmmc)

c0 <- summary(glht(mm0, linfct=allcontr))
c0

c0$focus <- "psi"
cld(c0)

#------------------------------------------------------------------------------------------
# colocando resultados em um gr�fico

pmm$psi <- factor(pmm$psi, levels=levels(db$psi))
pmm$cld <- cld(c0)$mcletters$Letters

segplot(psi~Lower+Upper, centers=Estimate, data=pmm,
        ylab="Monovinyl", xlab="Psi", horizontal=FALSE,
        draw.bands=FALSE, segments.fun=panel.arrows, ends="both",
        angle=90, length=1, unit="mm",
        panel=function(x, y, z, centers, subscripts, ...){
          panel.segplot(x, y, z, centers=centers, subscripts=subscripts, ...)
          panel.text(z, centers,
                     labels=paste(format(centers, digits=3), pmm$cld[subscripts]),
                     adj=c(0.5,-0.5), srt=90)
        })

#------------------------------------------------------------------------------------------
# os n�veis do fator psi s�o cont�nuos, e mais interessante ajustar um modelo
# faremos uso de polin�mio

mm0 <- update(mm0, method="ML")

db$psi <- as.numeric(as.character(db$psi))

mm1 <- lme(monovinyl~poly(psi, 2), random=~1|run, data=db, method="ML")

anova(mm0, mm1) # compara a redu��o no modelo
summary(mm1)

#------------------------------------------------------------------------------------------
# fazendo a predi��o e colocando no gr�fico

pred <- data.frame(psi=seq(245, 550, by=5))
pred$y <- predict(mm1, newdata=pred, level=0)

xyplot(monovinyl~psi, groups=run, data=db)+
  as.layer(xyplot(y~psi, data=pred, type="l"))

#------------------------------------------------------------------------------------------
# lendo dados

rm(list=ls())
ls()

da <- read.table("http://www.leg.ufpr.br/~walmes/data/ramalho_biba.txt",
                 header=TRUE, sep="\t", colClasses=c("factor","factor","numeric"))
str(da)

#------------------------------------------------------------------------------------------
# ver os dados

xtabs(~bloco+populacao, da)
xtabs(~populacao, da)

xyplot(producao~populacao, groups=bloco, data=da, jitter.x=TRUE)

#------------------------------------------------------------------------------------------
# ajuste do modelo com efeito aleat�rio de bloco

mm0 <- lme(producao~populacao, random=~1|bloco, da)

plot(mm0) # res�duos e ajustados
qqnorm(residuals(mm0, type="pearson")) # normalidade dos res�duos
qqnorm(ranef(mm0)[[1]])                # normalidade dos efeitos de bloco

anova(mm0)   # quadro de teste de Wald
summary(mm0) # estimativa dos par�metros

#------------------------------------------------------------------------------------------
# m�dias ajustas

pmm <- as.data.frame(popMeans(mm0, effect="populacao"))
pmm

#------------------------------------------------------------------------------------------
# contraste entre m�dias ajustadas

pmmc <- popMatrix(lm(formula(mm0), da), effect="populacao")
pmmc

source("funcoes.R")

allcontr <- apc(pmmc)

c0 <- summary(glht(mm0, linfct=allcontr), test=adjusted(type="bonferroni"))
c0

c0$focus <- "populacao"
cld(c0)

c1 <- confint(glht(mm0, linfct=pmmc))
pmm <- cbind(populacao=levels(da$populacao), as.data.frame(c1$confint))

#------------------------------------------------------------------------------------------
# colocando resultados em um gr�fico

pmm$cld <- cld(c0)$mcletters$Letters
pmm$regular <- 2
pmm$regular[grep("\\d", pmm$populacao)] <- 1
str(pmm)

segplot(reorder(populacao, Estimate)~lwr+upr, centers=Estimate, data=pmm,
        ylab="Produ��o", xlab="Popula��o", horizontal=FALSE, col=pmm$regular,
        draw.bands=FALSE, segments.fun=panel.arrows, ends="both",
        angle=90, length=1, unit="mm",
        panel=function(x, y, z, centers, subscripts, ...){
          panel.segplot(x, y, z, centers=centers, subscripts=subscripts, ...)
          panel.text(z, centers,
                     labels=paste(format(centers, digits=3), pmm$cld[subscripts]),
                     adj=c(0.5,-0.5), srt=90)
        })

#------------------------------------------------------------------------------------------
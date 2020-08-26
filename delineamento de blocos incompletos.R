#Análise de experimento em delineamento de blocos incompletos casualizados (bic) e fator
# de níveis categóricos (balanceado), resposta contínua, blocos fixos e aleatórios.
#==========================================================================================

# para gerar o delineamento em blocos incompletos balanceados
# http://www.wekaleamstudios.co.uk/posts/generating-balanced-incomplete-block-designs-bibd/

#------------------------------------------------------------------------------------------
# sobre o modelo atribuído ao experimento
# suposição distribucional: Y|x ~ Normal(\mu(x), \sigma)
# suposição funcional:      \mu(x) = \mu+\alpha_i+\tau_j  (i=2,...; j=2,...)          fixo

# suposição funcional:      \mu(x) = \mu+\alpha_i+b_j  (i=2,...; j=2,...)        aleatório
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

da <- read.table("http://www.leg.ufpr.br/~walmes/data/pimentel_bib3.txt",
                 header=TRUE, sep="\t")
str(da)
da <- transform(da, bloco=factor(bloco), variedade=factor(variedade))
str(da)

#------------------------------------------------------------------------------------------
# ver os dados

xtabs(~bloco+variedade, da) # cada bloco têm 3 variedades, ao todo 5 variedades
xtabs(~variedade, da)       # 6 repetições de cada variedade

xyplot(y~variedade, groups=bloco, data=da)

#------------------------------------------------------------------------------------------
# ajuste do modelo de efeito fixo de bloco

m0 <- lm(y~bloco+variedade, da)

par(mfrow=c(2,2)); plot(m0); layout(1) # análise dos pressupostos

anova(m0)   # quadro de análise de variância
summary(m0) # estimativa dos parâmetros

#------------------------------------------------------------------------------------------
# obtendo as médias ajustadas

pm <- as.data.frame(popMeans(m0, effect="variedade")) # IC com cobertura individual de 95%
pm # o erro padrão igual devido à balanceamento/equilibrio e igual número de repetições

#------------------------------------------------------------------------------------------
# comparando as médias ajustadas (allpairwise)

pmc <- popMatrix(m0, effect="variedade")
str(pmc)

source("funcoes.R")

allcontr <- apc(pmc)

summary(glht(m0, linfct=allcontr)) # sigle-step demora para muitas comparações
summary(glht(m0, linfct=allcontr), test=adjusted(type="bonferroni"))
summary(glht(m0, linfct=allcontr), test=adjusted(type="fdr"))

# o resultado é o mesmo devido ao equilibrio, se perder parcela melhor o de cima
c0 <- summary(glht(m0, linfct=mcp(variedade="Tukey")))
c0
cld(c0) # compact letter display, recomendado por terem erros padrões iguais

#------------------------------------------------------------------------------------------
# obtendo as letras

c1 <- summary(glht(m0, linfct=allcontr)) # sigle-step demora para muitas comparações
cld(c1) # dá erro porque não tem valor para "focus"
c1$focus <- "variedade" # adiciona valor
cld(c1)

#------------------------------------------------------------------------------------------
# colocando no gráfico

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
# ajuste do modelo de efeito aleatório de bloco

mm0 <- lme(y~variedade, random=~1|bloco, da)

plot(mm0) # resíduos e ajustados
qqnorm(residuals(mm0, type="pearson")) # normalidade dos resíduos
qqnorm(ranef(mm0)[[1]])                # normalidade dos efeitos de bloco

anova(mm0)   # quadro de teste de Wald
summary(mm0) # estimativa dos parâmetros

#------------------------------------------------------------------------------------------
# médias ajustas

pmm <- as.data.frame(popMeans(mm0, effect="variedade"))
pmm

#------------------------------------------------------------------------------------------
# contraste entre médias ajustadas

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
# limpa a memória e importa dados

rm(list=ls())
ls()

db <- read.table("http://www.leg.ufpr.br/~walmes/data/tastebibd.txt",
                 header=TRUE, sep="\t")
str(db)
db <- transform(db, block=factor(block), formulation=factor(formulation))
str(db)

#------------------------------------------------------------------------------------------
# ver os dados

xtabs(~block+formulation, db) # cada bloco têm 3 formulação, ao todo 5 variedades
xtabs(~formulation, db)       # 6 repetições de cada formulação

xyplot(rating~formulation, groups=block, data=db, jitter.x=TRUE)

#------------------------------------------------------------------------------------------
# ajuste do modelo com efeito aleatório de bloco

mm0 <- lme(rating~formulation, random=~1|block, db)

plot(mm0) # resíduos e ajustados
qqnorm(residuals(mm0, type="pearson")) # normalidade dos resíduos
qqnorm(ranef(mm0)[[1]])                # normalidade dos efeitos de bloco

anova(mm0)   # quadro de teste de Wald
summary(mm0) # estimativa dos parâmetros

#------------------------------------------------------------------------------------------
# médias ajustas

pmm <- as.data.frame(popMeans(mm0, effect="formulation"))
pmm

#------------------------------------------------------------------------------------------
# contraste entre médias ajustadas

pmmc <- popMatrix(lm(formula(mm0), db), effect="formulation")
pmmc

allcontr <- apc(pmmc)

c0 <- summary(glht(mm0, linfct=allcontr))
c0

c0$focus <- "formulation"
cld(c0)

#------------------------------------------------------------------------------------------
# colocando resultados em um gráfico

pmm$formulation <- factor(pmm$formulation, levels=levels(db$formulation))
pmm$cld <- cld(c0)$mcletters$Letters

segplot(reorder(formulation, Estimate)~Lower+Upper, centers=Estimate, data=pmm,
        xlab="Satisfação", ylab="Formulação",
        draw.bands=FALSE, segments.fun=panel.arrows, ends="both",
        angle=90, length=1, unit="mm",
        panel=function(x, y, z, centers, subscripts, ...){
          panel.segplot(x, y, z, centers=centers, subscripts=subscripts, ...)
          panel.text(centers, z,
                     labels=paste(format(centers, digits=3), pmm$cld[subscripts]), pos=3)
        })

#------------------------------------------------------------------------------------------
# limpa a memória e importa dados

rm(list=ls())
ls()

db <- read.table("http://www.leg.ufpr.br/~walmes/data/dietbibd.txt",
                 header=TRUE, sep="\t")
str(db)
db <- na.omit(db)
str(db)

#------------------------------------------------------------------------------------------
# ver os dados

xtabs(~age+fat, db)     # cada bloco têm 2 níveis
xtabs(~fat, db)         # 4 repetições

xyplot(lipids~fat, groups=age, data=db, jitter.x=TRUE)

#------------------------------------------------------------------------------------------
# ajuste do modelo com efeito aleatório de bloco

mm0 <- lme(lipids~fat, random=~1|age, db)

plot(mm0) # resíduos e ajustados
qqnorm(residuals(mm0, type="pearson")) # normalidade dos resíduos
qqnorm(ranef(mm0)[[1]])                # normalidade dos efeitos de bloco

anova(mm0)   # quadro de teste de Wald
summary(mm0) # estimativa dos parâmetros

#------------------------------------------------------------------------------------------
# médias ajustas

pmm <- as.data.frame(popMeans(mm0, effect="fat"))
pmm

#------------------------------------------------------------------------------------------
# contraste entre médias ajustadas

pmmc <- popMatrix(lm(formula(mm0), db), effect="fat")
pmmc

allcontr <- apc(pmmc)

c0 <- summary(glht(mm0, linfct=allcontr))
c0

c0$focus <- "fat"
cld(c0)

#------------------------------------------------------------------------------------------
# colocando resultados em um gráfico

pmm$fat <- factor(pmm$fat, levels=levels(db$fat))
pmm$cld <- cld(c0)$mcletters$Letters

segplot(reorder(fat, Estimate)~Lower+Upper, centers=Estimate, data=pmm,
        xlab="Lipídios", ylab="Gordura",
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

xtabs(~run+psi, db)     # cada bloco têm 3 níveis
xtabs(~psi, db)         # 6 repetições

xyplot(monovinyl~psi, groups=run, data=db, jitter.x=TRUE)

#------------------------------------------------------------------------------------------
# ajuste do modelo com efeito aleatório de bloco

mm0 <- lme(monovinyl~psi, random=~1|run, db)

plot(mm0) # resíduos e ajustados
qqnorm(residuals(mm0, type="pearson")) # normalidade dos resíduos
qqnorm(ranef(mm0)[[1]])                # normalidade dos efeitos de bloco

anova(mm0)   # quadro de teste de Wald
summary(mm0) # estimativa dos parâmetros

#------------------------------------------------------------------------------------------
# médias ajustas

pmm <- as.data.frame(popMeans(mm0, effect="psi"))
pmm

#------------------------------------------------------------------------------------------
# contraste entre médias ajustadas

pmmc <- popMatrix(lm(formula(mm0), db), effect="psi")
pmmc

allcontr <- apc(pmmc)

c0 <- summary(glht(mm0, linfct=allcontr))
c0

c0$focus <- "psi"
cld(c0)

#------------------------------------------------------------------------------------------
# colocando resultados em um gráfico

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
# os níveis do fator psi são contínuos, e mais interessante ajustar um modelo
# faremos uso de polinômio

mm0 <- update(mm0, method="ML")

db$psi <- as.numeric(as.character(db$psi))

mm1 <- lme(monovinyl~poly(psi, 2), random=~1|run, data=db, method="ML")

anova(mm0, mm1) # compara a redução no modelo
summary(mm1)

#------------------------------------------------------------------------------------------
# fazendo a predição e colocando no gráfico

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
# ajuste do modelo com efeito aleatório de bloco

mm0 <- lme(producao~populacao, random=~1|bloco, da)

plot(mm0) # resíduos e ajustados
qqnorm(residuals(mm0, type="pearson")) # normalidade dos resíduos
qqnorm(ranef(mm0)[[1]])                # normalidade dos efeitos de bloco

anova(mm0)   # quadro de teste de Wald
summary(mm0) # estimativa dos parâmetros

#------------------------------------------------------------------------------------------
# médias ajustas

pmm <- as.data.frame(popMeans(mm0, effect="populacao"))
pmm

#------------------------------------------------------------------------------------------
# contraste entre médias ajustadas

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
# colocando resultados em um gráfico

pmm$cld <- cld(c0)$mcletters$Letters
pmm$regular <- 2
pmm$regular[grep("\\d", pmm$populacao)] <- 1
str(pmm)

segplot(reorder(populacao, Estimate)~lwr+upr, centers=Estimate, data=pmm,
        ylab="Produção", xlab="População", horizontal=FALSE, col=pmm$regular,
        draw.bands=FALSE, segments.fun=panel.arrows, ends="both",
        angle=90, length=1, unit="mm",
        panel=function(x, y, z, centers, subscripts, ...){
          panel.segplot(x, y, z, centers=centers, subscripts=subscripts, ...)
          panel.text(z, centers,
                     labels=paste(format(centers, digits=3), pmm$cld[subscripts]),
                     adj=c(0.5,-0.5), srt=90)
        })

#------------------------------------------------------------------------------------------
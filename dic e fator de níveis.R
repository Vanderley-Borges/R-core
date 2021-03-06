# An�lise de experimento em delineamento inteiramente casualizado (dic) e fator de n�veis
# categ�ricos (balanceado), resposta cont�nua.
#==========================================================================================

#------------------------------------------------------------------------------------------
# sobre o modelo atribu�do ao experimento
# suposi��o distribucional: Y|x ~ Normal(\mu(x), \sigma)
# suposi��o funcional:      \mu(x) = \mu+\alpha_i  (i=2,...)

#------------------------------------------------------------------------------------------
# defini��es da sess�o

require(lattice)
require(agricolae)
require(contrast)
require(multcomp)
require(doBy)
require(latticeExtra)
require(ExpDes)

#------------------------------------------------------------------------------------------
# lendo arquivos de dados

da <- read.table("http://www.leg.ufpr.br/~walmes/data/pimentel_racoes.txt",
                 header=TRUE, sep="\t")
str(da)
is.factor(da$racoes)
levels(da$racoes)

#------------------------------------------------------------------------------------------
# ver os dados

xyplot(ganhopeso~racoes, data=da)

#------------------------------------------------------------------------------------------
# ajuste do modelo

m0 <- lm(ganhopeso~racoes, data=da)

par(mfrow=c(2,2)); plot(m0); layout(1) # an�lise dos pressupostos

anova(m0)   # quadro de an�lise de vari�ncia
summary(m0) # estimativa dos par�metros

#------------------------------------------------------------------------------------------
# acessando resultados do ajuste

coef(m0)          # estimativas
fitted(m0)        # valores ajustados
residuals(m0)     # res�duos
model.matrix(m0)  # matriz do modelo
deviance(m0)      # soma de quadrado dos res�duos
df.residual(m0)   # grau de liberdade dos res�duos
vcov(m0)          # matriz de covari�ncia das estimativas

#------------------------------------------------------------------------------------------
# matricialmente como s�o obtidos

X <- model.matrix(~racoes, da)  # matriz do modelo
y <- as.matrix(da$ganhopeso)    # vetor de observa��es

# estimativas
beta <- solve(t(X)%*%X)%*%t(X)%*%y # (X'X)^{-1} X'y
beta

# valores ajustados
fit <- X%*%beta
fit

# res�duos
res <- y-fit

# grau de liberdade do res�duos
dfr <- length(da$ganhopeso)-length(beta)
dfr

# soma de quadrado dos res�duos
sqr <- t(res)%*%res

qmr <- sqr/dfr # quadrado m�dio, estimatica de \sigma^2

# covariancia das estimativas
solve(t(X)%*%solve(diag(c(qmr), nrow(y)))%*%X)
solve(t(X)%*%X)*c(qmr)

#------------------------------------------------------------------------------------------
# aplicando testes de m�dia, caso de igual n�mero de repeti��es
# todos contra todos (all pairwise)

xtabs(~racoes, na.omit(da)) # repeti��es por n�vel

glr <- df.residual(m0)
qmr <- deviance(m0)/glr

# teste t
with(da,
     LSD.test(ganhopeso, racoes, DFerror=glr, MSerror=qmr, alpha=0.05))

# teste t com corre��o de Bonferroni
with(da,
     LSD.test(ganhopeso, racoes, DFerror=glr, MSerror=qmr, alpha=0.05, p.adj="bonferroni"))

# teste de Tukey
with(da,
     HSD.test(ganhopeso, racoes, DFerror=glr, MSerror=qmr, alpha=0.05))

# teste de Student-Newman-Keuls
with(da,
     SNK.test(ganhopeso, racoes, DFerror=glr, MSerror=qmr, alpha=0.05))

#------------------------------------------------------------------------------------------
# fazendo contrastes entre os n�veis
# contrastes ortogonais: A vs B, C vs D, A+B vs C+D

contrast(m0, list(racoes="A"), list(racoes="B"))
contrast(m0, list(racoes="C"), list(racoes="D"))
contrast(m0, type="average",
         list(racoes=c("A","B")), list(racoes=c("C","D")))

#------------------------------------------------------------------------------------------
# contrastes passando a matriz de contraste

C <- cbind(1, contrasts(da$racoes))
C

beta <- coef(m0)

mc <- rbind("AvsB"=c(1,-1,0,0),
            "CvsD"=c(0,0,1,-1),
            "ABvsCD"=c(1,1,-1,-1)/2)

C%*%beta        # valores ajustados ou m�dias (ajustadas)
mc%*%(C%*%beta) # estimativas dos contrastes

g0 <- glht(m0, linfct=mc%*%C)
summary(g0, test=adjusted(type="none"))
confint(g0)

#------------------------------------------------------------------------------------------
# representando os resultados, m�dias ajustadas com intervalo de confian�a

pm <- popMeans(m0, effect="racoes")
pm <- as.data.frame(pm)
pm$racoes <- factor(pm$racoes, levels=levels(da$racoes))

segplot(racoes~Lower+Upper, center=Estimate, data=pm)

segplot(racoes~Lower+Upper, center=Estimate, data=pm,
        xlab="Ganho de peso", ylab="Ra��es",
        draw.bands=FALSE, segments.fun=panel.arrows, ends="both",
        angle=90, length=1, unit="mm")

#------------------------------------------------------------------------------------------
# obtendo os resultados com o pacote ExpDes

with(da, crd(treat=racoes, resp=ganhopeso, mcomp="tukey"))
with(da, crd(treat=racoes, resp=ganhopeso, mcomp="sk")) # Scott-Knott

#------------------------------------------------------------------------------------------
# outros dados na mesma estrutura

db <- read.table("http://www.leg.ufpr.br/~walmes/data/pimentel_feno.txt",
                 header=TRUE, sep="\t")

#------------------------------------------------------------------------------------------
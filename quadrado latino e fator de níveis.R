An�lise de experimento em delineamento quadrado latino e fator de n�veis
# categ�ricos (balanceado), resposta cont�nua.
#==========================================================================================

#------------------------------------------------------------------------------------------
# sobre o modelo atribu�do ao experimento
# suposi��o distribucional: Y|x ~ Normal(\mu(x), \sigma)
# suposi��o funcional:      \mu(x) = \mu+\alpha_i+\eta_j+\tau_k  (i=2,...; j=2,..., k=2...)

#------------------------------------------------------------------------------------------
# defini��es da sess�o

require(lattice)
require(agricolae)
require(contrast)
require(multcomp)
require(doBy)
require(latticeExtra)
require(ExpDes)
require(reshape)

#------------------------------------------------------------------------------------------
# lendo arquivos de dados

da <- read.table("http://www.leg.ufpr.br/~walmes/data/pimentel_canadeacucar2.txt",
                 header=TRUE, sep="\t")
str(da)
da <- transform(da, linha=factor(linha), coluna=factor(coluna))
str(da)

#------------------------------------------------------------------------------------------
# ver os dados

xtabs(~linha+coluna, da)
xtabs(as.numeric(variedade)~linha+coluna, da)

xyplot(producao~variedade, data=da)

levelplot(producao~linha+coluna, da,
          panel=function(x, y, z, subscripts, ...){
            panel.levelplot(x, y, z, subscripts=subscripts)
            panel.text(x, y, da$variedade[subscripts])
            panel.text(x, y, z, pos=1)
          })

#------------------------------------------------------------------------------------------
# ajuste do modelo

m0 <- lm(producao~linha+coluna+variedade, data=da)
par(mfrow=c(2,2)); plot(m0); layout(1)

anova(m0)   # quadro de an�lise de vari�ncia
summary(m0) # estimativas dos par�metros

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

X <- model.matrix(~linha+coluna+variedade, da)  # matriz do modelo
y <- as.matrix(da$producao)                     # vetor de observa��es

# estimativas
beta <- solve(t(X)%*%X)%*%t(X)%*%y # (X'X)^{-1} X'y
beta

# valores ajustados
fit <- X%*%beta
fit

# res�duos
res <- y-fit

# grau de liberdade do res�duos
dfr <- length(da$producao)-length(beta)
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

glr <- df.residual(m0)
qmr <- deviance(m0)/glr

# teste t com corre��o de Bonferroni
with(da,
     LSD.test(producao, variedade, DFerror=glr, MSerror=qmr,
              alpha=0.05, p.adj="bonferroni"))

# teste de Tukey
with(da,
     HSD.test(producao, variedade, DFerror=glr, MSerror=qmr, alpha=0.05))

#------------------------------------------------------------------------------------------
# representando os resultados, m�dias ajustadas com intervalo de confian�a

pm <- popMeans(m0, effect="variedade")
pm <- as.data.frame(pm)
pm$variedade <- factor(pm$variedade, levels=levels(da$variedade))

segplot(variedade~Lower+Upper, center=Estimate, data=pm)

segplot(reorder(variedade, Estimate)~Lower+Upper, center=Estimate, data=pm,
        xlab="Produ��o", ylab="Variedade",
        draw.bands=FALSE, segments.fun=panel.arrows, ends="both",
        angle=90, length=1, unit="mm")

#------------------------------------------------------------------------------------------
# obtendo os resultados com o pacote ExpDes

with(da, latsd(treat=variedade, row=linha, column=coluna, resp=producao, mcomp="tukey"))
with(da, latsd(treat=variedade, row=linha, column=coluna, resp=producao, mcomp="sk"))

#------------------------------------------------------------------------------------------
# outros dados na mesma estrutura

# http://www.leg.ufpr.br/~walmes/data/pimentel_crotalaria.txt
# http://www.leg.ufpr.br/~walmes/data/pimentel_castracao.txt

#------------------------------------------------------------------------------------------
# limpa a mem�ria e importa dados

rm(list=ls())
ls()

db <- read.table("http://www.leg.ufpr.br/~walmes/data/pimentel_crotalaria.txt",
                 header=TRUE, sep="\t")
str(db)
db <- transform(db, linhas=factor(linhas), colunas=factor(colunas))
str(db)

#------------------------------------------------------------------------------------------
# ver os dados

xyplot(producao~adubacao, data=db)

levelplot(producao~linhas+colunas, db,
          panel=function(x, y, z, subscripts, ...){
            panel.levelplot(x, y, z, subscripts=subscripts)
            panel.text(x, y, as.numeric(db$adubacao[subscripts]))
            panel.text(x, y, z, pos=1)
          })

#------------------------------------------------------------------------------------------
# ajuste do modelo

m0 <- lm(producao~linhas+colunas+adubacao, data=db)
par(mfrow=c(2,2)); plot(m0); layout(1)

anova(m0)   # quadro de an�lise de vari�ncia
summary(m0) # estimativas dos par�metros

#------------------------------------------------------------------------------------------
# fazendo contrastes planejados (n�o necess�riamento ortogonais nesse caso)

lad <- levels(db$adubacao) # n�veis de aduba��o
lli <- levels(db$linhas)   # n�veis de linha
lco <- levels(db$colunas)  # n�veis de colunas

# COM vs SEM calc�rio (1+2+3 vs 4+5+6)
contrast(m0, type="average",
         list(linhas=lli[1], colunas=lco[1], adubacao=lad[1:3]),
         list(linhas=lli[1], colunas=lco[1], adubacao=lad[4:6]))

# COM vs SEM aduba��o (1+4 vs 2+5)
contrast(m0, type="average",
         list(linhas=lli[1], colunas=lco[1], adubacao=lad[c(1,4)]),
         list(linhas=lli[1], colunas=lco[1], adubacao=lad[c(2,5)]))

# COM vs SEM crotalaria (2+5 vs 3+6)
contrast(m0, type="average",
         list(linhas=lli[1], colunas=lco[1], adubacao=lad[c(2,5)]),
         list(linhas=lli[1], colunas=lco[1], adubacao=lad[c(3,6)]))

#------------------------------------------------------------------------------------------
# m�dias ajustadas, intervalos de cobertura individual de 95%

pm <- as.data.frame(popMeans(m0, effect="adubacao"))
pm

#------------------------------------------------------------------------------------------
# fazendo o mesmo conjunto de contrastes de outra maneira (mais f�cil)

Xm <- popMatrix(m0, effect="adubacao")
Xm # matrix que define as m�dias ajustadas
Xm%*%coef(m0)

compr <- list(CvsScalc=list(1:3, 4:6),
              CvsSadub=list(c(1,4), c(2,5)),
              CvsScrot=list(c(2,5), c(3,6)))

colMeans(Xm[1:3,])

contr <- sapply(compr,
                function(x){
                  colMeans(Xm[x[[1]],])-colMeans(Xm[x[[2]],])
                })
dim(contr)
contr

summary(glht(m0, linfct=t(contr)), test=adjusted(type="none"))
summary(glht(m0, linfct=t(contr)), test=adjusted(type="bonferroni"))

#------------------------------------------------------------------------------------------
# os n�veis do fator adubacao s�o na verdade combina��es de n�veis de 3 fatores
# (calcario, adubo, crotalaria) ambos com 2 n�veis (presente, ausente) mas n�o foram
# aplicadas todas as combina��es (fatorial incompleto). Vamos analisar o fatorial
# incompleto no futuro.

#------------------------------------------------------------------------------------------
# criando os n�veis de calc�rio, crotalaria e adubo

db$calc <- db$adub <- db$crot <- "S"
db$calc[grep("com calcario", db$adubacao)] <- "C"
db$adub[grep("com adubo", db$adubacao)] <- "C"
db$crot[grep("com crotalaria", db$adubacao)] <- "C"
str(db)

xtabs(~calc+adub+crot, db) # 0 s�o as combina��es que n�o ocorrem
lad

m1 <- lm(producao~linhas+colunas+calc*adub*crot, data=db)
anova(m0, m1) # mostra s�o modelos de mesmo espa�o

anova(m1) # crot mesmo p-value do glht("none") feito acima
          # formas equivalentes de testar a mesma hip�tese

summary(m1) # cont�m NA para a conbina��es do fatorial n�o realizadas

#------------------------------------------------------------------------------------------
# lendo arquivos de dados

db <- read.table("http://www.leg.ufpr.br/~walmes/data/pimentel_castracao.txt",
                 header=TRUE, sep="\t")
str(db)
db <- transform(db, leitegada=factor(leitegada), classepeso=factor(classepeso),
                castracao=factor(castracao, levels=c("testemunha","7","21","56")))
str(db)

#------------------------------------------------------------------------------------------
# ver os dados

xyplot(ganhopeso~castracao, data=db)

levelplot(ganhopeso~leitegada+classepeso, db,
          panel=function(x, y, z, subscripts, ...){
            panel.levelplot(x, y, z, subscripts=subscripts)
            panel.text(x, y, db$castracao[subscripts])
            panel.text(x, y, z, pos=1)
          })

#------------------------------------------------------------------------------------------
# ajuste do modelo

m0 <- lm(ganhopeso~classepeso+leitegada+castracao, data=db)
par(mfrow=c(2,2)); plot(m0); layout(1)

anova(m0)   # quadro de an�lise de vari�ncia
summary(m0) # estimativas dos par�metros

#------------------------------------------------------------------------------------------
# contrastes contra a testemunha (j� saem no summary mas vamos usar a fun��o contrast)

contrast(m0,
         list(classepeso="1", leitegada="1", castracao="testemunha"),
         list(classepeso="1", leitegada="1", castracao=c("7","21","56")))

#------------------------------------------------------------------------------------------
# testando a mesma hip�tese pela an�lise de vari�ncia
# esse delineamento � balanceado mas n�o � de efeitos ortogonais

m0 <- aov(ganhopeso~classepeso+leitegada+castracao, data=db)
summary(m0, split=list(castracao=list(Tvs7=1, Tvs21=2, Tvs56=3)))

#------------------------------------------------------------------------------------------
# m�dias ajustadas

popMeans(m0, effect="castracao")

#------------------------------------------------------------------------------------------
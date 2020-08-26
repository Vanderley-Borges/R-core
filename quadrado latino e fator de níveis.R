Análise de experimento em delineamento quadrado latino e fator de níveis
# categóricos (balanceado), resposta contínua.
#==========================================================================================

#------------------------------------------------------------------------------------------
# sobre o modelo atribuído ao experimento
# suposição distribucional: Y|x ~ Normal(\mu(x), \sigma)
# suposição funcional:      \mu(x) = \mu+\alpha_i+\eta_j+\tau_k  (i=2,...; j=2,..., k=2...)

#------------------------------------------------------------------------------------------
# definições da sessão

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

anova(m0)   # quadro de análise de variância
summary(m0) # estimativas dos parâmetros

#------------------------------------------------------------------------------------------
# acessando resultados do ajuste

coef(m0)          # estimativas
fitted(m0)        # valores ajustados
residuals(m0)     # resíduos
model.matrix(m0)  # matriz do modelo
deviance(m0)      # soma de quadrado dos resíduos
df.residual(m0)   # grau de liberdade dos resíduos
vcov(m0)          # matriz de covariância das estimativas

#------------------------------------------------------------------------------------------
# matricialmente como são obtidos

X <- model.matrix(~linha+coluna+variedade, da)  # matriz do modelo
y <- as.matrix(da$producao)                     # vetor de observações

# estimativas
beta <- solve(t(X)%*%X)%*%t(X)%*%y # (X'X)^{-1} X'y
beta

# valores ajustados
fit <- X%*%beta
fit

# resíduos
res <- y-fit

# grau de liberdade do resíduos
dfr <- length(da$producao)-length(beta)
dfr

# soma de quadrado dos resíduos
sqr <- t(res)%*%res

qmr <- sqr/dfr # quadrado médio, estimatica de \sigma^2

# covariancia das estimativas
solve(t(X)%*%solve(diag(c(qmr), nrow(y)))%*%X)
solve(t(X)%*%X)*c(qmr)

#------------------------------------------------------------------------------------------
# aplicando testes de média, caso de igual número de repetições
# todos contra todos (all pairwise)

glr <- df.residual(m0)
qmr <- deviance(m0)/glr

# teste t com correção de Bonferroni
with(da,
     LSD.test(producao, variedade, DFerror=glr, MSerror=qmr,
              alpha=0.05, p.adj="bonferroni"))

# teste de Tukey
with(da,
     HSD.test(producao, variedade, DFerror=glr, MSerror=qmr, alpha=0.05))

#------------------------------------------------------------------------------------------
# representando os resultados, médias ajustadas com intervalo de confiança

pm <- popMeans(m0, effect="variedade")
pm <- as.data.frame(pm)
pm$variedade <- factor(pm$variedade, levels=levels(da$variedade))

segplot(variedade~Lower+Upper, center=Estimate, data=pm)

segplot(reorder(variedade, Estimate)~Lower+Upper, center=Estimate, data=pm,
        xlab="Produção", ylab="Variedade",
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
# limpa a memória e importa dados

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

anova(m0)   # quadro de análise de variância
summary(m0) # estimativas dos parâmetros

#------------------------------------------------------------------------------------------
# fazendo contrastes planejados (não necessáriamento ortogonais nesse caso)

lad <- levels(db$adubacao) # níveis de adubação
lli <- levels(db$linhas)   # níveis de linha
lco <- levels(db$colunas)  # níveis de colunas

# COM vs SEM calcário (1+2+3 vs 4+5+6)
contrast(m0, type="average",
         list(linhas=lli[1], colunas=lco[1], adubacao=lad[1:3]),
         list(linhas=lli[1], colunas=lco[1], adubacao=lad[4:6]))

# COM vs SEM adubação (1+4 vs 2+5)
contrast(m0, type="average",
         list(linhas=lli[1], colunas=lco[1], adubacao=lad[c(1,4)]),
         list(linhas=lli[1], colunas=lco[1], adubacao=lad[c(2,5)]))

# COM vs SEM crotalaria (2+5 vs 3+6)
contrast(m0, type="average",
         list(linhas=lli[1], colunas=lco[1], adubacao=lad[c(2,5)]),
         list(linhas=lli[1], colunas=lco[1], adubacao=lad[c(3,6)]))

#------------------------------------------------------------------------------------------
# médias ajustadas, intervalos de cobertura individual de 95%

pm <- as.data.frame(popMeans(m0, effect="adubacao"))
pm

#------------------------------------------------------------------------------------------
# fazendo o mesmo conjunto de contrastes de outra maneira (mais fácil)

Xm <- popMatrix(m0, effect="adubacao")
Xm # matrix que define as médias ajustadas
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
# os níveis do fator adubacao são na verdade combinações de níveis de 3 fatores
# (calcario, adubo, crotalaria) ambos com 2 níveis (presente, ausente) mas não foram
# aplicadas todas as combinações (fatorial incompleto). Vamos analisar o fatorial
# incompleto no futuro.

#------------------------------------------------------------------------------------------
# criando os níveis de calcário, crotalaria e adubo

db$calc <- db$adub <- db$crot <- "S"
db$calc[grep("com calcario", db$adubacao)] <- "C"
db$adub[grep("com adubo", db$adubacao)] <- "C"
db$crot[grep("com crotalaria", db$adubacao)] <- "C"
str(db)

xtabs(~calc+adub+crot, db) # 0 são as combinações que não ocorrem
lad

m1 <- lm(producao~linhas+colunas+calc*adub*crot, data=db)
anova(m0, m1) # mostra são modelos de mesmo espaço

anova(m1) # crot mesmo p-value do glht("none") feito acima
          # formas equivalentes de testar a mesma hipótese

summary(m1) # contém NA para a conbinações do fatorial não realizadas

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

anova(m0)   # quadro de análise de variância
summary(m0) # estimativas dos parâmetros

#------------------------------------------------------------------------------------------
# contrastes contra a testemunha (já saem no summary mas vamos usar a função contrast)

contrast(m0,
         list(classepeso="1", leitegada="1", castracao="testemunha"),
         list(classepeso="1", leitegada="1", castracao=c("7","21","56")))

#------------------------------------------------------------------------------------------
# testando a mesma hipótese pela análise de variância
# esse delineamento é balanceado mas não é de efeitos ortogonais

m0 <- aov(ganhopeso~classepeso+leitegada+castracao, data=db)
summary(m0, split=list(castracao=list(Tvs7=1, Tvs21=2, Tvs56=3)))

#------------------------------------------------------------------------------------------
# médias ajustadas

popMeans(m0, effect="castracao")

#------------------------------------------------------------------------------------------
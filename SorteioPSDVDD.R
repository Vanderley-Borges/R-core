#------------------------------------------------------
# Exemplo constante no livro: Experimentação Agrícola -
# David A. Banzatto e Sérgio do N. Kronka, 3ª ed.     -
#                                                     -
# Página 177 - EXPERIMENTO EM PARCELAS SUBDIVIDIDAS   -
#---------------------------------------------------------------------
#' São comparados 4 variedades de aveia e 4 tratamentos de sementes  - 
#' (3 produtos químicos + testemunha não tratada) quanto aos efeitos -
#' sobre a produção:                                                 -
#'                                                                   -
#' As variedades utilizadas (fator A) foram:                         -
#'  A1 - Vicland 1 - infectada com H. victoriae                      -
#'  A2 - Vicland 2 - não infectada                                   -
#'  A3 - Clinton - resistente a H. Victoriae                         -
#'  A4 - Branch - resistente a H. Victoriae                          -
#'                                                                   -
#' As 4 variedades foram distribuídas ao acaso nas parcelas de cada  -
#' um dos 4 blocos do experimento.                                   -
#'                                                                   -
#' Os tratamentos de sementes (fator B) foram:                       -
#'  B1 - Testemunha (não tratada)                                    -
#'  B2 - Ceresan M                                                   -
#'  B3 - Panogen                                                     -
#'  B4 - Agrox                                                       -
#---------------------------------------------------------------------
rm(list=ls())

# Carregando vários pacotes a serem usados na sessão simultaneamente
pkg = c("lattice", "latticeExtra", "gridExtra", "doBy", "multcomp",
        "reshape", "plyr", "nlme", "wzRfun")
sapply(pkg, require, character.only=TRUE)


#---------------------------------------
# Experimento em parcelas subdivididas -
#---------------------------------------
# resp = Produção de aveia
# Parcela: Variedades
# Suparcela: Tratamentos de Sementes

Variedades=c(paste('V', 1:4, sep=''))
Sementes=c(paste('S', 1:4, sep=''))
Blocos=c(paste('B', 1:4, sep=''))

(sorteio = design.split(Variedades, Sementes, r=4, serie=2, seed=45, 
                        kinds="Super-Duper", design=c("rcbd")))
(croqui = sorteio$book)

resp = c(63.6, 64.5, 62.3, 63.4, 64.1, 57.6, 53.3, 59.8, 70.3, 75.4, 71.6, 68.8, 42.9, 44.4, 49.5, 53.8,
         69.6, 69.6, 57.4, 65.8, 69.4, 65.3, 65.6, 67.3, 58.5, 50.4, 56.1, 46.1, 58.5, 41.6, 41.8, 53.8,
         40.7, 43.9, 28.9, 28.3, 42.4, 41.4, 45.4, 44.1, 44.6, 62.6, 52.7, 45.0, 57.6, 54.0, 56.6, 45.6,
         51.6, 51.9, 45.4, 35.1, 46.3, 39.4, 34.7, 30.8, 58.5, 52.7, 51.0, 47.4, 46.7, 50.3, 50.3, 51.8)

(dados = data.frame(croqui, resp))
colnames(dados) = c('Parcelas', 'Subparcelas', 'Blocos', 'Variedades', 'Sementes', 'resp')

head(dados)
tail(dados)
str(dados)
names(dados)

#-------------
# Descritiva -
#-------------
#
# Médias
(media.v = with(dados, tapply(resp, Variedades, mean)))
(media.s = with(dados, tapply(resp, Sementes, mean)))
(media.b = with(dados, tapply(resp, Blocos, mean)))
(media.vs = with(dados, tapply(resp, list(Variedades, Sementes), mean)))
(media.vb = with(dados, tapply(resp, list(Variedades, Blocos), mean)))
(media.vb = with(dados, tapply(resp, list(Sementes, Blocos), mean)))
(media.vsb = with(dados, tapply(resp, list(Variedades, Sementes, Blocos), mean)))

# Somas dos valores
(soma.g = with(dados, sum(resp)))
soma.vs = with(dados, tapply(resp, list(Variedades, Sementes), sum))
addmargins(soma.vs, FUN=list(Total=sum), quiet=T)

soma.vb = with(dados, tapply(resp, list(Variedades, Blocos), sum))
addmargins(soma.vb, FUN=list(Total=sum), quiet=T)

soma.sb = with(dados, tapply(resp, list(Sementes, Blocos), sum))
addmargins(soma.sb, FUN=list(Total=sum), quiet=T)

soma.vsb = with(dados, tapply(resp, list(Variedades, Sementes, Blocos), sum))
addmargins(soma.vsb, FUN=list(Total=sum), quiet=T)


library(doBy)
saída = summaryBy(resp ~ Variedades + Sementes, data=dados, FUN=c(length, mean, var, sd))
saída


#---------------
# Visualização -
#---------------
require(lattice)

xyplot(resp ~ Variedades, data=dados)
xyplot(resp ~ Sementes, data=dados)

xyplot(resp ~ Variedades + Sementes|Blocos, data=dados, auto.key=TRUE, pch=c(1,19))
xyplot(resp ~ Sementes|Variedades, data=dados, pch=c(19))

xyplot(resp ~ Sementes|Blocos, groups=Variedades, data=dados, pch=c(19,20,1,2), 
       type=c("p","a"), auto.key=TRUE, jitter.x=T)

xyplot(resp ~ Variedades|Blocos, groups=Sementes, data=dados, pch=c(19,20,1,2), 
       type=c("p","a"), auto.key=TRUE, jitter.x=T)

require(ggplot2)
#
# Sementes e Blocos
inter.1 = ggplot(data=dados, 
                 aes(x=Blocos, y=resp, group=Sementes, color=Sementes, 
                     linetype=Sementes, shape=Sementes)) +
  ggtitle("Interação dos dados brutos") +  xlab("Blocos") + ylab("Médias") +
  geom_point(size=3) + geom_line()
inter.1

# Variedades e Blocos
inter.2 = ggplot(data=dados, 
                 aes(x=Blocos, y=resp, group=Variedades, color=Variedades, 
                     linetype=Variedades, shape=Variedades)) +
  ggtitle("Interação dos dados brutos") +  xlab("Blocos") + ylab("Médias") +
  geom_point(size=3) + geom_line()
inter.2

# Variedades e Sementes
inter.3 = ggplot(data=dados, 
                 aes(x=Sementes, y=resp, group=Variedades, color=Variedades, 
                     linetype=Variedades, shape=Variedades)) +
  ggtitle("Interação dos dados brutos") +  xlab("Blocos") + ylab("Médias") +
  geom_point(size=3) + geom_line()
inter.3


# Gráfico de caixas da interação
with(dados, 
     boxplot(resp ~ Variedades:Sementes, data=dados, las=1, range=0,
             notch=FALSE, main="", sub="", col='lightyellow',
             xlab="Variedades*Sementes", ylab="Produção"))
points(c(media.vs), pch='+', col='red', cex=1.3)

with(dados, interaction.plot(Variedades, Sementes, resp, col=1:4, ylab='Produção'))




#------------------------------------------------
# Análise de variância em Parcelas Subdivididas -
#------------------------------------------------
mod.1 = with(dados, aov(resp ~ Blocos + Variedades*Sementes + Error(Blocos/Variedades)))
summary(mod.1)

#--------------------------------------------------------------------------------
# 1. Comparação entre níveis de SUBPARCELAS dentro de um nível comum de PARCELA -
#--------------------------------------------------------------------------------
#
# Tratamento: Testemunha
(trat.test = subset(dados, trat=='T1'))
trat.test.mod = lm(resp ~ bloco + epoca, data=trat.test)
anova(trat.test.mod)
(comp.1 = LSD.test(trat.test.mod, "epoca"))

require(multcomp)
comp.tukey.1 = glht(trat.test.mod, linfct=mcp(epoca="Tukey"))
plot(print(confint(comp.tukey.1)))

comp.dunnett = glht(trat.test.mod, linfct=mcp(epoca="Dunnett"))
confint(comp.dunnett)


# Tratamento: Polaris
(trat.polaris = subset(dados, trat=='T2'))
trat.polaris.mod = lm(resp ~ bloco + epoca, data=trat.polaris)
anova(trat.polaris.mod)
(comp.2 = LSD.test(trat.polaris.mod, "epoca"))

comp.tukey.p = glht(trat.polaris.mod, linfct=mcp(epoca="Tukey"))
plot(print(confint(comp.tukey.p)))


# Tratamento: Ethrel
(trat.ethrel = subset(dados, trat=='T3'))
trat.ethrel.mod = lm(resp ~ bloco + epoca, data=trat.ethrel)
anova(trat.ethrel.mod)
(comp.3 = LSD.test(trat.ethrel.mod, "epoca"))

comp.tukey.e = glht(trat.ethrel.mod, linfct=mcp(epoca="Tukey"))
plot(print(confint(comp.tukey.e)))


#--------------------------------------------------------------------------------
# 2. Comparação entre níveis de PARCELAS dentro de um nível comum de SUBPARCELA -
#--------------------------------------------------------------------------------
#
# Semana = 0
(sem.0 = subset(dados, epoca==0))
mod.sem.0 = lm(resp ~ bloco + trat, data=sem.0)
anova(mod.sem.0)
(comp.4 = LSD.test(mod.sem.0, "trat"))

# Semana = 2
(sem.2 = subset(dados, epoca==2))
mod.sem.2 = lm(resp ~ bloco + trat, data=sem.2)
anova(mod.sem.2)
(comp.5 = LSD.test(mod.sem.2, "trat"))

# Semana = 4
(sem.4 = subset(dados, epoca==4))
mod.sem.4 = lm(resp ~ bloco + trat, data=sem.4)
anova(mod.sem.4)
(comp.6 = LSD.test(mod.sem.4, "trat"))

# Semana = 6
(sem.6 = subset(dados, epoca==6))
mod.sem.6 = lm(resp ~ bloco + trat, data=sem.6)
anova(mod.sem.6)
(comp.7 = LSD.test(mod.sem.6, "trat"))

# Semana = 8
(sem.8 = subset(dados, epoca==8))
mod.sem.8 = lm(resp ~ bloco + trat, data=sem.8)
anova(mod.sem.8)
(comp.8 = LSD.test(mod.sem.8, "trat"))

# Semana = 10
(sem.10 = subset(dados, epoca==10))
mod.sem.10 = lm(resp ~ bloco + trat, data=sem.10)
anova(mod.sem.10)
(comp.9 = LSD.test(mod.sem.10, "trat"))


#-------------------------
# Usando o pacote ExpDes -
#-------------------------
require(ExpDes.pt)
with(dados, psub2.dbc(trat, epoca, bloco, resp, fac.names=c('Tratamentos', 'Semanas')))

#----------------------------
# Usando o pacote agricolae -
#----------------------------
require(agricolae)
with(dados, sp.plot(bloco, trat, epoca, resp))


#-----------------------
# Usando o pacote nlme -
#-----------------------
require(nlme)

# Criando a parcela
dados$parcela = with(dados, interaction(bloco, trat))

mod.10 = with(dados, lme(fixed=resp ~ bloco + trat*epoca, random=~1|parcela, 
                        data=dados, method="ML"))
anova(mod.10)
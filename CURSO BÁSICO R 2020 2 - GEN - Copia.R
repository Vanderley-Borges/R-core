title: "CURSO BÁSICO R 2020 - GEN - Planejamento Experimental e pressupostos da ANOVA"
output: html_notebook
---

##------------------- PLANEJAMENTO EXPERIMENTAL -------------------##

#1) Repetição
#2) Casualização
#3) Controle local

```{r}
library("agricolae")
```

```{r}
?design.X
# X deve ser substituido por:
# crd para Inteiramente casualizado
# rcbd para Blocos Completos casualizado
# ab para Esquema fatorial
# alpha para Alpha Latice
# bib para Blocos Incompletos
# split para Parcela Subdividida
# dau para Blocos Aumentados
# lsd para Quadrado Latino
# lattice para Latice
# graeco para Quadrado Greco-Latino
# strip para experimentos em faixas
```
          
#Ctrl + shift + H -> Definir diretório    

#DIC, X Tratamentos, X Repetições

```{r}
trt<-(1:X) #Número de tratamentos
r<-X #Número de repetições
```

#Sorteio
```{r}
sorteioDIC<-design.crd(trt, r, serie = 3, seed = 0, kinds = "Super-Duper",randomization=TRUE)
```

#Escrevendo a planilha de sorteio
```{r}
write.table(sorteioDIC$book, file='SorteioDIC.csv', 
            row.names=FALSE, col.names=c("Parcela", "Rep", "Trat"), sep =" ")   
```

       
#DBC, X Tratamentos, X Repetições          
```{r}
trt<-(1:X)
r<-X
```

```{r}
sorteioDBCC<-design.rcbd(trt, r, serie = 3, seed = 0, kinds = "Super-Duper",randomization=TRUE)
```

```{r}
write.table(sorteioDBCC$book, file='SorteioDBCC.csv', 
            row.names=FALSE, col.names=c("Parcela", "Bloco", "Trat"), sep =" ")
```


#FATORIAL A por B
```{r}
trt<-c(A,B)#Mais fatores -> separar por virgulas. Basta substituir A e B pelo nivel dos fatores, exemplo 5 x 4.
r<-X
```

#Fatorial DIC
```{r}
sorteioFATDIC<-design.ab(trt, r, serie = 3, design=c("crd"),seed = 0, kinds = "Super-Duper",first=TRUE,randomization=TRUE)
write.table(sorteioFATDIC$book, file='SorteioFATDIC.csv', 
            row.names=FALSE, col.names=c("Parcela", "Rep", "Fator A", "Fator B"), sep =" ")
```
 
#Fatorial DBC      
```{r}
sorteioFATDBCC<-design.ab(trt, r, serie = 3, design=c("rcbd"),seed = 0, kinds = "Super-Duper",first=TRUE,randomization=TRUE)
write.table(sorteioFATDBCC$book, file='SorteioFATDBCC.csv', 
            row.names=FALSE, col.names=c("Parcela", "Bloco", "Fator A", "Fator B"), sep =" ")
```


#PARCELA SUBDIVIDIDA
```{r}
trt1<-(1:3)
trt2<-(1:12)
r<-3
```

#Psub DIC
```{r}
sorteioSPLITDIC<-design.split(trt1, trt2,r, design=c("crd"),serie = 3, seed = 0, kinds = "Super-Duper", first=TRUE,randomization=TRUE)
```

```{r}
write.table(sorteioSPLITDIC$book, file='SorteioSPLITDIC.csv', 
            row.names=FALSE, col.names=c("Parcela", "Subparcela", "Rep", "Fator A", "Fator B"), sep =" ")
```

#Psub DBC
```{r}
sorteioSPLITDBCC<-design.split(trt1, trt2,r, design=c("rcbd"),serie = 3, seed = 0, kinds = "Super-Duper", first=TRUE,randomization=TRUE)
```

```{r}
write.table(sorteioSPLITDBCC$book, file='SorteioSPLITDBCC.csv', 
            row.names=FALSE, col.names=c("Parcela", "Subparcela", "Bloco", "Fator A", "Fator B"), sep =" ")
```


##------------------- PRESSUPOSTOS DA ANOVA -------------------##


# 1) Normalidade dos erros (Os erros devem seguir distribuição Normal)
# 2) Homocedasticidade dos erros (Os erros devem ser homogeneos)
# 3) Independência dos erros (Os erros devem ser independentes)
# 4) Aditividade do modelo (O modelo deve conter apenas efeitos aditivos)


#CARREGANDO PACOTES E IMPORTANDO OS CONJUNTOS DE DADOS
```{r}
library("agricolae")
library("laercio")
library("ggplot2")
data(sweetpotato) #agricolae
data(cotton) #agricolae
dados<-(ldata) #laercio
```


#DIC
$$
y_{ij} = \mu + t_i + e_{ij}
$$

#Sweetpotato

```{r}
AOVsweetpotato<-aov(yield ~ virus, data=sweetpotato)
AOVsweetpotato$residuals #Extraindo os residuos/erros
```

#Verificando a NORMALIDADE - Shapiro-Wilk
```{r}
shapiro.test(AOVsweetpotato$residuals)
```

#Verificando a NORMALIDADE - Gráfico QQplot
```{r}
#install.packages("fBasics")
library(fBasics)
qqnormPlot(AOVsweetpotato$residuals)
```

#Verificando a HOMOCEDASTICIDADE - Barlett
```{r}
bartlett.test(AOVsweetpotato$residuals~virus,data=sweetpotato) 
```

#Verificando a INDEPENDÊNCIA - Durbin-Watson Test
```{r}
#install.packages("car")
library("car")
dwt(lm(yield ~ virus, data=sweetpotato))
```

#Verificando a ADITIVIDADE -> Somente 1 fator principal (DIC) não precisa testar



#DBC
$$
y_{ij} = \mu + t_i + b_j + e_{ij}
$$

# Conjunto "Dados"
```{r}
dados<-(ldata) #laercio
str(dados)
dados<-transform(dados, trat=factor(trat), block=factor(block))
str(dados)
```

#Extraindo os residuos
```{r}
AOVdados<-aov(resp ~ trat + block, data=dados)
AOVdados$residuals #Extraindo os residuos/erros
```

#NORMALIDADE - Shapiro-Wilk
```{r}
shapiro.test(AOVdados$residuals)
qqnormPlot(AOVdados$residuals)
```

#HOMOCEDASTICIDADE - Bartlett
```{r}
bartlett.test(AOVdados$residuals~trat,data=dados) 
```

#HOMOCEDASTICIDADE - ONeill & Mathews
```{r}
library("ExpDes.pt")
oneilldbc(dados$resp, dados$trat, dados$block)
```

#INDEPENDÊNCIA - Durbin-Watson Test
```{r}
dwt(lm(resp ~ trat + block, data=dados))
```

#ADITIVIDADE - Tukey add Test
```{r}
#install.packages("asbio")
library(asbio)
tukey.add.test(dados$resp, dados$trat, dados$block)
```


##------------------- TRANSFORMÇÃO DE DADOS -------------------##

```{r}
dados2<-read.table("https://raw.githubusercontent.com/VSSEric/Introduction-to-R-UFLA-2019/master/Exemplos/exemplo2.txt", h=T)
str(dados2)
```

#Trabalhando como DIC
```{r}
AOVdados2<-aov(VarResp ~ Trat, data=dados2)
```

```{r}
shapiro.test(AOVdados2$residuals)
```

```{r}
bartlett.test(residuals(AOVdados2)~dados2$Trat)
```

```{r}
car::dwt(lm(VarResp ~ Trat, data=dados2))
```

##------------------- Testando Transformações Comuns -------------------##

#Raiz Quadrada
```{r}
dados2$RQUAD<-dados2$VarResp^(1/2)
```

#Raiz Cubica
```{r}
dados2$RCUB<-dados2$VarResp^(1/3)
```

#Log
```{r}
dados2$LOG<-log(dados2$VarResp)
```

#Potência
```{r}
dados2$POT2<-dados2$VarResp^2
```

#Conferindo a Planilha
```{r}
head(dados2)
```

#Extraindo os residuos
```{r}
AOVRQUAD<-aov(RQUAD ~ Trat, data=dados2)
AOVRCUB<-aov(RCUB ~ Trat, data=dados2)
AOVLOG<-aov(LOG ~ Trat, data=dados2)
AOVPOT2<-aov(POT2 ~ Trat, data=dados2)
```


# VERIFICANDO OS PRESSUPOSTOS

#Raiz Quadrada
```{r}
shapiro.test(AOVRQUAD$residuals)
bartlett.test(residuals(AOVRQUAD)~dados2$Trat)
car::dwt(lm(RQUAD ~ Trat, data=dados2))
```

#Raiz Cubica
```{r}
shapiro.test(AOVRCUB$residuals)
bartlett.test(residuals(AOVRCUB)~dados2$Trat)
car::dwt(lm(RCUB ~ Trat, data=dados2))
```

#LOG
```{r}
shapiro.test(AOVLOG$residuals)
bartlett.test(residuals(AOVLOG)~dados2$Trat)
car::dwt(lm(LOG ~ Trat, data=dados2))
```

#Potência
```{r}
shapiro.test(AOVPOT2$residuals)
bartlett.test(residuals(AOVPOT2)~dados2$Trat)
car::dwt(lm(POT2 ~ Trat, data=dados2))
```

#ANOVA
```{r}
AOVRQUAD<-aov(RQUAD ~ Trat, data=dados2)
anova(AOVRQUAD)
print(agricolae::HSD.test(AOVRQUAD, "Trat", group = TRUE)$groups)
```

```{r}
dic(trat=dados2$Trat, resp=dados2$RQUAD, quali = TRUE, mcomp = "tukey", nl = FALSE,
    hvar='bartlett', sigT = 0.05, sigF = 0.05)
```

          
          
#EXEMPLO DBC          

```{r}
dados3<-read.table("https://raw.githubusercontent.com/VSSEric/Introduction-to-R-UFLA-2019/master/Exemplos/exemplo2.txt", h=T)
```

```{r}
head(dados3)
```

```{r}
str(dados3)
dados3<-transform(dados3, Rep=factor(Rep))
str(dados3)
```

#Extraindo Residuos
```{r}
AOVdados3<-aov(VarResp ~ Trat + Rep, data=dados3)
```

```{r}
shapiro.test(AOVdados3$residuals)
```

```{r}
bartlett.test(residuals(AOVdados3)~dados3$Trat)
ExpDes.pt::oneilldbc(dados3$VarResp, dados3$Trat, dados3$Rep)
```

```{r}
car::dwt(lm(VarResp ~ Trat + Rep, data=dados3))
```

```{r}
asbio::tukey.add.test(dados3$VarResp, dados3$Trat, dados3$Rep)
```

##------------------------- VERIFICANDO VIA BOXCOX ---------------------##

```{r}
#install.packages("MASS")
library("MASS")
```

```{r}
bc<-boxcox(AOVdados3)
bc
```

```{r}
lambda <- bc$x[which(bc$y==max(bc$y))]
lambda
```

#Realizando a transformação indicada
```{r}
dados3$VRT<-dados3$VarResp^lambda
head(dados3)
```

#Extraindo residuos
```{r}
AOVtr<- aov(VRT ~ Trat + Rep, data=dados3)
```

```{r}
shapiro.test(residuals(AOVtr))
bartlett.test(residuals(AOVtr)~dados3$Trat)
car::dwt(lm(AOVtr)) 
asbio::tukey.add.test(dados3$VRT,  dados3$Rep, dados3$Trat)
```

#ANOVA
```{r}
anova(AOVtr)
print(agricolae::HSD.test(AOVtr, "Trat", group = TRUE)$groups)
```

```{r}
dbc(trat=dados3$Trat, bloco=dados3$Rep, resp=dados3$VRT, quali = TRUE, mcomp = "tukey")
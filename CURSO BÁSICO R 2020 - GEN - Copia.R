title: "CURSO BÁSICO R 2020 - GEN - Primeiros passos"
output: html_notebook
---

#Citando o R
```{r}
citation() 
```

#INSTALANDO PACOTES
```{r}
install.packages("ExpDes.pt")
```

#CARREGANDO PACOTES
```{r}
library("ExpDes.pt")
```

#Citando pacotes do R
```{r}
citation("ExpDes.pt")
```

#OPERAÇÕES MATEMATICAS
```{r}
2+2
3-1
2*3 
4/2
2^2
sqrt(100)
sin(45)
cos(45)
tan(45)
4<7	  
2<=3 	
5>2	  
4>=3	
2==3	
6!=7	
```

##-------------------------- CRIANDO OBJETOS -------------------------##
#CRIANDO OBJETOS
```{r}
a<-7 
a 
b<-5
a+b 
c<-a+b  
c
a
a<-19
a
a<-7
a2<-19
a
a2
ls()
rm(a2)
ls()
rm(list=ls())
```

##---------------------- ENTRANDO COM DADOS NO R ----------------------##

#1 - ENTRANDO COM DADOS DIRETAMENTE NO R
```{r}
a<-c(10, 20, 30, 40, 50, 60) #criando Vetor A
a
b <-c(2, 4, 6, 10, 8, 12) #Criando Vetor B
b
```

#OPERAÇÕES BÁSICAS
```{r}
a+b
a*b
a/b
sum(a)
sum(a+b)
length(a+b)
media<-sum(a+b)/length(a+b)
media
mean(a+b) 
```

#MANIPULAÇÕES

#nomeando
```{r}
names(b)<-c("A","B","C","D", "E", "F") 
b
names(a)<-c("A","B","C","D", "E", "F")
a
a["A"]+b["E"]
```

#ordenando          
```{r}
order(b, na.last = TRUE, decreasing = TRUE)
novob<-b[(order(b, na.last = TRUE, decreasing = TRUE))]
novob
```

#Recortando, removendo, adicionando e substituindo valores
```{r}
b2<-b[1:3];b2  
b3<-b[-5]; b3  
b[6]<-100; b
mean(a); a
a2<-a[a>mean(a)]; a2 #Somente valores acima da média
mean(a2) #nova média
```

#Criando um novo dataframe
```{r}
ab<-data.frame(a,b); ab 
```

```{r}
names(ab)<-c("Amb1", "Amb2");ab 
```

```{r}
ab$Amb1 #Observar somente o Ambiente 1
```

```{r}
mean(ab$Amb1)
var(ab$Amb1) 
```

```{r}
length(ab[1,])
print(ab[1,])
```

```{r}
length(ab[,2])
print(ab[,2])
```

```{r}
sum(ab[1,]) 
sum(ab[,"Amb2"])
```

```{r}
ab
```

#FUNÇÃO APPLY
#apply(X, MARGIN, FUN, ...) X=objeto, Margin = Lin ou Col? FUN = função
```{r}
apply(ab,1,mean) #média de todos os genotipos (1 para linhas)
apply(ab,2,mean) #média de todos os ambientes (2 para colunas)
```

#Um novo ambiente
```{r}
ab$Amb3<- c(30, 60, 90, 100, 120, 760); ab
mean(ab$Amb3)
```

#Conectando comandos
```{r}
ab[ab$Amb1 > mean(ab$Amb1)
   & ab$Amb2 > mean(ab$Amb2)
   & ab$Amb3 > mean(ab$Amb3), ]
```

```{r}
ab2<-ab[order(ab$Amb2, decreasing = TRUE), ]; ab2
```

 
#2- IMPORTANDO PANILHAS DE DADOS

#Definindo diretorio
```{r}
setwd("C:\\Users\\Usuario\\Desktop\\CURSO R GEN 2020") #Ou Crtl+Shift+H
dir()
```

#Importando .TXT
```{r}
exemplo1<-read.table("https://raw.githubusercontent.com/VSSEric/Introduction-to-R-UFLA-2019/master/Exemplos/exemplo1.txt", h=T)
```

```{r}
head(exemplo1)
tail(exemplo1)
```

```{r}
str(exemplo1)
```

#Nomeando as linhas
```{r}
rownames(exemplo1)<-c(LETTERS[1:20])
colnames(exemplo1)<-c("Lavras", "Ijaci", "Itutinga")
head(exemplo1)
```

#Manipulação
```{r}
exemplo1$Lavras
exemplo1$Ijaci
exemplo1$Itutinga
attach(exemplo1)
mean(Lavras)
mean(Ijaci)
mean(Itutinga)
detach(exemplo1)
apply(exemplo1,1,mean) #média de todos os genotipos
apply(exemplo1,2,mean) #média de todos os ambientes
```

```{r}
exemplo1[exemplo1$Lavras > mean(exemplo1$Lavras)
   & exemplo1$Ijaci > mean(exemplo1$Ijaci)
   & exemplo1$Itutinga > mean(exemplo1$Itutinga), ]
```

```{r}
ex12<-exemplo1[order(exemplo1$Ijaci, decreasing = TRUE), ]; ex12
```

#ESTATÍSTICA DESCRITIVA BÁSICA
```{r}
summary(exemplo1)
var(exemplo1)
apply(exemplo1,2,var)
apply(exemplo1,2,summary)
apply(exemplo1,1, summary) 
apply(exemplo1,1,var)
```

#Instalando pacotes
```{r}
# install.packages("agricolae")
# install.packages("laercio")
# install.packages("ggplot2")
library("agricolae")
library("laercio")
library("ggplot2")
```

#Importando dados de pacotes
```{r}
data(sweetpotato) #agricolae
data(cotton) #agricolae
dados<-(ldata) #laercio
```

```{r}
head(sweetpotato)
summary(sweetpotato)
str(sweetpotato)
```

```{r}
head(dados)
summary(dados)
str(dados)
```

```{r}
head(cotton)
summary(cotton)
str(cotton)
```

```{r}
boxplot(sweetpotato)
ggplot(sweetpotato) + 
  geom_boxplot(aes(x = virus, y = yield))
```

```{r}
boxplot(dados)
dados<-transform(dados, trat=factor(trat))
ggplot(dados) + 
  geom_boxplot(aes(x = trat, y = resp))
```

```{r}
boxplot(cotton)
library("ggplot2")
ggplot(cotton) + 
  geom_boxplot(aes(x = site, y = yield))
```

#Identificando Outliers
```{r}
cottonLima <- cotton[cotton$site == "Lima",]
boxplot(cottonLima)
boxplot.stats(cottonLima$yield)
```

```{r}
#install.packages("outliers")
#install.packages("tidyverse")
library("outliers")
library("tidyverse")
```

```{r}
outlier(
  x = cottonLima$yield,
  logical = TRUE
) %>% as_tibble %>% 
  bind_cols(cottonLima)
```

##---------------------------------------------------------------------##
##----------------------------- EXEMPLOS ------------------------------##
##---------------------------------------------------------------------##

#Batata doce - DIC

```{r}
head(sweetpotato)
```

```{r}
str(sweetpotato)
```

```{r}
summary(sweetpotato)
```

#Aplicando Análise de Variância nos dados

#$$ abre o LaTex
$$
y_{ij} = \mu + t_i + e_{ij}
$$

#função AOV
```{r}
analise<-aov(yield ~ virus, data=sweetpotato)
summary(analise)
```

#ou função lm
```{r}
analise2<-lm(yield ~ virus, data=sweetpotato)
anova(analise2)
```

#Dados - DBC
```{r}
dados
```

$$
y_{ij} = \mu + t_i + b_j + e_{ij}
$$


#O que está errado???
```{r}
analise3<-aov(resp ~ trat + block, data=dados)
summary(analise3)
```

```{r}
str(dados)
```

```{r}
dados<-transform(dados, trat=factor(trat), block=factor(block))
str(dados)
```

```{r}
analise4<-aov(resp ~ trat + block, data=dados)
summary(analise4)
```

```{r}
analise5<-lm(resp ~ trat + block, data=dados)
anova(analise5)
```

#Utilizando pacotes
```{r}
library("ExpDes.pt")
```

```{r}
?ExpDes.pt
```

#Sweetpotato
```{r}
dic(trat=sweetpotato$virus, resp=sweetpotato$yield, quali = TRUE, mcomp = "tukey", nl = FALSE,
    hvar='bartlett', sigT = 0.05, sigF = 0.05)
```

#Simplificando
```{r}
attach(sweetpotato)
dic(trat=virus, resp=yield, quali = TRUE, mcomp = "tukey", nl = FALSE,
    hvar='bartlett', sigT = 0.05, sigF = 0.05)
detach(sweetpotato)
```

#Dados DBC
```{r}
attach(dados)
dbc(trat=trat, bloco=block, resp=resp, quali=TRUE, mcomp = "tukey")
detach(dados)
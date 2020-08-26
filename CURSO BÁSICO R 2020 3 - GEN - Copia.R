---
title: "CURSO BÁSICO R 2020 - GEN - EXEMPLOS DE ANÁLISES COMPLETAS"
output: html_notebook
---

#EXEMPLO DIC QUALITATIVO

```{r}
dadosdic<-read.table("https://raw.githubusercontent.com/VSSEric/Introduction-to-R-UFLA-2019/master/Exemplos/dic.txt", h=T)
```

```{r}
head(dadosdic)
str(dadosdic)
dadosdic<-transform(dadosdic, Trat=factor(Trat))
```

```{r}
boxplot(dadosdic$Prod)
```

```{r}
library("outliers")
library("tidyverse")
outlier(
  x = dadosdic$Prod,
  logical = TRUE
) %>% as_tibble %>% 
  bind_cols(dadosdic)
```

$$
y_{ij} = \mu + t_i + e_{ij}
$$

```{r}
modelo<-lm(Prod ~ Trat, data=dadosdic)
```

```{r}
shapiro.test(modelo$residuals)
bartlett.test(residuals(modelo)~dadosdic$Trat)
car::dwt(lm(Prod ~ Trat, data=dadosdic))
```

```{r}
anova(modelo)
agricolae::cv.model(modelo)
```

```{r}
print(agricolae::HSD.test(modelo, "Trat", group = TRUE)$groups)
plot(agricolae::HSD.test(modelo, "Trat", group = TRUE))
```

```{r}
library("ggplot2")
ggplot(dadosdic, aes(x = Trat, y = Prod, color=Trat)) +
  geom_boxplot() +
  scale_colour_manual(position=("left"), values=c("blue", "blue", "blue",
                                                  "blue", "green", "red", 
                                                  "blue", "red", "blue",
                                                  "red")) +
  scale_y_continuous(limits=c(200, 600)) +
  guides(color = FALSE) +
  labs(x="Genótipos", y="Produção")
```

#EXEMPLO DBC QUANTITATIVO

```{r}
brix<-read.table("https://raw.githubusercontent.com/VSSEric/Introduction-to-R-UFLA-2019/master/Exemplos/dbc2.txt", h=T)
str(brix)
brix<-transform(brix, Bloco=factor(Bloco))
str(brix)
```

```{r}
boxplot(brix)
```

```{r}
outlier(
  x = brix$Brix,
  logical = TRUE
) %>% as_tibble %>% 
  bind_cols(brix)
```

```{r}
require(lattice)
xyplot(Brix~Epoca, data=brix, jitter.x=T, type=c("p","a"))
```


$$
y_{ij} = \mu + b_j + t_i + e_{ij}
$$

```{r}
mbrix<-lm(Brix ~ Bloco + ordered(Epoca), data=brix)
```

```{r}
shapiro.test(mbrix$residuals)
bartlett.test(residuals(mbrix)~brix$Epoca)
car::dwt(lm(Brix ~ Bloco + ordered(Epoca), data=brix))
asbio::tukey.add.test(brix$Brix, brix$Bloco, ordered(brix$Epoca))
```

```{r}
anova(mbrix)
agricolae::cv.model(mbrix)
```

```{r}
summary(mbrix)
```

```{r}
m1brix<-lm(Brix ~ Bloco + Epoca, data=brix)
m2brix<-lm(Brix ~ Bloco + Epoca + I(Epoca^2), data=brix)
m3brix<-lm(Brix ~ Bloco + Epoca + I(Epoca^2) + I(Epoca^3), data=brix)
```

```{r}
anova(m1brix)
anova(m2brix)
anova(m3brix)
```

```{r}
anova(m1brix, m2brix, m3brix)
```

```{r}
summary(m2brix)
```


```{r}
coeficientes<-coef(m2brix)
coeficientes
```


```{r}
Majus<-predict(m2brix)
ajust<-as.data.frame(Majus)
ajust$Epoca<-brix$Epoca
ajust
```

```{r}
library("ggplot2")
library("ggpmisc")
ggplot(brix, aes(x=Epoca, y=Brix)) +
  stat_smooth(data=ajust, aes(y = Majus, x=Epoca),method = "lm", formula = y ~ x + I(x^2), size = 1)+
  stat_poly_eq(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),formula = y ~ x + I(x^2), parse = TRUE) +
  geom_point(size=1.5, pch=21, fill='darkred',
             colour='grey30', alpha=0.7) +
  scale_x_continuous(limits=c(0,35), breaks=seq(0,35,1)) +
  scale_y_continuous(limits=c(10,25), breaks=seq(10, 25, 1)) +
  xlab('Época (Dias)') +
  ylab(expression(paste('Brix (' , degree, 'Bx)'))) +
  theme_bw()
```


#Utilizando pacote ExpDes.pt
```{r}
library(ExpDes.pt)
AnovaDBC<-dbc(trat=brix$Epoca, bloco=brix$Bloco, resp=brix$Brix, quali = FALSE)
graficos(AnovaDBC, grau = 2, mod = TRUE, main = " ", sub = " ",
         xlab = "Época", ylab = "Brix", pch = 19,
         xlim = NULL, ylim = NULL, bty = "o")
```


#EXEMPLO DBC QUALITATIVO

```{r}
#Pacote com conjunto de dados
install.packages("agridat")
```

```{r}
library("agridat")
DADOS<-(dasilva.maize)
```

#Trabalhando com o ambiente 1
```{r}
DADOSEXP1<-subset(DADOS, DADOS$env=="E1")
head(DADOSEXP1)
tail(DADOSEXP1)
```

```{r}
boxplot(DADOSEXP1$yield)
```

```{r}
outlier(
  x = DADOSEXP1$yield,
  logical = TRUE
) %>% as_tibble %>% 
  bind_cols(DADOSEXP1)
```

```{r}
require(lattice)
xyplot(yield~gen, data=DADOSEXP1, jitter.x=T, type=c("p","a"))
```

```{r}
analise1<-aov(yield ~ rep + gen, data=DADOSEXP1)
```

```{r}
shapiro.test(analise1$residuals)
bartlett.test(residuals(analise1)~DADOSEXP1$gen)
car::dwt(lm(yield ~ rep + gen, data=DADOSEXP1))
asbio::tukey.add.test(DADOSEXP1$yield, DADOSEXP1$rep, DADOSEXP1$gen)
```

```{r}
anova(analise1)
agricolae::cv.model(analise1)
```

```{r}
print(agricolae::HSD.test(analise1, "gen", group = TRUE)$groups)
plot(agricolae::HSD.test(analise1, "gen", group = TRUE))
```

```{r}
library("ScottKnott")
agrupamento<-SK(analise1, which="gen")
summary(agrupamento)
plot(agrupamento, title='Agrupamento dos Genótipos de Milho - Ambiente 1')
```


```{r}
library("ggplot2")
ggplot(DADOSEXP1, aes(x = gen, y = yield, fill=gen)) +
  geom_boxplot() +
  scale_y_continuous(limits=c(1, 10)) +
  guides(fill = FALSE) +
  labs(title = "Produtividade média de genótipos de milho - Ambiente 1",
       y = expression(Produtividade*" "(Kg*" "*ha^-1)), x = "Genótipos",
       caption = "Fonte: Curso Gen 2020")
```

#Criando um dataframe com o resultado do SK
```{r}
rank<-data.frame(summary(agrupamento))
colnames(rank)<-c("Genotipos", "Media", "Letra")
rank
```

#Paleta de grupo -> 3 grupos
```{r}
cores <- c(rgb(143,199,74,maxColorValue = 255),
           rgb(242,104,34,maxColorValue = 255), 
           rgb(111,145,202,maxColorValue = 255))
```

#Plotando o gráfico
```{r}
ggplot(rank, aes(x = Genotipos, y = Media)) +
  geom_col(fill=cores[as.numeric(rank$Letra)]) +
  ylim(0,10)+
    guides(fill = FALSE)+
  geom_text(aes(label = rank$Letra), vjust = -0.5, colour=cores[as.numeric(rank$Letra)])+
  labs(title = "Produtividade média de genótipos de milho - Ambiente 1",
       y = expression(Produtividade*" "(Kg*" "*ha^-1)), x = "Genótipos",
       caption = "Fonte: Curso Gen 2020")
```


#Trabalhando com o ambiente 2
```{r}
DADOSEXP2<-subset(DADOS, DADOS$env=="E2")
```

```{r}
outlier(
  x = DADOSEXP2$yield,
  logical = TRUE
) %>% as_tibble %>% 
  bind_cols(DADOSEXP2)
```

```{r}
require(lattice)
xyplot(yield~gen, data=DADOSEXP2, jitter.x=T, type=c("p","a"))
```

```{r}
analise2<-aov(yield ~ rep + gen, data=DADOSEXP2)
```

```{r}
shapiro.test(analise2$residuals)
bartlett.test(residuals(analise2)~DADOSEXP2$gen)
car::dwt(lm(yield ~ rep + gen, data=DADOSEXP2))
asbio::tukey.add.test(DADOSEXP2$yield, DADOSEXP2$rep, DADOSEXP2$gen)
```

```{r}
anova(analise2)
agricolae::cv.model(analise2)
```

```{r}
print(agricolae::HSD.test(analise2, "gen", group = TRUE)$groups)
plot(agricolae::HSD.test(analise2, "gen", group = TRUE))
```

```{r}
library("ScottKnott")
agrupamento2<-SK(analise2, which="gen")
summary(agrupamento2)
plot(agrupamento2, title='Agrupamento dos Genótipos de Milho - Ambiente 2')
```

```{r}
library("ggplot2")
ggplot(DADOSEXP2, aes(x = gen, y = yield, fill=gen)) +
  geom_boxplot() +
  scale_y_continuous(limits=c(1, 10)) +
  guides(fill = FALSE) +
  labs(title = "Produtividade média de genótipos de milho - Ambiente 2",
       y = expression(Produtividade*" "(Kg*" "*ha^-1)), x = "Genótipos",
       caption = "Fonte: Curso Gen 2020")
```


```{r}
rank2<-data.frame(summary(agrupamento2))
colnames(rank2)<-c("Genotipos", "Media", "Letra")
```


```{r}
ggplot(rank2, aes(x = Genotipos, y = Media)) +
  geom_col(fill=cores[as.numeric(rank2$Letra)]) +
  ylim(0,10)+
    guides(fill = FALSE)+
  geom_text(aes(label = rank2$Letra), vjust = -0.5, colour=cores[as.numeric(rank2$Letra)])+
  labs(title = "Produtividade média de genótipos de milho - Ambiente 2",
       y = expression(Produtividade*" "(Kg*" "*ha^-1)), x = "Genótipos",
       caption = "Fonte: Curso Gen 2020")
```

#Análise via ExpDes.pt
```{r}
DBCAMB1<-dbc(DADOSEXP1$gen, DADOSEXP1$rep, DADOSEXP1$yield, quali = TRUE, mcomp = "sk")
```

```{r}
DBCAMB2<-dbc(DADOSEXP2$gen, DADOSEXP2$rep, DADOSEXP2$yield, quali = TRUE, mcomp = "sk")
```


#ANÁLISE CONSIDERANDO TODOS OS NOVE AMBIENTES
$$
y_{ijk} = \mu + b_{j(k)} + a_k + t_i + ta_{ik} + e_{ijk}
$$

```{r}
DADOS
str(DADOS)
```

```{r}
boxplot(DADOS$yield)
```


```{r}
conjunta<-aov(yield ~ rep%in%env + gen*env, data=DADOS)
```

```{r}
anova(conjunta)
agricolae::cv.model(conjunta)
```

```{r}
car::Anova(lm(yield ~ env:rep + env + gen + gen*env, data=DADOS)) #add type=3
```

#Interaction Plots
```{r}
with(DADOS, interaction.plot(gen, env, yield, ylab = "médias",
   xlab = "Genótiposs"))
with(DADOS, interaction.plot(env, gen, yield, ylab = "médias",
   xlab = "Ambientes"))
```


```{r}
library(emmeans)
mg<-emmeans(conjunta, ~ gen, data=DADOS)
me<-emmeans(conjunta, ~ env, data=DADOS)
mge<-emmeans(conjunta, ~ gen*env, data=DADOS)
```

#Resumindo o conjunto de dados
```{r}
m<-data.frame(mge)
m[,4:7]<-NULL
n<-data.frame(mg)
n[,3:6]<-NULL
q<-data.frame(me)
q[,3:6]<-NULL
head(n)
head(q)
head(m)
```

#Ranking dos Genótipos
```{r}
ggplot(n, aes(x = gen, y = emmean)) +
  geom_col(aes(fill=-(emmean))) +
  ylim(0,10)+
  guides(fill = FALSE)+
  labs(title = "Produtividade média geral dos genótipos de milho",
       y = expression(Produtividade*" "(Kg*" "*ha^-1)), x = "Genótipos",
       caption = "Fonte: Curso Gen 2020")
```

#Ranking dos Ambientes
```{r}
ggplot(q, aes(x = env, y = emmean)) +
  geom_col(aes(fill=-(emmean))) +
  ylim(0,15)+
  guides(fill = FALSE)+
  labs(title = "Produtividade média geral dos genótipos de milho por ambiente",
       y = expression(Produtividade*" "(Kg*" "*ha^-1)), x = "Ambiente",
       caption = "Fonte: Curso Gen 2020")
```


#Gráfico da interação
```{r}
library(reshape2)
m = melt(m, value.name="emmean")
ggplot(data=m, aes(x=env, y=emmean, group = gen, colour = gen)) +
  geom_line() +
  labs(title = "Produtividade média dos genótipos de milho",
       y = expression(Produtividade*" "(Kg*" "*ha^-1)), x = "Ambientes", 
       colour = "Genótipos",
       caption = "Fonte: Curso Gen 2020")
```



#Pacote Multi-Environment Traisl Analysis (metan) - Tiago Olivoto (2020)
```{r}
library(metan)
```

```{r}
mod<-anova_joint(DADOS, env, gen, rep, yield)
```

#médias ajustadas
```{r}
mediasge<-ge_means(DADOS, env, gen, resp = yield)
```

#Ranqueamento
```{r}
ge_winners(DADOS, env, gen, resp=yield, type = "ranks")
ge_winners(DADOS, env, gen, resp=yield, type = "winners")
```

```{r}
ge_plot(DADOS, env, gen, yield, type = 2)
```


#Sugestões de literatura

#-> Manual de Planejamento e Análise de Experimentos com R - Walmes 
#   Marques Zeviani, UFPR http://leg.ufpr.br/~walmes/mpaer/


#-> ggPlot2

https://rstudio.com/wp-content/uploads/2016/03/ggplot2-cheatsheet-portuguese.pdf
http://recologia.com.br/tag/graficos/
https://rpubs.com/mnunes/ggplot2
#Delineamento em Blocos Casualizados
Exemplo: Com a finalidade de estudar os efeitos da administração de raízes e tubérculos,
como suplementação de inverno na alimentação de vacas em lactação, considerou-se um
experimento em blocos casualizados com 4 tipos de suplementos (tratamentos) e 5 raças
(blocos). As produções médias diárias de leite (kg).

#O modelo estatístico para este delineamento é: yij = µ + ti + ßj + ?ij,

prod = c(6.4,6.2,6.2,7.1,6.6,10.9,11.6,11.4,10.4,12.4,12.0,10.9,11.5,11.1,11.8,11.2,11.6,10.9,12.1,10.1)
Prod = data.frame(Trat=factor(rep(1:4, each=5)), Blocos=factor(rep(1:5, 4)), resp=prod)
attach(Prod)
anava.bl= aov(prod~Trat+Blocos)
anova(anava.bl)
summary(anava.bl)
aov(anava.bl)

#Homogeneidade de variâncias: A aplicação do teste de Bartlett
bartlett.test(prod, Trat)

#Normalidade dos resíduos: Usa-se o teste de Shapiro-Wilks
shapiro.test(anava.bl$res)

#Aditividade: Usa-se o teste de aditividade de Tukey(1949)
require(asbio)
tukey.add.test(prod, Trat, Blocos)

#Determinar o valor q tabelado
qtt = qtukey(.95, 4, 12)
qtt

#Ddiferença mínima significativa
Delta = qtukey(.95,4,12)*sqrt(anova(anava.bl)$Mean[3]/5)
Delta

#Teste de Tukey
summary(anava.bl = aov(prod ~ Blocos + Trat))
TukeyHSD(anava.bl, "Trat", ordered = TRUE)
plot(TukeyHSD(anava.bl, "Trat"), col=’blue’, las=1)


Aula DBC para uso na aula Linear Models

mudas = read.csv("http://ecologia.ib.usp.br/bie5782/lib/exe/fetch.php?media=dados:altura-mudas.csv",header=T)
summary(mudas)

plot( altura ~ bloco + substrato , data=mudas , subset=especie=="paineira")
Hit <Return> to see next plot:
Hit <Return> to see next plot:

plot( altura ~ bloco + substrato , data=mudas , subset=especie=="tamboril")

muda.pai = lm( altura ~ as.factor(bloco) + as.factor(substrato), data=mudas, subset= especie=="paineira" )
class(muda.pai)

muda.tam = lm( altura ~ as.factor(bloco) + as.factor(substrato), data=mudas, subset= especie=="tamboril" )
class(muda.tam)

anova(muda.pai)

# Ramalho, M. A. P., Ferreira, D. F., & Oliveira, A. C. de. (2005). Experimentação
# em genética e melhoramento de plantas (2nd ed., p. 322). Editora UFLA.
# Ramalho et al, tabela 1 página 102.
# Volume de madeira por �rvore, m³ e-4, para 10 progenies de E. camaldulensis
# em 3 blocos e 6 árvores por bloco:progenie.

anovadentro<- read.table("C:/Users/Vander/Documents/R/An�lise de experimento em delineamento dbc com repeti��es dentro.txt", header=TRUE, sep="\t", colClasses=c("factor","factor",NA,NA))
print(anovadentro)
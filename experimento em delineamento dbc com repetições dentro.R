# Ramalho, M. A. P., Ferreira, D. F., & Oliveira, A. C. de. (2005). Experimenta√ß√£o
# em gen√©tica e melhoramento de plantas (2nd ed., p. 322). Editora UFLA.
# Ramalho et al, tabela 1 p√°gina 102.
# Volume de madeira por √rvore, m¬≥ e-4, para 10 progenies de E. camaldulensis
# em 3 blocos e 6 √°rvores por bloco:progenie.

anovadentro<- read.table("C:/Users/Vander/Documents/R/An·lise de experimento em delineamento dbc com repetiÁıes dentro.txt", header=TRUE, sep="\t", colClasses=c("factor","factor",NA,NA))
print(anovadentro)
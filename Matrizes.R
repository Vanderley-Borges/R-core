A = matrix(data=c(7,18,-2,22,-16,3,55,1,9,-4,0,31),byrow=TRUE,nrow=3,ncol=4)
A
# Check the dimensions
dim(A)

# Transposta de A
B=t(A)
B
A = matrix(data=c(7,18,-2,22,-16,3,55,1,9,-4,0,31,8,5,2,8),byrow=TRUE,nrow=4,ncol=4)
#inversa da matriz A
solve(A)

A = matrix(data=c(7,18,-2,-16,3,55,9,-4,0),byrow=TRUE,nrow=3,ncol=3)
A
solve(A)
B=t(A)

#Traço
trA = sum(diag(A))

A = matrix(c(7,18,-2,-16,3,55,9,-4,0),3,3))
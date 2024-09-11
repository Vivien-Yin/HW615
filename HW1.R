### Liwen Yin ###

### Homework 1                                         ###
### Standard R ###

#Put your code in this file. Make sure you assign the relevant values to the correct variable names, which are given below. 
#Uncomment the variables as you assign your final values/functions/results to them.

### VECTORS ###
## Q1 

v1 <- c(1,2,3,4,5,6,7,8,7,6,5,4,3,2,1)
v_2<-c(9,4,1)
v2 <- rep(v_2,11,length.out=31)

e1 <- seq(3, 36, by = 3)
e2 <- c(1:12)
e3 <- c(rbind(e1, e2))
v3 <- sapply(1:length(e3), function(i) {
  if (i %% 2 == 1) {
    return(0.1^e3[i])  
  } else {
    return(0.2^e3[i])  
  }
})

  
### MATRICES ###

## Q2 
matA<-matrix(0,nrow = 6,ncol = 6)
matA
#the row/col position of each element in the matrix
row(matA)
col(matA)
matA[row(matA)-col(matA) == 1 | col(matA)-row(matA) == 1] <-1

## Q3
set.seed(42)
matB<- matrix(sample(10,size = 60, replace = T),nrow = 6, ncol = 10)
matB
a3 <- apply(matB, 1, function(r) length(which(r > 4)))
b3 <- which(apply(matB, 1, function(row) sum(row == 7) == 2))
a3
b3
## Q4

tmpFn<- function(xVec) {
  yVec <- numeric(length(xVec))
  yVec[xVec < 0] <- xVec[xVec < 0]^2 + 2 * xVec[xVec < 0] + 3
  yVec[xVec >= 0 & xVec < 2] <- xVec[xVec >= 0 & xVec < 2] + 3
  yVec[xVec >= 2] <- xVec[xVec >= 2]^2 + 4 * xVec[xVec >= 2] - 7
  return(yVec)
}
#test:
xVec <- seq(-3,3,length.out = 1000)
yVec <- tmpFn(xVec)
plot(xVec,yVec,main = "Plot of f(x) for -3 <= x <= 3", xlab = "x", ylab = "f(x)")
## Q5

quadmap<- function(start,rho,niter){
  x<-numeric(niter)
  x[1]<- start
  for(k in 2:niter){
    x[k]=rho*x[k-1]*(1-x[k-1])
  }
  return(x)
}
#x should ->0.5 as n-> bigger
tmp <- quadmap(start=0.95, rho=2.99, niter=500)
print(tmp)
plot(tmp[300:500], type="l")

## Q6
In1<- function(xVec, yVec) {
comparison_matrix <- outer(yVec, xVec, "<")
# Turn into 0 and 1, and sum them up
zVec <- colSums(comparison_matrix)#number of elements in y smaller than x
return(zVec)
}
In2<- function(xVec, yVec) {
zVec <- sapply(xVec, function(x) sum(yVec < x))#sum function is used to sum up the number of elements in x is bigger than y
return(zVec)
}

## Q7
fibonacci<- function(n){
  a=b=1
  for (i in 1:n) {
    temp<- a
    a<- b
    b<- a + temp
  }
  print(temp)
}

## Q8


product <- 1
n <- 1
while (product <= 10000000) {
    n <- n + 1
    product <- product * n
}
print(n)

## Q9
sum_matrix <- matrix(1:1000000, nrow = 100000, ncol = 10)
  # Row sums using a for-loop
rowAdd <- numeric(100000)
  for (i in 1:100000) {
    rowAdd[i] <- sum(sum_matrix[i, ])
  }
# Row sums using apply function
rowApply <- apply(sum_matrix, 1, sum)
# Row sums using built-in function
rowSums <- rowSums(sum_matrix)
print(rowAdd )

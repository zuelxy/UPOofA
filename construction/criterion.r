A_matrix <- function(m){
  A <- matrix(ncol = m, nrow = m)
  for (i in 1:m) {
    for (j in 1:m) {
      a <- 1-abs(i-j)/m
      A[i, j] <- a
    }
  }
  return(A)
}

permanent <- function(x) {
  n <- nrow(x)
  s <- 0
  if (n > 2) {
    for (j in 1:n) {
      s <- s + x[1, j] * Recall(x[-1, -j])
    }
    return(s)
  } else {
    x[1, 1] * x[2, 2] + x[1, 2] * x[2, 1]
  }
}


AD_OofA <- function(x){
  n <- nrow(x)
  m <- ncol(x)
  
  if(m<20)
    firC <- perm_data[which(perm_data[,1]==m), 2]
   else
    firc <- permanent(A_matrix(m))/factorial(m)
  
  Q <- 0
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      term <- prod((1-abs(x[i,]-x[j,])/m))
      Q <- Q+term
    }
  }
  y <- -firC+1/n+2*Q/(n^2)
  return(y)
}


ADOofA_LB <- function(n, m) {

  if(m<20)
    firC <- perm_data[which(perm_data[,1]==m), 2]
  else
    firc <- permanent(A_matrix(m))/factorial(m)
  
  product_term <- 1
  for (k in 1:(m-1)) {
    # (k/m)^{2nk/(m(n-1))}
    exponent <- (2 * n * k) / (m * (n - 1))
    product_term <- product_term * (k / m)^exponent
  }
  
  result <- 1/n-firC+((n - 1) / n) * product_term
  return(result)
}


perm_data <- matrix(c(
  3, 0.382716, 4, 0.239583, 5, 0.152608, 6, 0.098238, 7, 0.063644, 
  8, 0.041399, 9, 0.027002, 10, 0.017645, 11, 0.011546, 12, 0.007563,
  13, 0.004958, 14, 0.003252), ncol=2, byrow = T)

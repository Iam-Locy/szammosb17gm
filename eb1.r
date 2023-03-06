lvm <- function(n, r, a){
  dn <- r * (1 - a %*% n) * n
  return(dn)
}

N <- c(0.4,0.6)
R <- c(1.5,1.1)
A <- matrix(c(1,2.0,0.4,1),2,2)

lvm(N,R,A)
R[2] * N[2] * (1-(A[2,1]*N[1]+A[2,2]*N[2]))

out1 <- c(N[1])
out2 <- c(N[2])


for(i in (1:20)){
  o <- lvm(N,R,A)
  N <- N + o
  out1 <- c(out1, N[1])
  out2 <- c(out2, N[2])
}

plot(0:20,out1,type="l", col="red",ylim=c(0,1))
lines(0:20,out2,col="green")

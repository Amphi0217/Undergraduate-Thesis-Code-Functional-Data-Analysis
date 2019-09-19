slice_func<-function(x1,y1,h)
{n=nrow(x1)
p=ncol(x1)
mx <- t(matrix(apply(x1, 2, mean), p, n))
x1 <- (x1 - mx)
dots = rep(0, h + 1)
slicevar=matrix(0,p,p)
a = sort(y1)
dots[1] = a[1]
k1=floor(n/h)
for(i in 2:h) {
  dots[i] = a[(i - 1) * k1 + 1]
}
dots[h + 1] = a[n] + 0.001

ai = rep(0, h)
mu = array(0, dim = c(1, p, h))
for(j in 1:h) {indictor=which(y1>=dots[j]&y1<dots[j+1])
ai[j]=length(indictor)
mu[,,j]=apply(x1[indictor,],2,mean)
slicevar = slicevar + (ai[j]/n) * (mu[,  , j]) %*% t(mu[,  , j])
}
#sy=rep(0,h)
#for(s in 1:h) {
#  if(y>=dots[s]&y<dots[s+1]){sy[s]=1}
#  }
slicevar
}

###slice
u1=rep(0,tmax);u1[1]=1
u2=rep(0,tmax);u2[1]=0
alpha=rep(0,tmax);alpha[1]=1/digamma(u2[1])
t=2
while(t<=tmax & delta<tol){
  u1[t]=mean()
  
  
}
###-------------- Defines necessary funtions  ----------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

CircReg<-function(mod1,mod2=mod1,data,tm,t.lag=1,burn=2000,flag.lk=FALSE){

#-------------------------------------------------------------------------------


library(MASS)

# Data Matrix

datose<-cbind(cos(data$theta),sin(data$theta))
n<-length(data$theta)

# Design Matrices

X.1.aux<-model.matrix(mod1,data)
p.1<-length(X.1.aux[1,])
X.1<-matrix(X.1.aux,ncol=p.1)

X.2.aux<-model.matrix(mod2,data)
p.2<-length(X.2.aux[1,])
X.2<-matrix(X.2.aux,ncol=p.2)

# Prior specification for "matrix" B.

m1<-rep(0,p.1)
L1<-0.0001*diag(p.1)
m2<-rep(0,p.2)
L2<-0.0001*diag(p.2)

# Posterior specification for "matrix" B.

XtX.1<-crossprod(X.1)
XtX.2<-crossprod(X.2)
Lstar1<-L1+XtX.1
Lstar2<-L2+XtX.2
Sigma1<-chol2inv(chol(Lstar1))
Sigma2<-chol2inv(chol(Lstar2))
L1m1<-crossprod(L1,m1)
L2m2<-crossprod(L2,m2)

###----------------------  GIBBS SAMPLING   -----------------------------------
#cat("Starting Gibbs Sampler ....", fill = TRUE)

### Initial values

r<-rep(1,n)

### Number of iterations

kk<-tm*t.lag
print(paste(" Total iterations =", kk, " ..."))

### Matrices for getting final sample.

B1<-matrix(0,tm,p.1)
B2<-matrix(0,tm,p.2)
R<-matrix(0,kk,n)

#------------------ Burn-in period ----------------------

for(k in 1:burn)
{

Y<-r*datose

# Sampling of vectors beta1 and beta2.

XtY1<-crossprod(X.1,Y[,1])
XtY2<-crossprod(X.2,Y[,2])
mstar1<-c( crossprod(Sigma1,((L1m1)+XtY1) ) )
mstar2<-c( crossprod(Sigma2,((L2m2)+XtY2) ) )
beta1<-mvrnorm(1,mstar1,Sigma1)
beta2<-mvrnorm(1,mstar2,Sigma2)


# Sampling of vector r

for(j in 1:n)
  {
    t.aux<-data$theta[j]
    mu.b1<-c(crossprod(beta1,X.1[j,]))
    mu.b2<-c(crossprod(beta2,X.2[j,]))
    b<-Dbd(t.aux,mu.b1,mu.b2)
    r[j]<-b+(  pnorm(b)/( dnorm(b)+b*pnorm(b) )   )
  }

}

### ---------------------------- Iterations ------------------------------------

for(k in 1:kk)
{

flag1<-(k/1000)-trunc(k/1000)
if(flag1==0){print(k)}


# Sampling of vectors beta1 and beta2.
XtY1<-crossprod(X.1,Y[,1])
XtY2<-crossprod(X.2,Y[,2])
mstar1<-c( crossprod(Sigma1,((L1m1)+XtY1) ) )
mstar2<-c( crossprod(Sigma2,((L2m2)+XtY2) ) )
beta1<-mvrnorm(1,mstar1,Sigma1)
beta2<-mvrnorm(1,mstar2,Sigma2)

# Sampling of vector r

for(j in 1:n)
  {
    t.aux<-data$theta[j]
    mu.b1<-c(crossprod(beta1,X.1[j,]))
    mu.b2<-c(crossprod(beta2,X.2[j,]))
    r[j]<-Slice(t.aux, mu.b1, mu.b2,  r[j])
  }

Y<-r*datose


#---- Values of each iteration ------------------------------------------------

flag<-(k/t.lag)-trunc(k/t.lag)
if(flag==0)
{
ii<-k/t.lag

B1[ii,]<-beta1
B2[ii,]<-beta2
R[ii,]<-r

}
#-------------  Gibbs completed ------------------------------------------------
}
# Out:

colnames(B1)<-c(1:ncol(B1))
colnames(B2)<-c(1:ncol(B2))
colnames(R)<-c(1:ncol(R))

return(cbind (B1, B2))

}

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
## Slice sampler.

Slice<-function(t, mu1, mu2, r){
  b<-Dbd(t,mu1,mu2)
  y<-runif(1,0,exp(-0.5*(r-b)^2))
  u<-runif(1,0,1)
  r1<-b+max(c(-b,-sqrt(-2*log(y))))
  r2<-b+sqrt(-2*log(y))
  rnew<-sqrt(((r2^2-r1^2)*u)+r1^2)
  drop(rnew)
}

Dbd<-function(t,mu1,mu2) { cos(t)*mu1+sin(t)*mu2  }
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------



### R code from vignette source 'MCMCadditionalmaterial.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: MCMCadditionalmaterial.Rnw:130-132
###################################################
x<-rnorm(1000,mean=0,sd=1)
mean(x) ## cf. analytical value 0


###################################################
### code chunk number 2: MCMCadditionalmaterial.Rnw:137-142
###################################################
counts<-table(x<1.96)[2]
## pretty close to the theoretical value:
counts/length(x)
## theoretical value:
pnorm(1.96)


###################################################
### code chunk number 3: MCMCadditionalmaterial.Rnw:175-177
###################################################
indep.samp<-rnorm(500,mean=0,sd=1)
head(indep.samp,n=3)


###################################################
### code chunk number 4: indepsamp
###################################################
plot(1:500,indep.samp,type="l")


###################################################
### code chunk number 5: markovchainexample
###################################################
nsim<-500
x<-rep(NA,nsim)
y<-rep(NA,nsim)
x[1]<-rnorm(1) ## initialize x
for(i in 2:nsim){
## draw i-th value based on i-1-th value:  
y[i]<-rnorm(1,mean=x[i-1],sd=1)
x[i]<-y[i]
}
plot(1:nsim,y,type="l")


###################################################
### code chunk number 6: MCMCadditionalmaterial.Rnw:220-221
###################################################
nsim<-500
x<-rep(NA,nsim)
y<-rep(NA,nsim)
x[1]<-rnorm(1) ## initialize x
for(i in 2:nsim){
## draw i-th value based on i-1-th value:  
y[i]<-rnorm(1,mean=x[i-1],sd=1)
x[i]<-y[i]
}
plot(1:nsim,y,type="l")


###################################################
### code chunk number 7: MCMCadditionalmaterial.Rnw:266-277
###################################################
## Set up transition matrix:
T<-matrix(rep(0,36),nrow=6)
diag(T)<-0.5
offdiags<-c(rep(0.25,4),0.5)
for(i in 2:6){
T[i,i-1]<-offdiags[i-1]
}
offdiags2<-c(0.5,rep(0.25,4))
for(i in 1:5){
T[i,i+1]<-offdiags2[i]
}


###################################################
### code chunk number 8: MCMCadditionalmaterial.Rnw:286-287
###################################################
T


###################################################
### code chunk number 9: MCMCadditionalmaterial.Rnw:292-293
###################################################
rowSums(T)


###################################################
### code chunk number 10: MCMCadditionalmaterial.Rnw:303-304
###################################################
T[1,]


###################################################
### code chunk number 11: MCMCadditionalmaterial.Rnw:309-312
###################################################
sample(1:6,size=1,prob=T[1,])
sample(1:6,size=1,prob=T[1,])
sample(1:6,size=1,prob=T[1,])


###################################################
### code chunk number 12: randomwalk
###################################################
nsim<-500
s<-rep(0,nsim)
## initialize:
s[1]<-3
for(i in 2:nsim){
  s[i]<-sample(1:6,size=1,prob=T[s[i-1],])
}

plot(1:nsim,s,type="l",main="States visited")


###################################################
### code chunk number 13: MCMCadditionalmaterial.Rnw:340-341
###################################################
nsim<-500
s<-rep(0,nsim)
## initialize:
s[1]<-3
for(i in 2:nsim){
  s[i]<-sample(1:6,size=1,prob=T[s[i-1],])
}

plot(1:nsim,s,type="l",main="States visited")


###################################################
### code chunk number 14: MCMCadditionalmaterial.Rnw:355-371
###################################################
nsim<-50000
s<-rep(0,nsim)
## initialize:
s[1]<-3
for(i in 2:nsim){
  s[i]<-sample(1:6,size=1,prob=T[s[i-1],])
}

blocks<-seq(500,50000,by=500)
n<-length(blocks)
## store transition probs over increasing blocks:
store.probs<-matrix(rep(rep(0,6),n),ncol=6)
## compute relative frequencies over increasing blocks:
for(i in 1:n){
  store.probs[i,]<-table(s[1:blocks[i]])/blocks[i]
}


###################################################
### code chunk number 15: convergence
###################################################
op <- par(mfrow=c(3,2))
for(i in 1:6){
plot(1:n,store.probs[,i],type="l",lty=1,xlab="block",
     ylab="probability",main=paste("State ",i,sep=""))
}


###################################################
### code chunk number 16: MCMCadditionalmaterial.Rnw:393-394
###################################################
op <- par(mfrow=c(3,2))
for(i in 1:6){
plot(1:n,store.probs[,i],type="l",lty=1,xlab="block",
     ylab="probability",main=paste("State ",i,sep=""))
}


###################################################
### code chunk number 17: MCMCadditionalmaterial.Rnw:403-404
###################################################
store.probs[1,]


###################################################
### code chunk number 18: MCMCadditionalmaterial.Rnw:409-410
###################################################
(w<-store.probs[n,])


###################################################
### code chunk number 19: MCMCadditionalmaterial.Rnw:420-422
###################################################
round(w%*%T,digits=2)
round(w,digits=2)


###################################################
### code chunk number 20: MCMCadditionalmaterial.Rnw:570-576
###################################################
## initial value:
theta.t<-0
## always give the same result:
set.seed(43210)
## generate candidate:
(Y<-rnorm(1,mean=theta.t,sd=1))


###################################################
### code chunk number 21: MCMCadditionalmaterial.Rnw:597-598
###################################################
(U <- runif(1,0,1))


###################################################
### code chunk number 22: MCMCadditionalmaterial.Rnw:632-659
###################################################
## solution:
nsteps<-5000
chain<-rep(NA,nsteps)
init<-0
generate.candidate<-function(theta.t){
  rnorm(1,mean=theta.t,sd=1)
}
alpha<-function(theta.t,Y){
  min(1,(1+theta.t^2)/(1+Y^2))
}

theta.t<-init
chain[1]<-theta.t

for(i in 2:nsteps){
  Y<-generate.candidate(theta.t)
  prob<-alpha(theta.t,Y)
  U <- runif(1,0,1)
  if(U <= prob){
    theta.t <- Y
  } else {
    if(U > prob){
    theta.t<-theta.t
  }  
  }
  chain[i]<-theta.t
}


###################################################
### code chunk number 23: MCMCadditionalmaterial.Rnw:662-667 (eval = FALSE)
###################################################
## ## probability of 0<theta<1:
## counts<-table(0<chain & chain<1)
## counts[2]/sum(counts)
## ## theoretical value:
## pcauchy(1)-pcauchy(0)


###################################################
### code chunk number 24: MCMCadditionalmaterial.Rnw:670-700
###################################################
## multiple chain version:
nchains<-3
nsteps<-5000
chains<-matrix(rep(NA,nchains*nsteps),ncol=nsteps)
init.vector<-c(-100,0,100)
generate.candidate<-function(theta.t){
  rnorm(1,mean=theta.t,sd=1)
}
alpha<-function(theta.t,Y){
  min(1,(1+theta.t^2)/(1+Y^2))
}


for(j in 1:nchains){
  theta.t<-init.vector[j]
  chains[j,1]<-theta.t
for(i in 2:nsteps){
  Y<-generate.candidate(theta.t)
  prob<-alpha(theta.t,Y)
  U <- runif(1,0,1)
  if(U <= prob){
    theta.t <- Y
  } else {
    if(U > prob){
    theta.t<-theta.t
  }  
  }
  chains[j,i]<-theta.t
}
}


###################################################
### code chunk number 25: MCMCadditionalmaterial.Rnw:702-708 (eval = FALSE)
###################################################
## ## probability of 0<theta<1:
## burnedin.chains<-chains[,2001:5000]
## counts<-table(0<burnedin.chains[1:3,] & burnedin.chains[1:3,]<1)
## counts[2]/sum(counts)
## ## theoretical value:
## pcauchy(1)-pcauchy(0)


###################################################
### code chunk number 26: MCMCadditionalmaterial.Rnw:716-717
###################################################
plot(1:nsteps,chain,type="l")


###################################################
### code chunk number 27: MCMCadditionalmaterial.Rnw:725-728
###################################################
plot(1:nsteps,chains[1,],type="l",ylim=c(-100,100))
lines(1:nsteps,chains[2,],col="red")
lines(1:nsteps,chains[3,],col="orange")



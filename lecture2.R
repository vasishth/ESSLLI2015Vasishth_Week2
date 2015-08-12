## ----setup,include=FALSE,cache=FALSE-------------------------------------
library(knitr)
library(coda)

# set global chunk options, put figures into folder
options(replace.assign=TRUE,show.signif.stars=FALSE)
opts_chunk$set(fig.path='figures/figure-', fig.align='center', fig.show='hold')
options(replace.assign=TRUE,width=75)
opts_chunk$set(dev='postscript')

## ------------------------------------------------------------------------
pnorm(600,mean=600,sd=sqrt(50))

## ------------------------------------------------------------------------
qnorm(0.5,mean=600,sd=sqrt(50))

## ------------------------------------------------------------------------
qnorm(0.5,mean=600,sd=sqrt(50),lower.tail=FALSE)

## ------------------------------------------------------------------------
pnorm(610,mean=600,sd=sqrt(50))-
  pnorm(490,mean=600,sd=sqrt(50))

## ------------------------------------------------------------------------
x<-rnorm(10000,mean=600,sd=sqrt(50))
## proportion of cases where 
## x is less than 500:
mean(x<590)
## theoretical value:
pnorm(590,mean=600,sd=sqrt(50))

## ----echo=FALSE,fig.height=4---------------------------------------------
plot(function(x) dnorm(x), -6, 6,
      main = "Normal density N(0,1)",ylim=c(0,.4),
              ylab="density",xlab="X")

## ------------------------------------------------------------------------
pnorm(-2)

## ------------------------------------------------------------------------
##P(Z < -x):
pnorm(-2)
##P(Z > x):
pnorm(2,lower.tail=FALSE)

## ----echo=FALSE,fig.height=4---------------------------------------------
## code source: http://www.r-bloggers.com/creating-shaded-areas-in-r/
coord.x <- c(-3,seq(-3,-2,0.01),-2) 
 coord.y <- c(0,dnorm(seq(-3,-2,0.01)),0) 
 curve(dnorm(x,0,1),xlim=c(-3,3),main='Standard Normal') 
 polygon(coord.x,coord.y,col='skyblue')
text(x=-2.5,y=0.1,label=expression(phi(-2)))

## ------------------------------------------------------------------------
round(qnorm(0.025,lower.tail=FALSE),digits=2)

## ------------------------------------------------------------------------
x<-rnorm(1,mean=600,sd=sqrt(50))

## ----echo=FALSE,fig.height=3---------------------------------------------
mu<-seq(400,800,by=0.1)
plot(mu,dnorm(x,mean=mu,sd=sqrt(50)),type="l",
     ylab="")

## ------------------------------------------------------------------------
x<-rnorm(10,mean=600,sd=sqrt(50))

## ------------------------------------------------------------------------
## mu = 500
dnorm(x,mean=500,sd=sqrt(50))

## ------------------------------------------------------------------------
## mu = 500
sum(dnorm(x,mean=500,sd=sqrt(50),log=TRUE))

## ----fig.height=2.5------------------------------------------------------
mu<-seq(400,800,by=0.1)
liks<-rep(NA,length(mu))
for(i in 1:length(mu)){
liks[i]<-sum(dnorm(x,mean=mu[i],sd=sqrt(50),log=TRUE))
}
plot(mu,liks,type="l")

## ------------------------------------------------------------------------
dbinom(x=46,size=100,0.4)
dbinom(x=46,size=100,0.46)
dbinom(x=46,size=100,0.5)
dbinom(x=46,size=100,0.6)

## ----echo=FALSE,fig.height=3---------------------------------------------
theta<-seq(0,1,by=0.01)
plot(theta,dbinom(x=46,size=100,theta),
     xlab=expression(theta),type="l")

## ----betas,echo=FALSE,fig.height=4---------------------------------------
plot(function(x) 
  dbeta(x,shape1=1,shape2=1), 0,1,
      main = "Beta density",
              ylab="density",xlab="X",ylim=c(0,4))
text(.5,.9,"a=1,b=1")


plot(function(x) 
  dbeta(x,shape1=2,shape2=2), 0,1,
      main = "Beta density",
              ylab="density",xlab="X",ylim=c(0,4),add=T)

text(.5,1.3,"a=2,b=2")

plot(function(x) 
  dbeta(x,shape1=3,shape2=3),0,1,add=T)
text(.5,1.8,"a=3,b=3")


plot(function(x) 
  dbeta(x,shape1=6,shape2=6),0,1,add=T)
text(.5,2.6,"a=6,b=6")

plot(function(x) 
  dbeta(x,shape1=10,shape2=10),0,1,add=T)
text(.5,3.5,"a=60,b=60")

## ----echo=F,fig.height=5-------------------------------------------------
##lik:
plot(function(x) 
  dbeta(x,shape1=46,shape2=54),0,1,
              ylab="",xlab="X",col="red",
  ylim=c(0,10))

## prior:
plot(function(x) 
  dbeta(x,shape1=2,shape2=2), 0,1,
      main = "Prior",
              ylab="density",xlab="X",add=T,lty=2)

## posterior
plot(function(x) 
  dbeta(x,shape1=48,shape2=56), 0,1,
      main = "Posterior",
              ylab="density",xlab="X",add=T)

legend(0.1,6,legend=c("post","lik","prior"),
       lty=c(1,1,2),col=c("black","red","black"))

## ----echo=F,fig.height=5-------------------------------------------------
##lik:
plot(function(x) 
  dbeta(x,shape1=46,shape2=54),0,1,
              ylab="",xlab="X",col="red",
  ylim=c(0,10))

## prior:
plot(function(x) 
  dbeta(x,shape1=6,shape2=6), 0,1,
      main = "Prior",
              ylab="density",xlab="X",add=T,lty=2)

## posterior
plot(function(x) 
  dbeta(x,shape1=52,shape2=60), 0,1,
      main = "Posterior",
              ylab="density",xlab="X",add=T)

legend(0.1,6,legend=c("post","lik","prior"),
       lty=c(1,1,2),col=c("black","red","black"))

## ----echo=F,fig.height=5-------------------------------------------------
##lik:
plot(function(x) 
  dbeta(x,shape1=46,shape2=54),0,1,
              ylab="",xlab="X",col="red",
  ylim=c(0,10))

## prior:
plot(function(x) 
  dbeta(x,shape1=21,shape2=21), 0,1,
      main = "Prior",
              ylab="density",xlab="X",add=T,lty=2)

## posterior
plot(function(x) 
  dbeta(x,shape1=67,shape2=75), 0,1,
      main = "Posterior",
              ylab="density",xlab="X",add=T)

legend(0.1,6,legend=c("post","lik","prior"),
       lty=c(1,1,2),col=c("black","red","black"))

## ------------------------------------------------------------------------
round(qbeta(0.025,shape1=100,shape2=100),digits=1)
round(qbeta(0.975,shape1=100,shape2=100),digits=1)
## ambivalent as to whether theta <0.5 or not:
round(pbeta(0.5,shape1=100,shape2=100),digits=1)

## ------------------------------------------------------------------------
qbeta(0.5,shape1=589,shape2=611,lower.tail=FALSE)

## ------------------------------------------------------------------------
beautydata<-read.table("data/beauty.txt",header=T)
## Note: beauty level is centered.
head(beautydata)

## ------------------------------------------------------------------------
## restate the data as a list for JAGS:
data<-list(x=beautydata$beauty,
           y=beautydata$evaluation)

## ------------------------------------------------------------------------
library(rjags)

## ------------------------------------------------------------------------
cat("model{
    ## specify model for data:
    for(i in 1:463){ 
    y[i] ~ dnorm(mu[i],tau)
    mu[i] <- beta0 + beta1 * (x[i])
    }
    # priors:
    beta0 ~ dunif(-10,10)
    beta1 ~ dunif(-10,10)
    sigma ~ dunif(0,100)
    sigma2 <- pow(sigma,2)
    tau <- 1/sigma2
   }",
     file="JAGSmodels/beautyexample1.jag" )

## ----cache=TRUE----------------------------------------------------------
## specify which variables you want to examine
## the posterior distribution of:
track.variables<-c("beta0","beta1","sigma")

## define model:
beauty.mod <- jags.model( 
  file = "JAGSmodels/beautyexample1.jag",
                     data=data,
                     n.chains = 2,
                     n.adapt =2000, 
                     quiet=T)

## sample from posterior:
beauty.res <- coda.samples(beauty.mod,
                          var = track.variables,
                          n.iter = 2000,
                          thin = 1 ) 

## ------------------------------------------------------------------------
round(summary(beauty.res)$statistics[,1:2],digits=2)
round(summary(beauty.res)$quantiles[,c(1,3,5)],digits=2)

## ------------------------------------------------------------------------
lm_summary<-summary(lm(evaluation~beauty,
                       beautydata))

round(lm_summary$coef,digits=2)
round(lm_summary$sigma,digits=2)

## ----fig.height=2--------------------------------------------------------
op<-par(mfrow=c(1,3),pty="s")
library(coda)
traceplot(beauty.res)

## ----echo=FALSE,fig.height=3---------------------------------------------
MCMCsamp<-as.matrix(beauty.res)
op<-par(mfrow=c(1,3),pty="s")
hist(MCMCsamp[,1],main=expression(beta[0]),
     xlab="",freq=FALSE)
hist(MCMCsamp[,2],main=expression(beta[1]),
     xlab="",freq=FALSE)
hist(MCMCsamp[,3],main=expression(sigma),
     xlab="",freq=FALSE)

## ------------------------------------------------------------------------
data<-list(x=c(8,15,22,29,36),
           y=c(177,236,285,350,376))

## ------------------------------------------------------------------------
lm_summary_rats<-summary(fm<-lm(y~x,data))
round(lm_summary_rats$coef,digits=3)

## ------------------------------------------------------------------------
cat("
model
   {
    ## specify model for data:
    for(i in 1:5){ 
    y[i] ~ dnorm(mu[i],tau)
    mu[i] <- beta0 + beta1 * (x[i]-mean(x[]))
    }
    # priors:
    beta0 ~ dunif(-500,500)
    beta1 ~ dunif(-500,500)
    tau <- 1/sigma2
    sigma2 <-pow(sigma,2)
    sigma ~ dunif(0,200)
   }",
     file="JAGSmodels/ratsexample2.jag" )

## ----echo=FALSE,eval=FALSE,label="logsigma"------------------------------
## cat("
## model
##    {
##     ## specify model for data:
##     for(i in 1:5){
##     y[i] ~ dnorm(mu[i],tau)
##     mu[i] <- beta0 + beta1 * (x[i]-mean(x[]))
##     }
##     # priors:
##     beta0 ~ dunif(-500,500)
##     beta1 ~ dunif(-500,500)
##     tau <- 1/sigma2
##     sigma <-pow(sigma2,1/2)
##     #sigma ~ dunif(0,200)
##     log(sigma2) <- 2* log.sigma
##     log.sigma ~ dunif(0,8)
##    }",
##      file="JAGSmodels/ratsexample2llogsigma.jag" )
## 
## track.variables<-c("beta0","beta1","sigma")
## 
## ## define model:
## rat.mod <- jags.model(
##   file = "JAGSmodels/ratsexample2llogsigma.jag",
##                      data=data,
##                      n.chains = 4,
##                      n.adapt =2000,
##                      quiet=T)
## 
## ## sample from posterior:
## rat.res <- coda.samples(rat.mod,
##                           var = track.variables,
##                           n.iter = 2000,
##                           thin = 1 )
## 
## summary(rat.res)$statistics[,1:2]

## ------------------------------------------------------------------------
lexdec<-read.table("data/lexdec.txt",header=TRUE)
data<-lexdec[,c(1,2,3,4,5)]

contrasts(data$NativeLanguage)<-contr.sum(2)
contrasts(data$Sex)<-contr.sum(2)

## ------------------------------------------------------------------------
lm_summary_lexdec<-summary(fm<-lm(RT~
  scale(Trial,scale=F)+
  NativeLanguage+Sex,data))

round(lm_summary_lexdec$coef[,1:2],digits=2)

## ------------------------------------------------------------------------
contrasts(data$NativeLanguage)
contrasts(data$Sex)

## ------------------------------------------------------------------------
## redo contrasts as vectors:
eng<-ifelse(data$NativeLanguage=="English",1,-1)
sex<-ifelse(data$Sex=="F",1,-1)

## ------------------------------------------------------------------------
dat<-list(y=data$RT,
          Trial=(data$Trial-mean(data$Trial)),
          Lang=eng,
          Sex=sex)

## ------------------------------------------------------------------------
cat("
model
   {
    ## specify model for data:
    for(i in 1:1659){ 
    y[i] ~ dnorm(mu[i],tau)
    mu[i] <- beta0 + 
             beta1 * Trial[i]+
             beta2 * Lang[i] +  beta3 * Sex[i] 
    }
    # priors:
    beta0 ~ dunif(-10,10)
    beta1 ~ dunif(-5,5)
    beta2 ~ dunif(-5,5)
    beta3 ~ dunif(-5,5)
    tau <- 1/sigma2
    sigma2 <-pow(sigma,2)
    sigma ~ dunif(0,200)
   }",
     file="JAGSmodels/multregexample1.jag" )

## ----cache=TRUE----------------------------------------------------------
track.variables<-c("beta0","beta1",
                   "beta2","beta3","sigma")
library(rjags)

lexdec.mod <- jags.model( 
  file = "JAGSmodels/multregexample1.jag",
                     data=dat,
                     n.chains = 2,
                     n.adapt =2000, 
                      quiet=T)

lexdec.res <- coda.samples( lexdec.mod,
                                 var = track.variables,
                              n.iter = 3000)

## ------------------------------------------------------------------------
round(summary(lexdec.res)$statistics[,1:2],
      digits=2)

## ------------------------------------------------------------------------
round(summary(lexdec.res)$quantiles[,c(1,3,5)],
      digits=2)

## ------------------------------------------------------------------------
beetledata<-read.table("data/beetle.txt",header=T)
head(beetledata)

## ------------------------------------------------------------------------
dat<-list(x=beetledata$dose-mean(beetledata$dose),
          n=beetledata$number,
          y=beetledata$killed)

## ------------------------------------------------------------------------
cat("
model
   {
for(i in 1:8){
    y[i] ~ dbin(p[i],n[i])
    logit(p[i]) <- beta0 + beta1 * x[i]
}
    # priors:
     beta0 ~ dunif(0,100)
     beta1 ~ dunif(0,100)
   }",
     file="JAGSmodels/glmexample1.jag" )

## ----cache=TRUE----------------------------------------------------------
track.variables<-c("beta0","beta1")
## new:
inits <- list (list(beta0=0,
                    beta1=0))

glm.mod <- jags.model( 
  file = "JAGSmodels/glmexample1.jag",
                     data=dat,
                    ## new:
                    inits=inits,
                     n.chains = 1,
                      n.adapt =2000, quiet=T)

## ------------------------------------------------------------------------
glm.res <- coda.samples( glm.mod,
                          var = track.variables,
                              n.iter = 2000) 

## ------------------------------------------------------------------------
round(summary(glm.res)$statistics[,1:2],
      digits=2)
round(summary(glm.res)$quantiles[,c(1,3,5)],
      digits=2)

## ------------------------------------------------------------------------
round(coef(glm(killed/number~scale(dose,scale=F),
         weights=number,
           family=binomial(),beetledata)),
      digits=2)

## ----fig.height=5--------------------------------------------------------
plot(glm.res)

## ------------------------------------------------------------------------
data<-list(x=c(8,15,22,29,36),
           y=c(177,236,285,350,376))

## ------------------------------------------------------------------------
cat("model{
    ## specify model for data:
    for(i in 1:5){ 
    y[i] ~ dnorm(mu[i],tau)
    mu[i] <- beta0 + beta1 * (x[i]-mean(x[]))
    }
    ## prediction
    mu45 <- beta0+beta1 * (45-mean(x[]))
    y45 ~ dnorm(mu45,tau)
    # priors:
    beta0 ~ dunif(-500,500)
    beta1 ~ dunif(-500,500)
    tau <- 1/sigma2
    sigma2 <-pow(sigma,2)
    sigma ~ dunif(0,200)
   }",
     file="JAGSmodels/ratsexample2pred.jag" )

## ------------------------------------------------------------------------
track.variables<-c("beta0","beta1","sigma","y45")

rats.mod <- jags.model( 
  file = "JAGSmodels/ratsexample2pred.jag",
                     data=data,
                     n.chains = 2,
                      n.adapt =2000, quiet=T)

rats.res <- coda.samples( rats.mod,
                          var = track.variables,
                              n.iter = 2000,
                                thin = 1) 

## ------------------------------------------------------------------------
round(summary(rats.res)$statistics[,1:2],
       digits=2)

## ----fig.height=4--------------------------------------------------------
traceplot(rats.res)


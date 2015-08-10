## ----setup,include=FALSE,cache=FALSE-------------------------------------
library(knitr)
library(coda)

# set global chunk options, put figures into folder
options(replace.assign=TRUE,show.signif.stars=FALSE)
opts_chunk$set(fig.path='figures/figure-', fig.align='center', fig.show='hold')
options(replace.assign=TRUE,width=75)
opts_chunk$set(dev='postscript')

## ----echo=FALSE----------------------------------------------------------
## load data:
data<-read.table("data/gibsonwu2012data.txt",header=T)
## take reciprocal rt to normalize residuals:
data$rrt<- -1000/data$rt
## define predictor x, coding as sum contrasts:
data$x <- ifelse(
  data$type%in%c("subj-ext"),-1,1)

## ------------------------------------------------------------------------
headnoun<-subset(data,region=="headnoun")
## no. of subjects:
length(unique(headnoun$subj))
## no. of items:
length(unique(headnoun$item))
## no. of rows in data frame:
dim(headnoun)[1]

## ------------------------------------------------------------------------
head.means<-aggregate(rt~subj+type,
                      mean,data=headnoun)

## ------------------------------------------------------------------------
t.test(log(subset(head.means,type=="subj-ext")$rt),
       log(subset(head.means,type=="obj-ext")$rt),
       paired=T)
t.test(subset(head.means,
              type=="subj-ext")$rt,
       subset(head.means,
              type=="obj-ext")$rt,
paired=T)

## ------------------------------------------------------------------------
head(data[,c(1,2,3,4,7,10,11)])

## ----echo=F--------------------------------------------------------------
library(lme4)

## ------------------------------------------------------------------------
m1 <- lmer(rrt~x+(1+x|subj)+(1+x|item),
          subset(data,region=="headnoun"))

m2 <- lmer(rrt~x+(1+x|subj)+(1|item),
          subset(data,region=="headnoun"))



## ------------------------------------------------------------------------
round(summary(m1)$coefficients,digits=3)

## ------------------------------------------------------------------------
m1<- lmer(rrt~x+(1+x|subj)+(1+x|item),
          headnoun)
m1a<- lmer(rrt~x+(1|subj)+(1|item),
          headnoun)

## ------------------------------------------------------------------------
anova(m1,m1a)

## ----echo=FALSE,cache=TRUE-----------------------------------------------
new.df <- function(cond1.rt=487, effect.size=123, 
                   sdev=544,
                   sdev.int.subj=160, sdev.slp.subj=195, 
                   rho.u=0.6,
                   nsubj=37,
                   sdev.int.items=154, sdev.slp.items=142,
                   rho.w=0.6,
                   nitems=15) {
  library(MASS)
  
  ncond <- 2
  
  subj <- rep(1:nsubj, each=nitems*ncond)
  item <- rep(1:nitems, nsubj, each=ncond)
  
  cond <- rep(0:1, nsubj*nitems)
  err  <- rnorm(nsubj*nitems*ncond, 0, sdev)
  d    <- data.frame(subj=subj, item=item, 
                     cond=cond+1, err=err)
  
  Sigma.u<-matrix(c(sdev.int.subj^2,
                    rho.u*sdev.int.subj*sdev.slp.subj,
                    rho.u*sdev.int.subj*sdev.slp.subj,
                    sdev.slp.subj^2),nrow=2)
  
  Sigma.w<-matrix(c(sdev.int.items^2,
                    rho.u*sdev.int.items*sdev.slp.items,
                    rho.u*sdev.int.items*sdev.slp.items,
                    sdev.slp.items^2),nrow=2)
  
  # Adding random intercepts and slopes for subjects:
  ## first col. has adjustment for intercept, 
  ## secdon col. has adjustment for slope
  subj.rand.effs<-mvrnorm(n=nsubj,rep(0,ncond),Sigma.u)
  
  item.rand.effs<-mvrnorm(n=nitems,rep(0,ncond),Sigma.w)
  
  re.int.subj <- subj.rand.effs[,1]
  d$re.int.subj <- rep(re.int.subj, each=nitems*ncond)
  re.slp.subj   <- subj.rand.effs[,2]
  
  d$re.slp.subj <- rep(re.slp.subj, 
                       each=nitems*ncond) * (cond - 0.5)
  
  re.int.item <- item.rand.effs[,1]
  d$re.int.item <- rep(re.int.item, nsubj, each=ncond)
  re.slp.item <- item.rand.effs[,2]
  d$re.slp.item <- rep(re.slp.item, nsubj, 
                       each=ncond) * (cond - 0.5)
  
  d$rt <- (cond1.rt + cond*effect.size
           + d$re.int.subj + d$re.slp.subj
           + d$re.int.item + d$re.slp.item
           + d$err)
  
  return(list(d,cor(re.int.subj,re.slp.subj),
              cor(re.int.item,re.slp.item)))
}

## ----cache=TRUE----------------------------------------------------------
gendata<-function(subjects=37,items=15){
  dat<-new.df(nsubj=subjects,nitems=items,
              rho.u=0.6,rho.w=0.6)
  dat <- dat[[1]]
  dat<-dat[,c(1,2,3,9)]
  dat$x<-ifelse(dat$cond==1,-0.5,0.5)
  
return(dat)
}

## ------------------------------------------------------------------------
nsim<-100

## ----cache=TRUE----------------------------------------------------------
library(lme4)
subjcorr<-rep(NA,nsim)
itemcorr<-rep(NA,nsim)

for(i in 1:nsim){
dat<-gendata()  
m3<-lmer(rt~x+(1+x|subj)+(1+x|item),dat)
subjcorr[i]<-attr(VarCorr(m3)$subj,"correlation")[1,2]
itemcorr[i]<-attr(VarCorr(m3)$item,"correlation")[1,2]
}

## ----echo=FALSE,fig.height=4---------------------------------------------
op<-par(mfrow=c(1,2),pty="s")
hist(subjcorr,freq=FALSE,xlab=expression(hat(rho)[u]),
     main="Distribution of subj. corr.")
abline(v=0.6,lwd=3)
hist(itemcorr,freq=FALSE,xlab=expression(hat(rho)[w]),
     main="Distribution of item corr.")
abline(v=0.6,lwd=3)

## ----cache=TRUE----------------------------------------------------------
subjcorr<-rep(NA,nsim)
itemcorr<-rep(NA,nsim)

for(i in 1:nsim){
dat<-gendata(subjects=50,items=30)  
m3<-lmer(rt~x+(1+x|subj)+(1+x|item),dat)
subjcorr[i]<-attr(VarCorr(m3)$subj,"correlation")[1,2]
itemcorr[i]<-attr(VarCorr(m3)$item,"correlation")[1,2]
}

## ----echo=FALSE,fig.height=4---------------------------------------------
op<-par(mfrow=c(1,2),pty="s")
hist(subjcorr,freq=FALSE,xlab=expression(hat(rho)[u]),
     main="Distribution of subj. corr.")
abline(v=0.6,lwd=3)
hist(itemcorr,freq=FALSE,xlab=expression(hat(rho)[w]),
     main="Distribution of item corr.")
abline(v=0.6,lwd=3)

## ----echo=F--------------------------------------------------------------
library(lme4)

## ------------------------------------------------------------------------
m1 <- lmer(rrt~x+(1+x|subj)+(1+x|item),
          headnoun)

## ------------------------------------------------------------------------
round(summary(m1)$coefficient,digits=3)

## ----echo=FALSE,fig.height=4,cache=TRUE----------------------------------
library(mvtnorm)
u0 <- u1 <- seq(from = -3, to = 3, length.out = 30)
Sigma1<-diag(2)
f <- function(u0, u1) dmvnorm(cbind(u0, u1), mean = c(0, 0),sigma = Sigma1)
z <- outer(u0, u1, FUN = f)
persp(u0, u1, z, theta = -30, phi = 30, ticktype = "detailed")

## ----echo=FALSE,fig.height=4,cache=TRUE----------------------------------
Sigma2<-matrix(c(1,.6,.6,1),byrow=FALSE,ncol=2)
f <- function(u0, u1) dmvnorm(cbind(u0, u1), mean = c(0, 0),sigma = Sigma2)
z <- outer(u0, u1, FUN = f)
persp(u0, u1, z, theta = -30, phi = 30, ticktype = "detailed")

## ----echo=FALSE,fig.height=4,cache=TRUE----------------------------------
Sigma3<-matrix(c(1,-.6,-.6,1),byrow=FALSE,ncol=2)
f <- function(u0, u1) dmvnorm(cbind(u0, u1), mean = c(0, 0),sigma = Sigma3)
z <- outer(u0, u1, FUN = f)
persp(u0, u1, z, theta = -30, phi = 30, ticktype = "detailed")

## ----echo=FALSE,fig.height=4---------------------------------------------
se <- function(x)
      {
        y <- x[!is.na(x)] # remove the missing values, if any
        sqrt(var(as.vector(y))/length(y))
}


ci <- function (scores){
m <- mean(scores,na.rm=TRUE)
stderr <- se(scores)
len <- length(scores)
upper <- m + qt(.975, df=len-1) * stderr 
lower <- m + qt(.025, df=len-1) * stderr 
return(data.frame(lower=lower,upper=upper))
}

## sample repeatedly:
lower <- rep(NA,100)
upper <- rep(NA,100)

for(i in 1:100){ 
  sample <- rnorm(100,mean=60,sd=4)
  lower[i] <- ci(sample)$lower
  upper[i] <- ci(sample)$upper
}
  
cis <- cbind(lower,upper)

store <- rep(NA,100)

pop.mean<-60
pop.sd<-4

for(i in 1:100){ 
  sample <- rnorm(100,mean=pop.mean,sd=pop.sd)
  lower[i] <- ci(sample)$lower
  upper[i] <- ci(sample)$upper
  if(lower[i]<pop.mean & upper[i]>pop.mean){
    store[i] <- TRUE} else {
      store[i] <- FALSE}
}

## need this for the plot below:
cis <- cbind(lower,upper)

## convert store to factor:
store<-factor(store)

main.title<-"95% CIs in 100 repeated samples"

line.width<-ifelse(store==FALSE,2,1)
cis<-cbind(cis,line.width)
x<-0:100
y<-seq(55,65,by=1/10)
plot(x,y,type="n",xlab="i-th repeated sample",ylab="Scores",main=main.title)
abline(60,0,lwd=2)
x0<-x
x1<-x
arrows(x0,y0=cis[,1],
       x1,y1=cis[,2],length=0,lwd=cis[,3])


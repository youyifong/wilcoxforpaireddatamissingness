library(kyotil)
library(beanplot)
if(!unix()) setwd("D:/gdrive/wmw/partially_matched_rank/R/")
source("lib_partially_matched_rank_tests.R")


save2file=T
n=500
lim=NULL
mypdf(file="figures/boxplot_sim_gamma", save2file=save2file, mfrow=c(2,2))
    dat=sim.partially.matched(m=n,n.x=n,n.y=n,distr="gamma",params=c(loc.2=0,shape.1=2,shape.2=2,rate.1=2,rate.2=1,rho=0.5),seed=1)
    X=dat$X; Y=dat$Y; Xprime=dat$Xprime; Yprime=dat$Yprime; Xmissing=dat$Xmissing; Ymissing=dat$Ymissing
    myprint(var(X), var(Y), cor(X,Y))
    #beanplot(list(X=X, Y=c(Y,Y)), ylim=ylim, what=c(0,1,1,1))
    myboxplot(list(X=X, Y=Y))
    corplot(X,Y, xlab="X",ylab="Y", xlim=lim, ylim=lim)    
    
    dat=sim.partially.matched(m=n,n.x=n,n.y=n,distr="gamma",params=c(loc.2=0,shape.1=2,shape.2=2,rate.1=2,rate.2=1,rho=0),seed=1)
    X=dat$X; Y=dat$Y; Xprime=dat$Xprime; Yprime=dat$Yprime; Xmissing=dat$Xmissing; Ymissing=dat$Ymissing
    #myprint(var(X), var(Y), cor(X,Y))
    myboxplot(list(X=X, Y=Y))
    #beanplot(list(X=X, Y=c(Y,Y)), ylim=ylim, what=c(0,1,1,1))
    corplot(X,Y, xlab="X",ylab="Y", xlim=lim, ylim=lim)    
if(save2file)dev.off()


save2file=F
n=500
lim=NULL
mypdf(file="figures/boxplot_sim_gamma2", save2file=save2file, mfrow=c(2,2))
    dat=sim.partially.matched(m=n,n.x=n,n.y=n,distr="gamma",params=c(loc.2=.3,shape.1=3,shape.2=2,rate.1=3,rate.2=2,rho=0),seed=2)
    X=dat$X; Y=dat$Y; Xprime=dat$Xprime; Yprime=dat$Yprime; Xmissing=dat$Xmissing; Ymissing=dat$Ymissing
    myprint(var(X), var(Y), cor(X,Y))
    beanplot(list(X=X, Y=c(Y,Y)), ylim=ylim, what=c(0,1,1,1), log="")
    #myboxplot(list(X=X, Y=Y))
    corplot(X,Y, xlab="X",ylab="Y", xlim=lim, ylim=lim)    
    
    dat=sim.partially.matched(m=n,n.x=n,n.y=n,distr="gamma",params=c(loc.2=.3,shape.1=3,shape.2=4,rate.1=3,rate.2=4,rho=0),seed=2)
    X=dat$X; Y=dat$Y; Xprime=dat$Xprime; Yprime=dat$Yprime; Xmissing=dat$Xmissing; Ymissing=dat$Ymissing
    myprint(var(X), var(Y), cor(X,Y))
    beanplot(list(X=X, Y=c(Y,Y)), ylim=ylim, what=c(0,1,1,1), log="")
    #myboxplot(list(X=X, Y=Y))
    corplot(X,Y, xlab="X",ylab="Y", xlim=lim, ylim=lim)    
if(save2file)dev.off()


save2file=T
n=500
lim=NULL
mypdf(file="figures/boxplot_sim_mixnormal", save2file=save2file, mfrow=c(2,2))
    dat=sim.partially.matched(m=n,n.x=n,n.y=n,distr="mixnormal",params=c(p.1=.9,p.2=.5,sd.n=1),seed=1)
    X=dat$X; Y=dat$Y; Xprime=dat$Xprime; Yprime=dat$Yprime; Xmissing=dat$Xmissing; Ymissing=dat$Ymissing
    myprint(var(X), var(Y), cor(X,Y))
    beanplot(list(X=X, Y=c(Y,Y)), ylim=ylim, what=c(0,1,1,1))
#    myboxplot(list(X=X, Y=Y))
    corplot(X,Y, xlab="X",ylab="Y", xlim=lim, ylim=lim)    
    
    dat=sim.partially.matched(m=n,n.x=n,n.y=n,distr="mixnormal",params=c(p.1=.9,p.2=.5,sd.n=.5),seed=1)
    X=dat$X; Y=dat$Y; Xprime=dat$Xprime; Yprime=dat$Yprime; Xmissing=dat$Xmissing; Ymissing=dat$Ymissing
    myprint(var(X), var(Y), cor(X,Y))
#    myboxplot(list(X=X, Y=Y))
    beanplot(list(X=X, Y=c(Y,Y)), ylim=ylim, what=c(0,1,1,1))
    corplot(X,Y, xlab="X",ylab="Y", xlim=lim, ylim=lim)    
if(save2file)dev.off()


save2file=T
n=500
lim=NULL
mypdf(file="figures/boxplot_sim_norm", save2file=save2file, mfcol=c(2,2))
    dat=sim.partially.matched(m=n,n.x=n,n.y=n,distr="normal",params=c(loc.2=1,rho=0,scale.2=1.4),seed=1)
    X=dat$X; Y=dat$Y; Xprime=dat$Xprime; Yprime=dat$Yprime; Xmissing=dat$Xmissing; Ymissing=dat$Ymissing
    myprint(var(X), var(Y), cor(X,Y))
    myboxplot(list(X=X, Y=Y))
    corplot(X,Y, xlab="X",ylab="Y", xlim=lim, ylim=lim)
    
    dat=sim.partially.matched(m=n,n.x=n,n.y=n,distr="normal",params=c(loc.2=-1,rho=0.5,scale.2=1),seed=1)
    X=dat$X; Y=dat$Y; Xprime=dat$Xprime; Yprime=dat$Yprime; Xmissing=dat$Xmissing; Ymissing=dat$Ymissing
    myprint(var(X), var(Y), cor(X,Y))
    myboxplot(list(X=X, Y=Y))
    corplot(X,Y, xlab="X",ylab="Y", xlim=lim, ylim=lim)
if(save2file)dev.off()


save2file=T
n=500
lim=NULL
mypdf(file="figures/boxplot_sim_student", save2file=save2file, mfcol=c(2,2))
    dat=sim.partially.matched(m=n,n.x=n,n.y=n,distr="student",params=c(loc.2=1,rho=0,scale.2=1.4),seed=1)
    X=dat$X; Y=dat$Y; Xprime=dat$Xprime; Yprime=dat$Yprime; Xmissing=dat$Xmissing; Ymissing=dat$Ymissing
    myprint(var(X), var(Y), cor(X,Y))
    myboxplot(list(X=X, Y=Y))
    corplot(X,Y, xlab="X",ylab="Y", xlim=lim, ylim=lim)
    
    dat=sim.partially.matched(m=n,n.x=n,n.y=n,distr="student",params=c(loc.2=-1,rho=0.5,scale.2=1),seed=1)
    X=dat$X; Y=dat$Y; Xprime=dat$Xprime; Yprime=dat$Yprime; Xmissing=dat$Xmissing; Ymissing=dat$Ymissing
    myprint(var(X), var(Y), cor(X,Y))
    myboxplot(list(X=X, Y=Y))
    corplot(X,Y, xlab="X",ylab="Y", xlim=lim, ylim=lim)    
if(save2file)dev.off()


save2file=T
n=500
lim=NULL
mypdf(file="figures/boxplot_sim_logistic", save2file=save2file, mfcol=c(2,2))
    dat=sim.partially.matched(m=n,n.x=n,n.y=n,distr="logistic",params=c(loc.2=1,rho=0,scale.2=1.4),seed=1)
    X=dat$X; Y=dat$Y; Xprime=dat$Xprime; Yprime=dat$Yprime; Xmissing=dat$Xmissing; Ymissing=dat$Ymissing
    myprint(var(X), var(Y), cor(X,Y))
    #beanplot(list(X=X, Y=c(Y,Y)), ylim=ylim, what=c(0,1,1,1))
    myboxplot(list(X=X, Y=Y))
    corplot(X,Y, xlab="X",ylab="Y", xlim=lim, ylim=lim)
    
    dat=sim.partially.matched(m=n,n.x=n,n.y=n,distr="logistic",params=c(loc.2=-1,rho=0.5,scale.2=1),seed=1)
    X=dat$X; Y=dat$Y; Xprime=dat$Xprime; Yprime=dat$Yprime; Xmissing=dat$Xmissing; Ymissing=dat$Ymissing
    myprint(var(X), var(Y), cor(X,Y))
    #beanplot(list(X=X, Y=c(Y,Y)), ylim=ylim, what=c(0,1,1,1))
    myboxplot(list(X=X, Y=Y))
    corplot(X,Y, xlab="X",ylab="Y", xlim=lim, ylim=lim)    
if(save2file)dev.off()


# the following shows that when the two tests are independent, adding a second test statistic can not do harm. 
# thus, if there is harm, it is probably due to correlation between test statistics
pchisq(qnorm(0.975)^2, 1, lower=F) # p-val 0.05
pchisq(qnorm(0.975)^2 + qnorm(0.975)^2, 1, lower=F) # p-val 0.005
pchisq(qnorm(0.975)^2 + qnorm(0.9)^2, 1, lower=F) # p-val 0.02
pchisq(qnorm(0.975)^2 + qnorm(0.1)^2, 1, lower=F) # p-val 0.02
pchisq(qnorm(0.975)^2 + qnorm(0.5)^2, 1, lower=F) # p-val 0.05




# validate a formula relating MW and W
m=10
n=20
X=rnorm(m)
Y=rnorm(n)
r <- rank(c(X, Y))
W = sum(r[(m+1):(m+n)])
U=0
for(i in 1:m)  for(j in 1:n)  U=U+ifelse(Y[j]>X[i],1,0)
stopifnot(W==U+n*(n+1)/2)

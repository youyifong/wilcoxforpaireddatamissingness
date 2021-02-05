<<<<<<< HEAD
# sourceable
if(!unix()) setwd("D:/gdrive/wmw/partially_matched_rank/R/")

# sizes
distrs=c("normal","logistic")
for (distr in distrs) {
#distr="logistic"
    
    library(kyotil)
    if(distr=="normal") rhos=c(0,0.5,0.8) else if(distr=="logistic") rhos=c(0,0.3,0.5)
    names(rhos)=rhos
    method="asym"
    m<-c(10,20,50); names(m)=m
    sizes=
    sapply(m, simplify="array", function (m.){
    sapply(rhos, simplify="array", function (rho){
        n=m.*2
        sim=paste(distr,m.,0,n,"0,"%+%rho%+%",1",sep="_")
        print(method%+%" "%+%sim)
        stats=get.sim.res ("res_"%+%method%+%"/"%+%sim, verbose=F)
        apply(stats, 1, function(x) mean(x[1:10e3]<0.05,na.rm=T)) # na should be very few, so the choice of na.action does not matter here
    })    
    })    
    round(sizes,2)
    
    tab=rbind(
        do.call(cbind, lapply("sr.mw."%+%c("10","20", "11","21"), function (i) t(sizes[i,,]) ))
        , NA, 
        do.call(cbind, lapply("mw.mw."%+%c("10","20", "11","21"), function (i) t(sizes[i,,]) ))
    )
    # shift column order
    tab=tab[,c(4:6,1:3,10:12,7:9)]
    round(tab,3)

#    out=lapply("mw.mw."%+%c("00","01"), function (i) t(sizes[i,,]) )
#    tab=rbind(tab, cbind(matrix(NA,length(m),length(rhos)*2), do.call(cbind, out)))
    names(dimnames(tab))=c("$\\rho$",NA) #$m\\backslash\\rho$    
    mytex(tab, digits=2, file="tables/sizes_pm_"%+%distr, sanitize.text.function = function(x) x, include.colnames = T, include.rownames=T, include.dup.rownames=T, align=c("c","c",rep(c("c","c","c|"),3),"c","c","c"),
#        , col.headers = "\\hline\n  &  \\multicolumn{"%+%(length(rhos)*4)%+%"}{c}{$\\rho$}\\\\  \n"
        , add.to.row=list(list(0,4),
            c(" $m$     &  \\multicolumn{"%+%(length(rhos))%+%"}{c|}{SR-MW$^l_{0}$} & \\multicolumn{"%+%(length(rhos))%+%"}{c|}{SR-MW$^q_{0}$}  &  \\multicolumn{"%+%(length(rhos))%+%"}{c|}{SR-MW$^l_{1}$} & \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR-MW$^q_{1}$}    \\\\ \n"
            , " $m$     &  \\multicolumn{"%+%(length(rhos))%+%"}{c|}{MW-MW$^l_{0}$} & \\multicolumn{"%+%(length(rhos))%+%"}{c|}{MW-MW$^q_{0}$}  &  \\multicolumn{"%+%(length(rhos))%+%"}{c|}{MW-MW$^l_{1}$} & \\multicolumn{"%+%(length(rhos))%+%"}{c}{MW-MW$^q_{1}$}    \\\\ \n"
#            , "      &  \\multicolumn{"%+%(length(rhos))%+%"}{c|}{}              & \\multicolumn{"%+%(length(rhos))%+%"}{c|}{}               &  \\multicolumn{"%+%(length(rhos))%+%"}{c|}{BP} & \\multicolumn{"%+%(length(rhos))%+%"}{c}{MW-MW$_{0,1}$}    \\\\ \n"
        ))
    )
    
}



# power
distrs=c("normal","logistic")
for (distr in distrs) {
# distr="normal"
    
    library(kyotil)
    pow=list()
    if(distr=="normal") rhos=c(0,0.5,.8) else if(distr=="logistic") rhos=c(0,0.3,0.5)
    names(rhos)=rhos
    mm=c(20,50); names(mm)=mm
    for(m in mm){
#        sapply(c(".1",".3",".5"), simplify="array", function (loc.2){
        loc.2=if(distr=="normal") {
            if(m==50) ".3" else ".5"
        } else if (distr=="logistic") {
            if(m==50) ".5" else "1"
        }
        res=
        sapply(rhos, simplify="array", function (rho){
            n=m*2
            sim=paste(distr,m,0,n,loc.2%+%","%+%rho%+%",1",sep="_")
            print(sim)
            stats=get.sim.res ("res_pow/"%+%sim, verbose=F)
            apply(stats, 1, function(x) mean(x<0.05,na.rm=T)) # na should be very few, so the choice of na.action does not matter here
        })    
#        })    
        round(res,2)
        pow=c(pow, list(res))
    }
    print(pow,digit=2)
    
    for (i in 0:1) {
        tab=lapply(pow, function(pow.) {
                 pow.[c("SR","MW","mw.mw.00","sr.mw.1"%+%i%+%"",     "mw.mw.1"%+%i%+%"",     "sr.mw.2"%+%i%+%"",     "mw.mw.2"%+%i%+%""),]
        })
        tab=do.call(cbind, tab)
        rownames(tab)=c("SR","MW","BP",      "SR-MW$^q_{"%+%i%+%"}$","MW-MW$^q_{"%+%i%+%"}$","SR-MW$^l_{"%+%i%+%"}$","MW-MW$^l_{"%+%i%+%"}$")
        names(dimnames(tab))=c("$\\rho$",NA)
        mytex(tab, file="tables/pow_pm"%+%i%+%"_"%+%distr, sanitize.text.function = function(x) x, include.colnames = T, include.rownames=T, include.dup.rownames=T, digits=2, align=c("c","l","c","c","c|","c","c","c"), 
            col.headers = "\\hline\n  &  \\multicolumn{3}{c|}{$m=20$}  &  \\multicolumn{3}{c}{$m=50$}  \\\\  \n", comment = FALSE, hline=c(3,7))
    }
    
}




# var
library(kyotil)
distr="normal"
reses=list()
rhos=c(0,0.3,0.8); names(rhos)=rhos
m=50
tab=sapply(rhos, simplify="array", function (rho){
    n=m
    sim=paste(distr,m,n,"0,"%+%rho%+%",1",sep="_")
    stats=get.sim.res ("res_var/"%+%sim, verbose=F)
    cor(stats["U.p",], stats["U.mw",])
    cor(stats["W.plus",], stats["W.mw",])
    t(apply(stats, 1, function(x) c(mean(x),var(x))))
})    
tab[c(11:15),,]


#########################################################################
#### with Xextra
    
# sizes
distrs=c("normal","lognormal","logistic","gamma")
for (distr in distrs) {
#distr="gamma"
    
    library(kyotil)
    #if(distr=="normal") rhos=c(0,0.5,0.8) else if(distr=="logistic") rhos=c(0,0.3,0.5)
    rhos=c(0,0.5,0.8) 
    names(rhos)=rhos
    method="asym"
    
    reses=list()
    for (m in c(10,20,50)) {
        lxs=floor(m*c(0.1,.25,.5,1)); names(lxs)=lxs
        loc.2=0
        res=sapply(lxs, simplify="array", function (lx){
            sapply(rhos, simplify="array", function (rho){            
                if(distr=="normal" | distr=="lognormal" | distr=="logistic") {
                    param="0,"%+%rho%+%",1"
                } else if (distr=="gamma") {
                    param="0,3,3,1,1,"%+%rho
                } else stop("no distr")
                n=m
                sim=paste(distr,m,lx,n,param,sep="_") 
                print(method%+%" "%+%sim)
                stats=get.sim.res ("res_"%+%method%+%"/"%+%sim, verbose=F)
                apply(stats, 1, function(x) mean(x[1:10e3]<0.05,na.rm=T)) # na should be very few, so the choice of na.action does not matter here
        })    
        }) 
        reses=c(reses, list(res))   
    }
    print(reses,2)
    
    tab=rbind(
        do.call(cbind, lapply(c("WZ","mw.mw.00", "sr.mw.20", "mw.mw.20"), function (i) t(reses[[1]][i,,]) )),
        do.call(cbind, lapply(c("WZ","mw.mw.00", "sr.mw.20", "mw.mw.20"), function (i) t(reses[[2]][i,,]) )),
        do.call(cbind, lapply(c("WZ","mw.mw.00", "sr.mw.20", "mw.mw.20"), function (i) t(reses[[3]][i,,]) ))
    )
    names(dimnames(tab))=c("$\\rho$",NA) #$m\\backslash\\rho$
    round(tab,2)    
    
    mytex(tab, file="tables/sizes_pm_"%+%distr%+%"_extended", sanitize.text.function = function(x) x, include.colnames = T, include.rownames=T, include.dup.rownames=T, digits=2
        , align=c("c","c",rep(c("c","c","c|"),3),"c","c","c")
#        , col.headers = "\\hline\n   & \\multicolumn{"%+%(length(rhos))%+%"}{c|}{SR} & \\multicolumn{"%+%(length(rhos))%+%"}{c|}{BP}   &  \\multicolumn{"%+%(length(rhos))%+%"}{c|}{SR-MW$^l_{0}$}     &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{MW-MW$^l_{0}$}    \\\\ \n"
        , add.to.row=list(list(0,4,8),
            c("              &  \\multicolumn{"%+%(length(rhos)*ncol(tab)/3)%+%"}{c}{$m=10$}   \\\\ \n $l$     &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{WZ} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{BP} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR-MW$^l_{0}$} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{MW-MW$^l_{0}$}    \\\\ \n"  
             ,"   \\hline    &  \\multicolumn{"%+%(length(rhos)*ncol(tab)/3)%+%"}{c}{$m=20$}   \\\\ \n $l$     &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{WZ} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{BP} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR-MW$^l_{0}$} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{MW-MW$^l_{0}$}    \\\\ \n"
             ,"   \\hline    &  \\multicolumn{"%+%(length(rhos)*ncol(tab)/3)%+%"}{c}{$m=50$}   \\\\ \n $l$     &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{WZ} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{BP} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR-MW$^l_{0}$} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{MW-MW$^l_{0}$}    \\\\ \n"
          ))
    )
    
    mytex(tab[1:8,], file="tables/sizes_pm_"%+%distr%+%"_extended_a", sanitize.text.function = function(x) x, include.colnames = T, include.rownames=T, include.dup.rownames=T, digits=2
        , align=c("c","c",rep(c("c","c","c|"),3),"c","c","c")
#        , col.headers = "\\hline\n   & \\multicolumn{"%+%(length(rhos))%+%"}{c|}{SR} & \\multicolumn{"%+%(length(rhos))%+%"}{c|}{BP}   &  \\multicolumn{"%+%(length(rhos))%+%"}{c|}{SR-MW$^l_{0}$}     &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{MW-MW$^l_{0}$}    \\\\ \n"
        , add.to.row=list(list(0,4),
            c("              &  \\multicolumn{"%+%(length(rhos)*ncol(tab)/3)%+%"}{c}{$m=10$}   \\\\ \n $l$     &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{WZ} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{BP} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR-MW$^l_{0}$} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{MW-MW$^l_{0}$}    \\\\ \n"  
             ,"   \\hline    &  \\multicolumn{"%+%(length(rhos)*ncol(tab)/3)%+%"}{c}{$m=20$}   \\\\ \n $l$     &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{WZ} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{BP} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR-MW$^l_{0}$} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{MW-MW$^l_{0}$}    \\\\ \n"
          ))
    )
    
}



# power
distrs=c("normal","lognormal","logistic","gamma")
for (distr in distrs) {
#distr="gamma"
    
    library(kyotil)
    #if(distr=="normal") rhos=c(0,0.5,0.8) else if(distr=="logistic") rhos=c(0,0.3,0.5)  
    rhos=c(0,0.5,0.8)
    names(rhos)=rhos
    method="pow"
    
    reses=list()
    for (m in c(20,50)) {
        lxs=floor(m*c(0.1,.25,.5,1)); names(lxs)=lxs
        res=sapply(lxs, simplify="array", function (lx){
            sapply(rhos, simplify="array", function (rho){
                if(distr=="normal" | distr=="lognormal") {
                    param=(if(m==50) ".3" else ".5")%+%","%+%rho%+%",1"
                } else if (distr=="logistic") {
                    param=(if(m==50) ".5" else "1")%+%","%+%rho%+%",1"
                } else if (distr=="gamma") {
                    param=(if(m==50) "0.4" else "0.6")%+%",3,3,1,1,"%+%rho
                } else stop("no distr")
        
                n=m
                sim=paste(distr,m,lx,n,param,sep="_") 
                print(method%+%" "%+%sim)
                stats=get.sim.res ("res_"%+%method%+%"/"%+%sim, verbose=F)
                apply(stats, 1, function(x) mean(x[1:10e3]<0.05,na.rm=T)) # na should be very few, so the choice of na.action does not matter here
        })    
        }) 
        reses=c(reses, list(res))   
    }
    reses
    
    tab=rbind(
        do.call(cbind, lapply(c("SR","WZ","mw.mw.00","sr.mw.20", "mw.mw.20"), function (i) t(reses[[1]][i,,]) )),
        do.call(cbind, lapply(c("SR","WZ","mw.mw.00","sr.mw.20", "mw.mw.20"), function (i) t(reses[[2]][i,,]) ))
    )
    names(dimnames(tab))=c("$\\rho$",NA) #$m\\backslash\\rho$    
    round(tab, 2)
    
    mytex(tab, file="tables/pow_pm_"%+%distr%+%"_extended", sanitize.text.function = function(x) x, include.colnames = T, include.rownames=T, include.dup.rownames=T, digits=2
        , align=c("c","c",rep(c("c","c","c|"),4),"c","c","c")
        , add.to.row=list(list(0,4),
            c("     &  \\multicolumn{"%+%(length(rhos)*ncol(tab)/3)%+%"}{c}{$m=20$}   \\\\ \n $l$   &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR}  &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{WZ}  &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{BP} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR-MW$^l_{0}$} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{MW-MW$^l_{0}$}    \\\\ \n",  
              " \\hline  &  \\multicolumn{"%+%(length(rhos)*ncol(tab)/3)%+%"}{c}{$m=50$}   \\\\ \n $l$   &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR}  &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{WZ}  &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{BP} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR-MW$^l_{0}$} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{MW-MW$^l_{0}$}    \\\\ \n")
    ))
    
}


# power, one table for m=50 and has all distributions, another table for m=20
for (m in c(50,20))  {
#m=50    
    library(kyotil)
    method="pow"
    rhos=c(0,0.5,0.8); names(rhos)=rhos
    reses=list()
    distrs=c("logistic","normal","lognormal","gamma")
    for (distr in distrs) {                
        lxs=floor(m*c(0.1,.25,.5,1)); names(lxs)=lxs
        res=sapply(lxs, simplify="array", function (lx){
            sapply(rhos, simplify="array", function (rho){
                if(distr=="normal" | distr=="lognormal") {
                    param=(if(m==50) ".3" else ".5")%+%","%+%rho%+%",1"
                } else if (distr=="logistic") {
                    param=(if(m==50) ".5" else "1")%+%","%+%rho%+%",1"
                } else if (distr=="gamma") {
                    param=(if(m==50) "0.4" else "0.6")%+%",3,3,1,1,"%+%rho
                } else stop("no distr")
        
                n=m
                sim=paste(distr,m,lx,n,param,sep="_") 
                print(method%+%" "%+%sim)
                stats=get.sim.res ("res_"%+%method%+%"/"%+%sim, verbose=F)
                apply(stats, 1, function(x) mean(x[1:10e3]<0.05,na.rm=T)) # na should be very few, so the choice of na.action does not matter here
        })    
        }) 
        reses=c(reses, list(res))   
    }
                
    tab=rbind(
        do.call(cbind, lapply(c("SR","WZ","mw.mw.00","sr.mw.20", "mw.mw.20"), function (i) t(reses[[1]][i,,]) )),
        do.call(cbind, lapply(c("SR","WZ","mw.mw.00","sr.mw.20", "mw.mw.20"), function (i) t(reses[[2]][i,,]) )),
        do.call(cbind, lapply(c("SR","WZ","mw.mw.00","sr.mw.20", "mw.mw.20"), function (i) t(reses[[3]][i,,]) )),
        do.call(cbind, lapply(c("SR","WZ","mw.mw.00","sr.mw.20", "mw.mw.20"), function (i) t(reses[[4]][i,,]) ))
    )
    names(dimnames(tab))=c("$\\rho$",NA) #$m\\backslash\\rho$
    round(tab, 2)
    
    mytex(tab, file="tables/pow_pm_extended_m"%+%m, sanitize.text.function = function(x) x, include.colnames = T, include.rownames=T, include.dup.rownames=T, digits=2
        , align=c("c","c",rep(c("c","c","c|"),4),"c","c","c")
        , add.to.row=list(list(0,4,8,12),
            c("          &  \\multicolumn{"%+%ncol(tab)%+%"}{c}{"%+%distrs[1]%+%"} \\\\ \n $l$   &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR}  &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{WZ}  &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{BP} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR-MW$^l_{0}$} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{MW-MW$^l_{0}$}    \\\\ \n",  
              " \\hline  &  \\multicolumn{"%+%ncol(tab)%+%"}{c}{"%+%distrs[2]%+%"} \\\\ \n $l$   &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR}  &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{WZ}  &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{BP} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR-MW$^l_{0}$} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{MW-MW$^l_{0}$}    \\\\ \n",
              " \\hline  &  \\multicolumn{"%+%ncol(tab)%+%"}{c}{"%+%distrs[3]%+%"} \\\\ \n $l$   &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR}  &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{WZ}  &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{BP} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR-MW$^l_{0}$} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{MW-MW$^l_{0}$}    \\\\ \n",
              " \\hline  &  \\multicolumn{"%+%ncol(tab)%+%"}{c}{"%+%distrs[4]%+%"} \\\\ \n $l$   &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR}  &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{WZ}  &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{BP} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR-MW$^l_{0}$} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{MW-MW$^l_{0}$}    \\\\ \n"
    )))
    
}


# compare distributions of gamma and lognormal distributed data
library(kyotil)
library(robustrank)
myfigure(mfrow=c(1,2), width=7, height=6)
    m=200
    n=75
    ylim=c(0,14.5)
    
    distr="lognormal"
    dat.2=sim.partially.matched(m=m,n.x=0,n.y=0,distr=distr,params=c(loc.2=0.3,rho=0,scale.2=1),seed=1)
    boxplot(list(dat.2$X, dat.2$Y), main=distr, ylim=ylim, names=c("X","Y"))
    wilcox.test(dat.2$X[1:n], dat.2$Y[1:n], paired=T)
    wilcox.test(dat.2$X[1:n], dat.2$Y[1:n], paired=F)
    mtext(side=3, line=2.5, adj=-.1, text="(b)", cex=1.4, font=2, xpd=NA)    
    
    distr="gamma"
    dat.1=sim.partially.matched(m=m,n.x=0,n.y=0,distr=distr,params=c(loc.2=0.4,shape.1=3,shape.2=3,rate.1=1,rate.2=1,rho=0),seed=1)
    boxplot(list(dat.1$X, dat.1$Y), main=distr, ylim=ylim, names=c("X","Y"))
    wilcox.test(dat.1$X[1:n], dat.1$Y[1:n], paired=T)
    wilcox.test(dat.1$X[1:n], dat.1$Y[1:n], paired=F)
    mtext(side=3, line=2.5, adj=-.1, text="(a)", cex=1.4, font=2, xpd=NA)        
mydev.off(file="figures/simulated_data_boxplot", ext="pdf")


# power and size for small number of missingness
for (i in c(1:4)) { # i=1 for size, i=2,3,4 for power; i=1,2 10% missingness, i=3 20% missingness, i=4 40% missingness
#i=1
    
    library(kyotil)    
    method=if(i==1) "asym" else "pow"
    reses=list()
    for (m in c(50,20))  {
        rhos=c(0,0.5,0.8); names(rhos)=rhos
        distrs=c("logistic","normal","lognormal","gamma")
        for (distr in distrs) {                
            lxs=floor(m*c(0.1,.25,.5,1)); names(lxs)=lxs
            res=sapply(rhos, simplify="array", function (rho){            
                if(distr=="normal" | distr=="lognormal") {
                    param=(if(i==1) "0" else if(m==50) ".3" else ".5")%+%","%+%rho%+%",1"
                } else if (distr=="logistic") {
                    param=(if(i==1) "0" else if(m==50) ".5" else "1")%+%","%+%rho%+%",1"
                } else if (distr=="gamma") {
                    param=(if(i==1) "0" else if(m==50) "0.4" else "0.6")%+%",3,3,1,1,"%+%rho
                } else stop("no distr")
                
                if(i==1 | i==2) sim=paste(distr,m,lx=floor(m/20),n=floor(m/20),param,sep="_") 
                if(i==3)        sim=paste(distr,m,lx=floor(m/5 ),n=floor(m/5 ),param,sep="_") 
                if(i==4)        sim=paste(distr,m,lx=floor(m/2 ),n=floor(m/2 ),param,sep="_") 
                print(method%+%" "%+%sim)
                stats=get.sim.res ("res_"%+%method%+%"/"%+%sim, verbose=F)
                apply(stats, 1, function(x) mean(x[1:10e3]<0.05,na.rm=T)) # na should be very few, so the choice of na.action does not matter here
            }) 
            reses=c(reses, list(res))   
        }                
    }
    
    tab=rbind(
        do.call(cbind, lapply(c("SR","WZ","mw.mw.00","sr.mw.20", "mw.mw.20"), function (i) t(reses[[1]][i,]) )),
        do.call(cbind, lapply(c("SR","WZ","mw.mw.00","sr.mw.20", "mw.mw.20"), function (i) t(reses[[2]][i,]) )),
        do.call(cbind, lapply(c("SR","WZ","mw.mw.00","sr.mw.20", "mw.mw.20"), function (i) t(reses[[3]][i,]) )),
        do.call(cbind, lapply(c("SR","WZ","mw.mw.00","sr.mw.20", "mw.mw.20"), function (i) t(reses[[4]][i,]) )),
        do.call(cbind, lapply(c("SR","WZ","mw.mw.00","sr.mw.20", "mw.mw.20"), function (i) t(reses[[5]][i,]) )),
        do.call(cbind, lapply(c("SR","WZ","mw.mw.00","sr.mw.20", "mw.mw.20"), function (i) t(reses[[6]][i,]) )),
        do.call(cbind, lapply(c("SR","WZ","mw.mw.00","sr.mw.20", "mw.mw.20"), function (i) t(reses[[7]][i,]) )),
        do.call(cbind, lapply(c("SR","WZ","mw.mw.00","sr.mw.20", "mw.mw.20"), function (i) t(reses[[8]][i,]) ))
    )
    rownames(tab)=c(distrs,distrs)
    names(dimnames(tab))=c("$\\rho$",NA) #$m\\backslash\\rho$
    round(tab, 2)
    if (i==1) tab=tab[,-(1:3)]    
    
    m=50
    if(i==1 | i==2) s1=paste("m=",m,",l=",floor(m/20),",n=",n=floor(m/20),sep="") 
    if(i==4)        s1=paste("m=",m,",l=",floor(m/2 ),",n=",n=floor(m/2 ),sep="") 
    if(i==3)        s1=paste("m=",m,",l=",floor(m/5 ),",n=",n=floor(m/5 ),sep="") 
    m=20
    if(i==1 | i==2) s2=paste("m=",m,",l=",floor(m/20),",n=",n=floor(m/20),sep="") 
    if(i==4)        s2=paste("m=",m,",l=",floor(m/2 ),",n=",n=floor(m/2 ),sep="") 
    if(i==3)        s2=paste("m=",m,",l=",floor(m/5 ),",n=",n=floor(m/5 ),sep="") 
    mytex(tab, file="tables/"%+%(if(method=="asym") "sizes" else method)%+%"_pm_extended_low_missing_"%+%i, sanitize.text.function = function(x) x, include.colnames = T, include.rownames=T, include.dup.rownames=T, digits=2
        , align=c("c","c",rep(c("c","c","c|"),if(i==1) 3 else 4),"c","c","c")
        , add.to.row=list(list(0,4),
            c("          &  \\multicolumn{"%+%ncol(tab)%+%"}{c}{$"%+%s1%+%"$} \\\\ \n   "%+%(if(i!=1) "&  \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR}  " else "")%+%"&  \\multicolumn{"%+%(length(rhos))%+%"}{c}{WZ}  &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{BP} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR-MW$^l_{0}$} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{MW-MW$^l_{0}$}    \\\\ \n",  
              " \\hline  &  \\multicolumn{"%+%ncol(tab)%+%"}{c}{$"%+%s2%+%"$} \\\\ \n   "%+%(if(i!=1) "&  \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR}  " else "")%+%"&  \\multicolumn{"%+%(length(rhos))%+%"}{c}{WZ}  &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{BP} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR-MW$^l_{0}$} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{MW-MW$^l_{0}$}    \\\\ \n")
    ))
    
}
=======
# sourceable
if(!unix()) setwd("D:/gdrive/wmw/partially_matched_rank/R/")

# sizes
distrs=c("normal","logistic")
for (distr in distrs) {
#distr="logistic"
    
    library(kyotil)
    if(distr=="normal") rhos=c(0,0.5,0.8) else if(distr=="logistic") rhos=c(0,0.3,0.5)
    names(rhos)=rhos
    method="asym"
    m<-c(10,20,50); names(m)=m
    sizes=
    sapply(m, simplify="array", function (m.){
    sapply(rhos, simplify="array", function (rho){
        n=m.*2
        sim=paste(distr,m.,0,n,"0,"%+%rho%+%",1",sep="_")
        print(method%+%" "%+%sim)
        stats=get.sim.res ("res_"%+%method%+%"/"%+%sim, verbose=F)
        apply(stats, 1, function(x) mean(x[1:10e3]<0.05,na.rm=T)) # na should be very few, so the choice of na.action does not matter here
    })    
    })    
    round(sizes,2)
    
    tab=rbind(
        do.call(cbind, lapply("sr.mw."%+%c("10","20", "11","21"), function (i) t(sizes[i,,]) ))
        , NA, 
        do.call(cbind, lapply("mw.mw."%+%c("10","20", "11","21"), function (i) t(sizes[i,,]) ))
    )
    # shift column order
    tab=tab[,c(4:6,1:3,10:12,7:9)]
    round(tab,3)

#    out=lapply("mw.mw."%+%c("00","01"), function (i) t(sizes[i,,]) )
#    tab=rbind(tab, cbind(matrix(NA,length(m),length(rhos)*2), do.call(cbind, out)))
    names(dimnames(tab))=c("$\\rho$",NA) #$m\\backslash\\rho$    
    mytex(tab, digits=2, file="tables/sizes_pm_"%+%distr, sanitize.text.function = function(x) x, include.colnames = T, include.rownames=T, include.dup.rownames=T, align=c("c","c",rep(c("c","c","c|"),3),"c","c","c"),
#        , col.headers = "\\hline\n  &  \\multicolumn{"%+%(length(rhos)*4)%+%"}{c}{$\\rho$}\\\\  \n"
        , add.to.row=list(list(0,4),
            c(" $m$     &  \\multicolumn{"%+%(length(rhos))%+%"}{c|}{SR-MW$^l_{0}$} & \\multicolumn{"%+%(length(rhos))%+%"}{c|}{SR-MW$^q_{0}$}  &  \\multicolumn{"%+%(length(rhos))%+%"}{c|}{SR-MW$^l_{1}$} & \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR-MW$^q_{1}$}    \\\\ \n"
            , " $m$     &  \\multicolumn{"%+%(length(rhos))%+%"}{c|}{MW-MW$^l_{0}$} & \\multicolumn{"%+%(length(rhos))%+%"}{c|}{MW-MW$^q_{0}$}  &  \\multicolumn{"%+%(length(rhos))%+%"}{c|}{MW-MW$^l_{1}$} & \\multicolumn{"%+%(length(rhos))%+%"}{c}{MW-MW$^q_{1}$}    \\\\ \n"
#            , "      &  \\multicolumn{"%+%(length(rhos))%+%"}{c|}{}              & \\multicolumn{"%+%(length(rhos))%+%"}{c|}{}               &  \\multicolumn{"%+%(length(rhos))%+%"}{c|}{BP} & \\multicolumn{"%+%(length(rhos))%+%"}{c}{MW-MW$_{0,1}$}    \\\\ \n"
        ))
    )
    
}



# power
distrs=c("normal","logistic")
for (distr in distrs) {
# distr="normal"
    
    library(kyotil)
    pow=list()
    if(distr=="normal") rhos=c(0,0.5,.8) else if(distr=="logistic") rhos=c(0,0.3,0.5)
    names(rhos)=rhos
    mm=c(20,50); names(mm)=mm
    for(m in mm){
#        sapply(c(".1",".3",".5"), simplify="array", function (loc.2){
        loc.2=if(distr=="normal") {
            if(m==50) ".3" else ".5"
        } else if (distr=="logistic") {
            if(m==50) ".5" else "1"
        }
        res=
        sapply(rhos, simplify="array", function (rho){
            n=m*2
            sim=paste(distr,m,0,n,loc.2%+%","%+%rho%+%",1",sep="_")
            print(sim)
            stats=get.sim.res ("res_pow/"%+%sim, verbose=F)
            apply(stats, 1, function(x) mean(x<0.05,na.rm=T)) # na should be very few, so the choice of na.action does not matter here
        })    
#        })    
        round(res,2)
        pow=c(pow, list(res))
    }
    print(pow,digit=2)
    
    for (i in 0:1) {
        tab=lapply(pow, function(pow.) {
                 pow.[c("SR","MW","mw.mw.00","sr.mw.1"%+%i%+%"",     "mw.mw.1"%+%i%+%"",     "sr.mw.2"%+%i%+%"",     "mw.mw.2"%+%i%+%""),]
        })
        tab=do.call(cbind, tab)
        rownames(tab)=c("SR","MW","BP",      "SR-MW$^q_{"%+%i%+%"}$","MW-MW$^q_{"%+%i%+%"}$","SR-MW$^l_{"%+%i%+%"}$","MW-MW$^l_{"%+%i%+%"}$")
        names(dimnames(tab))=c("$\\rho$",NA)
        mytex(tab, file="tables/pow_pm"%+%i%+%"_"%+%distr, sanitize.text.function = function(x) x, include.colnames = T, include.rownames=T, include.dup.rownames=T, digits=2, align=c("c","l","c","c","c|","c","c","c"), 
            col.headers = "\\hline\n  &  \\multicolumn{3}{c|}{$m=20$}  &  \\multicolumn{3}{c}{$m=50$}  \\\\  \n", comment = FALSE, hline=c(3,7))
    }
    
}




# var
library(kyotil)
distr="normal"
reses=list()
rhos=c(0,0.3,0.8); names(rhos)=rhos
m=50
tab=sapply(rhos, simplify="array", function (rho){
    n=m
    sim=paste(distr,m,n,"0,"%+%rho%+%",1",sep="_")
    stats=get.sim.res ("res_var/"%+%sim, verbose=F)
    cor(stats["U.p",], stats["U.mw",])
    cor(stats["W.plus",], stats["W.mw",])
    t(apply(stats, 1, function(x) c(mean(x),var(x))))
})    
tab[c(11:15),,]


#########################################################################
#### with Xextra
    
# sizes
distrs=c("normal","lognormal","logistic","gamma")
for (distr in distrs) {
#distr="gamma"
    
    library(kyotil)
    #if(distr=="normal") rhos=c(0,0.5,0.8) else if(distr=="logistic") rhos=c(0,0.3,0.5)
    rhos=c(0,0.5,0.8) 
    names(rhos)=rhos
    method="asym"
    
    reses=list()
    for (m in c(10,20,50)) {
        lxs=floor(m*c(0.1,.25,.5,1)); names(lxs)=lxs
        loc.2=0
        res=sapply(lxs, simplify="array", function (lx){
            sapply(rhos, simplify="array", function (rho){            
                if(distr=="normal" | distr=="lognormal" | distr=="logistic") {
                    param="0,"%+%rho%+%",1"
                } else if (distr=="gamma") {
                    param="0,3,3,1,1,"%+%rho
                } else stop("no distr")
                n=m
                sim=paste(distr,m,lx,n,param,sep="_") 
                print(method%+%" "%+%sim)
                stats=get.sim.res ("res_"%+%method%+%"/"%+%sim, verbose=F)
                apply(stats, 1, function(x) mean(x[1:10e3]<0.05,na.rm=T)) # na should be very few, so the choice of na.action does not matter here
        })    
        }) 
        reses=c(reses, list(res))   
    }
    print(reses,2)
    
    tab=rbind(
        do.call(cbind, lapply(c("WZ","mw.mw.00", "sr.mw.20", "mw.mw.20"), function (i) t(reses[[1]][i,,]) )),
        do.call(cbind, lapply(c("WZ","mw.mw.00", "sr.mw.20", "mw.mw.20"), function (i) t(reses[[2]][i,,]) )),
        do.call(cbind, lapply(c("WZ","mw.mw.00", "sr.mw.20", "mw.mw.20"), function (i) t(reses[[3]][i,,]) ))
    )
    names(dimnames(tab))=c("$\\rho$",NA) #$m\\backslash\\rho$
    round(tab,2)    
    
    mytex(tab, file="tables/sizes_pm_"%+%distr%+%"_extended", sanitize.text.function = function(x) x, include.colnames = T, include.rownames=T, include.dup.rownames=T, digits=2
        , align=c("c","c",rep(c("c","c","c|"),3),"c","c","c")
#        , col.headers = "\\hline\n   & \\multicolumn{"%+%(length(rhos))%+%"}{c|}{SR} & \\multicolumn{"%+%(length(rhos))%+%"}{c|}{BP}   &  \\multicolumn{"%+%(length(rhos))%+%"}{c|}{SR-MW$^l_{0}$}     &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{MW-MW$^l_{0}$}    \\\\ \n"
        , add.to.row=list(list(0,4,8),
            c("              &  \\multicolumn{"%+%(length(rhos)*ncol(tab)/3)%+%"}{c}{$m=10$}   \\\\ \n $l$     &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{WZ} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{BP} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR-MW$^l_{0}$} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{MW-MW$^l_{0}$}    \\\\ \n"  
             ,"   \\hline    &  \\multicolumn{"%+%(length(rhos)*ncol(tab)/3)%+%"}{c}{$m=20$}   \\\\ \n $l$     &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{WZ} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{BP} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR-MW$^l_{0}$} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{MW-MW$^l_{0}$}    \\\\ \n"
             ,"   \\hline    &  \\multicolumn{"%+%(length(rhos)*ncol(tab)/3)%+%"}{c}{$m=50$}   \\\\ \n $l$     &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{WZ} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{BP} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR-MW$^l_{0}$} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{MW-MW$^l_{0}$}    \\\\ \n"
          ))
    )
    
    mytex(tab[1:8,], file="tables/sizes_pm_"%+%distr%+%"_extended_a", sanitize.text.function = function(x) x, include.colnames = T, include.rownames=T, include.dup.rownames=T, digits=2
        , align=c("c","c",rep(c("c","c","c|"),3),"c","c","c")
#        , col.headers = "\\hline\n   & \\multicolumn{"%+%(length(rhos))%+%"}{c|}{SR} & \\multicolumn{"%+%(length(rhos))%+%"}{c|}{BP}   &  \\multicolumn{"%+%(length(rhos))%+%"}{c|}{SR-MW$^l_{0}$}     &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{MW-MW$^l_{0}$}    \\\\ \n"
        , add.to.row=list(list(0,4),
            c("              &  \\multicolumn{"%+%(length(rhos)*ncol(tab)/3)%+%"}{c}{$m=10$}   \\\\ \n $l$     &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{WZ} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{BP} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR-MW$^l_{0}$} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{MW-MW$^l_{0}$}    \\\\ \n"  
             ,"   \\hline    &  \\multicolumn{"%+%(length(rhos)*ncol(tab)/3)%+%"}{c}{$m=20$}   \\\\ \n $l$     &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{WZ} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{BP} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR-MW$^l_{0}$} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{MW-MW$^l_{0}$}    \\\\ \n"
          ))
    )
    
}



# power
distrs=c("normal","lognormal","logistic","gamma")
for (distr in distrs) {
#distr="gamma"
    
    library(kyotil)
    #if(distr=="normal") rhos=c(0,0.5,0.8) else if(distr=="logistic") rhos=c(0,0.3,0.5)  
    rhos=c(0,0.5,0.8)
    names(rhos)=rhos
    method="pow"
    
    reses=list()
    for (m in c(20,50)) {
        lxs=floor(m*c(0.1,.25,.5,1)); names(lxs)=lxs
        res=sapply(lxs, simplify="array", function (lx){
            sapply(rhos, simplify="array", function (rho){
                if(distr=="normal" | distr=="lognormal") {
                    param=(if(m==50) ".3" else ".5")%+%","%+%rho%+%",1"
                } else if (distr=="logistic") {
                    param=(if(m==50) ".5" else "1")%+%","%+%rho%+%",1"
                } else if (distr=="gamma") {
                    param=(if(m==50) "0.4" else "0.6")%+%",3,3,1,1,"%+%rho
                } else stop("no distr")
        
                n=m
                sim=paste(distr,m,lx,n,param,sep="_") 
                print(method%+%" "%+%sim)
                stats=get.sim.res ("res_"%+%method%+%"/"%+%sim, verbose=F)
                apply(stats, 1, function(x) mean(x[1:10e3]<0.05,na.rm=T)) # na should be very few, so the choice of na.action does not matter here
        })    
        }) 
        reses=c(reses, list(res))   
    }
    reses
    
    tab=rbind(
        do.call(cbind, lapply(c("SR","WZ","mw.mw.00","sr.mw.20", "mw.mw.20"), function (i) t(reses[[1]][i,,]) )),
        do.call(cbind, lapply(c("SR","WZ","mw.mw.00","sr.mw.20", "mw.mw.20"), function (i) t(reses[[2]][i,,]) ))
    )
    names(dimnames(tab))=c("$\\rho$",NA) #$m\\backslash\\rho$    
    round(tab, 2)
    
    mytex(tab, file="tables/pow_pm_"%+%distr%+%"_extended", sanitize.text.function = function(x) x, include.colnames = T, include.rownames=T, include.dup.rownames=T, digits=2
        , align=c("c","c",rep(c("c","c","c|"),4),"c","c","c")
        , add.to.row=list(list(0,4),
            c("     &  \\multicolumn{"%+%(length(rhos)*ncol(tab)/3)%+%"}{c}{$m=20$}   \\\\ \n $l$   &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR}  &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{WZ}  &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{BP} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR-MW$^l_{0}$} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{MW-MW$^l_{0}$}    \\\\ \n",  
              " \\hline  &  \\multicolumn{"%+%(length(rhos)*ncol(tab)/3)%+%"}{c}{$m=50$}   \\\\ \n $l$   &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR}  &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{WZ}  &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{BP} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR-MW$^l_{0}$} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{MW-MW$^l_{0}$}    \\\\ \n")
    ))
    
}


# power, one table for m=50 and has all distributions, another table for m=20
for (m in c(50,20))  {
#m=50    
    library(kyotil)
    method="pow"
    rhos=c(0,0.5,0.8); names(rhos)=rhos
    reses=list()
    distrs=c("logistic","normal","lognormal","gamma")
    for (distr in distrs) {                
        lxs=floor(m*c(0.1,.25,.5,1)); names(lxs)=lxs
        res=sapply(lxs, simplify="array", function (lx){
            sapply(rhos, simplify="array", function (rho){
                if(distr=="normal" | distr=="lognormal") {
                    param=(if(m==50) ".3" else ".5")%+%","%+%rho%+%",1"
                } else if (distr=="logistic") {
                    param=(if(m==50) ".5" else "1")%+%","%+%rho%+%",1"
                } else if (distr=="gamma") {
                    param=(if(m==50) "0.4" else "0.6")%+%",3,3,1,1,"%+%rho
                } else stop("no distr")
        
                n=m
                sim=paste(distr,m,lx,n,param,sep="_") 
                print(method%+%" "%+%sim)
                stats=get.sim.res ("res_"%+%method%+%"/"%+%sim, verbose=F)
                apply(stats, 1, function(x) mean(x[1:10e3]<0.05,na.rm=T)) # na should be very few, so the choice of na.action does not matter here
        })    
        }) 
        reses=c(reses, list(res))   
    }
                
    tab=rbind(
        do.call(cbind, lapply(c("SR","WZ","mw.mw.00","sr.mw.20", "mw.mw.20"), function (i) t(reses[[1]][i,,]) )),
        do.call(cbind, lapply(c("SR","WZ","mw.mw.00","sr.mw.20", "mw.mw.20"), function (i) t(reses[[2]][i,,]) )),
        do.call(cbind, lapply(c("SR","WZ","mw.mw.00","sr.mw.20", "mw.mw.20"), function (i) t(reses[[3]][i,,]) )),
        do.call(cbind, lapply(c("SR","WZ","mw.mw.00","sr.mw.20", "mw.mw.20"), function (i) t(reses[[4]][i,,]) ))
    )
    names(dimnames(tab))=c("$\\rho$",NA) #$m\\backslash\\rho$
    round(tab, 2)
    
    mytex(tab, file="tables/pow_pm_extended_m"%+%m, sanitize.text.function = function(x) x, include.colnames = T, include.rownames=T, include.dup.rownames=T, digits=2
        , align=c("c","c",rep(c("c","c","c|"),4),"c","c","c")
        , add.to.row=list(list(0,4,8,12),
            c("          &  \\multicolumn{"%+%ncol(tab)%+%"}{c}{"%+%distrs[1]%+%"} \\\\ \n $l$   &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR}  &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{WZ}  &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{BP} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR-MW$^l_{0}$} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{MW-MW$^l_{0}$}    \\\\ \n",  
              " \\hline  &  \\multicolumn{"%+%ncol(tab)%+%"}{c}{"%+%distrs[2]%+%"} \\\\ \n $l$   &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR}  &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{WZ}  &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{BP} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR-MW$^l_{0}$} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{MW-MW$^l_{0}$}    \\\\ \n",
              " \\hline  &  \\multicolumn{"%+%ncol(tab)%+%"}{c}{"%+%distrs[3]%+%"} \\\\ \n $l$   &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR}  &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{WZ}  &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{BP} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR-MW$^l_{0}$} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{MW-MW$^l_{0}$}    \\\\ \n",
              " \\hline  &  \\multicolumn{"%+%ncol(tab)%+%"}{c}{"%+%distrs[4]%+%"} \\\\ \n $l$   &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR}  &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{WZ}  &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{BP} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR-MW$^l_{0}$} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{MW-MW$^l_{0}$}    \\\\ \n"
    )))
    
}


# compare distributions of gamma and lognormal distributed data
library(kyotil)
library(robustrank)
myfigure(mfrow=c(1,2), width=7, height=6)
    m=200
    n=75
    ylim=c(0,14.5)
    
    distr="lognormal"
    dat.2=sim.partially.matched(m=m,n.x=0,n.y=0,distr=distr,params=c(loc.2=0.3,rho=0,scale.2=1),seed=1)
    boxplot(list(dat.2$X, dat.2$Y), main=distr, ylim=ylim, names=c("X","Y"))
    wilcox.test(dat.2$X[1:n], dat.2$Y[1:n], paired=T)
    wilcox.test(dat.2$X[1:n], dat.2$Y[1:n], paired=F)
    mtext(side=3, line=2.5, adj=-.1, text="(b)", cex=1.4, font=2, xpd=NA)    
    
    distr="gamma"
    dat.1=sim.partially.matched(m=m,n.x=0,n.y=0,distr=distr,params=c(loc.2=0.4,shape.1=3,shape.2=3,rate.1=1,rate.2=1,rho=0),seed=1)
    boxplot(list(dat.1$X, dat.1$Y), main=distr, ylim=ylim, names=c("X","Y"))
    wilcox.test(dat.1$X[1:n], dat.1$Y[1:n], paired=T)
    wilcox.test(dat.1$X[1:n], dat.1$Y[1:n], paired=F)
    mtext(side=3, line=2.5, adj=-.1, text="(a)", cex=1.4, font=2, xpd=NA)        
mydev.off(file="figures/simulated_data_boxplot", ext="pdf")


# power and size for small number of missingness
for (i in c(1:4)) { # i=1 for size, i=2,3,4 for power; i=1,2 10% missingness, i=3 20% missingness, i=4 40% missingness
#i=1
    
    library(kyotil)    
    method=if(i==1) "asym" else "pow"
    reses=list()
    for (m in c(50,20))  {
        rhos=c(0,0.5,0.8); names(rhos)=rhos
        distrs=c("logistic","normal","lognormal","gamma")
        for (distr in distrs) {                
            lxs=floor(m*c(0.1,.25,.5,1)); names(lxs)=lxs
            res=sapply(rhos, simplify="array", function (rho){            
                if(distr=="normal" | distr=="lognormal") {
                    param=(if(i==1) "0" else if(m==50) ".3" else ".5")%+%","%+%rho%+%",1"
                } else if (distr=="logistic") {
                    param=(if(i==1) "0" else if(m==50) ".5" else "1")%+%","%+%rho%+%",1"
                } else if (distr=="gamma") {
                    param=(if(i==1) "0" else if(m==50) "0.4" else "0.6")%+%",3,3,1,1,"%+%rho
                } else stop("no distr")
                
                if(i==1 | i==2) sim=paste(distr,m,lx=floor(m/20),n=floor(m/20),param,sep="_") 
                if(i==3)        sim=paste(distr,m,lx=floor(m/5 ),n=floor(m/5 ),param,sep="_") 
                if(i==4)        sim=paste(distr,m,lx=floor(m/2 ),n=floor(m/2 ),param,sep="_") 
                print(method%+%" "%+%sim)
                stats=get.sim.res ("res_"%+%method%+%"/"%+%sim, verbose=F)
                apply(stats, 1, function(x) mean(x[1:10e3]<0.05,na.rm=T)) # na should be very few, so the choice of na.action does not matter here
            }) 
            reses=c(reses, list(res))   
        }                
    }
    
    tab=rbind(
        do.call(cbind, lapply(c("SR","WZ","mw.mw.00","sr.mw.20", "mw.mw.20"), function (i) t(reses[[1]][i,]) )),
        do.call(cbind, lapply(c("SR","WZ","mw.mw.00","sr.mw.20", "mw.mw.20"), function (i) t(reses[[2]][i,]) )),
        do.call(cbind, lapply(c("SR","WZ","mw.mw.00","sr.mw.20", "mw.mw.20"), function (i) t(reses[[3]][i,]) )),
        do.call(cbind, lapply(c("SR","WZ","mw.mw.00","sr.mw.20", "mw.mw.20"), function (i) t(reses[[4]][i,]) )),
        do.call(cbind, lapply(c("SR","WZ","mw.mw.00","sr.mw.20", "mw.mw.20"), function (i) t(reses[[5]][i,]) )),
        do.call(cbind, lapply(c("SR","WZ","mw.mw.00","sr.mw.20", "mw.mw.20"), function (i) t(reses[[6]][i,]) )),
        do.call(cbind, lapply(c("SR","WZ","mw.mw.00","sr.mw.20", "mw.mw.20"), function (i) t(reses[[7]][i,]) )),
        do.call(cbind, lapply(c("SR","WZ","mw.mw.00","sr.mw.20", "mw.mw.20"), function (i) t(reses[[8]][i,]) ))
    )
    rownames(tab)=c(distrs,distrs)
    names(dimnames(tab))=c("$\\rho$",NA) #$m\\backslash\\rho$
    round(tab, 2)
    if (i==1) tab=tab[,-(1:3)]    
    
    m=50
    if(i==1 | i==2) s1=paste("m=",m,",l=",floor(m/20),",n=",n=floor(m/20),sep="") 
    if(i==4)        s1=paste("m=",m,",l=",floor(m/2 ),",n=",n=floor(m/2 ),sep="") 
    if(i==3)        s1=paste("m=",m,",l=",floor(m/5 ),",n=",n=floor(m/5 ),sep="") 
    m=20
    if(i==1 | i==2) s2=paste("m=",m,",l=",floor(m/20),",n=",n=floor(m/20),sep="") 
    if(i==4)        s2=paste("m=",m,",l=",floor(m/2 ),",n=",n=floor(m/2 ),sep="") 
    if(i==3)        s2=paste("m=",m,",l=",floor(m/5 ),",n=",n=floor(m/5 ),sep="") 
    mytex(tab, file="tables/"%+%(if(method=="asym") "sizes" else method)%+%"_pm_extended_low_missing_"%+%i, sanitize.text.function = function(x) x, include.colnames = T, include.rownames=T, include.dup.rownames=T, digits=2
        , align=c("c","c",rep(c("c","c","c|"),if(i==1) 3 else 4),"c","c","c")
        , add.to.row=list(list(0,4),
            c("          &  \\multicolumn{"%+%ncol(tab)%+%"}{c}{$"%+%s1%+%"$} \\\\ \n   "%+%(if(i!=1) "&  \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR}  " else "")%+%"&  \\multicolumn{"%+%(length(rhos))%+%"}{c}{WZ}  &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{BP} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR-MW$^l_{0}$} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{MW-MW$^l_{0}$}    \\\\ \n",  
              " \\hline  &  \\multicolumn{"%+%ncol(tab)%+%"}{c}{$"%+%s2%+%"$} \\\\ \n   "%+%(if(i!=1) "&  \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR}  " else "")%+%"&  \\multicolumn{"%+%(length(rhos))%+%"}{c}{WZ}  &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{BP} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{SR-MW$^l_{0}$} &  \\multicolumn{"%+%(length(rhos))%+%"}{c}{MW-MW$^l_{0}$}    \\\\ \n")
    ))
    
}
>>>>>>> ba84d8cf4f433c2350eb8f7e17f2d10b0a322a2e

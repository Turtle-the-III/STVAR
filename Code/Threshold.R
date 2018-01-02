### NOTES ###

#zzz[43]<-2
#zzz[71]<-1
#zzz[149]<-1
#zzz[156]<-2



### HEAD ###
#rm(list=ls(all=TRUE))
master<-FALSE
if(exists("GLOBAL_master")){master<-TRUE}

if(!master){
	setwd("C:/Users/SZV6W1/Documents/ERFIN")
}
par(mfrow=c(1,1))
dev.off()

### BIBLIOTEKA ###
library(vars)
library(forecast)
library(tseries)
library(urca)
library(Hmisc)
library(mFilter)
library(gtrendsR)
library(spikeslab)
library(bsts)
library(lubridate)
library(plyr)
library(parallel)
library(pryr)
library(magrittr)
library(doParallel)
library(iterators)
library(foreach)
#library(xlsx)
library(tsDyn)
library(ggplot2)
#library(MSBVAR)

### Zródla
source("./PrivateLib.R")
source("./Holidays.R")

### PARAMETRY ###
pamiec<-12
szlif<-2
p_lag<-2
#4.7
# 33 51
#down<-(3.134956)
#up<-(4.512596)

### DANE ###
dane_m<-na.omit(read.csv2("./Dane_ctrl.csv",1)[,1:15])
pocz<-as.Date(dane_m[1,1])
dane_m<-ts(dane_m[,-1],start=c(year(pocz),month(pocz)),freq=12)
pengab<-ts(read.csv2("./ESI.csv",1)[,2],start=c(1993,10),freq=12)


### DANE PREP ###
source("./DataPrep.R")

### SVAR MATRICES ###
u_xS<-matrix(0,6,6)
u_xS[1,c(2)]<-NA
u_xS[2,c(1)]<-NA
u_xS[3,c(1,2)]<-NA
u_xS[4,c(1,4,5)]<-NA
u_xS[5,c(2,4,5)]<-NA
u_xS[6,c(3,4,5,6)]<-NA
diag(u_xS)<-1

u_mon_xS<-matrix(0,5,5)
u_mon_xS[lower.tri(u_mon_xS)]<-NA
diag(u_mon_xS)<-1
u_mon_xSxW<-u_mon_xS
u_mon_xSxW[4,1]<-0
u_mon_xSxW[1,4]<-NA
u_mon_xSxW[5,1]<-0
u_mon_xSxW[1,5]<-NA
u_mon_xSxW[3,1]<-0

### DATA CREATION ###
dane_sent<-cbind(
	diff(wynagrodzenie),
	diff(zatrudnienie),
	diff(wspol_kapital),	
	diff(konsumpcja),
	diff(produkcja),
	diff(kredyty)
	)

#dane_sent<-window(dane_sent,start=c(1993,1))

poczsent<-pocz+months(1)+months(pamiec)

sentiments<-get_sents(dane_sent,u_xS,pamiec,szlif)

sent<-ts(cumsum(sentiments$sent/3),start=c(year(poczsent),month(poczsent)),freq=12)
org<-sentiments$org
max(roots(sentiments$model$var))

dane_monet<-cbind(
	pkb,
	cpi,
	wibor,
	reer
	)

dane_monet<-window(dane_monet,start=c(1994,4))
sent<-window(sent,start=c(1994,4))
pengab<-window(pengab,start=c(1994,4),end=end(sent))

VARselect(dane_monet)

#TVAR.LRtest(dane_monet,lag=p_lag,model="TAR",thVar=sent,test="1vs",nboot=100)
model_xS_R2<-TVAR(dane_monet,lag=p_lag,nthresh=1,model="TAR",thVar=sent,trim=0.23)
down<-model_xS_R2$model.specific$Thres[1]
model_xS_R22<-TVAR(dane_monet[c(1,1,na.omit(regime(model_xS_R2)))>1,],trim=0.26,lag=p_lag,nthresh=1,model="TAR",thVar=-sent[c(1,1,na.omit(regime(model_xS_R2)))>1])
up<-(-model_xS_R22$model.specific$Thres[1])
model_xS_R3<-TVAR(dane_monet,lag=p_lag,nthresh=2,model="TAR",thVar=sent,trim=0.01)
model_xS_R3P<-TVAR(dane_monet,lag=p_lag,nthresh=3,model="TAR",thVar=pengab)

varcovar_xS_R2<-t(model_xS_R2$residual) %*% model_xS_R2$residual
varcovar_xS_R3<-t(model_xS_R3$residual) %*% model_xS_R3$residual
LR<-log(det(varcovar_xS_R2))-log(det(varcovar_xS_R3))
pchisq(LR,model_xS_R2$df.residual-model_xS_R3$df.residual)


reg_xS_R2<-regime(model_xS_R2)
table(reg_xS_R2)

reg_xS_R3<-regime(model_xS_R3)
table(reg_xS_R3)

reg_xS_R3P<-regime(model_xS_R3P)
table(reg_xS_R3P)


#zzz<-reg_xS_R3
zzz<-rep(2,length(sent))
uup<-which(sent>up)+1
uup<-uup[uup<=length(sent)]
ddown<-which(sent<down)+1
ddown<-ddown[ddown<=length(sent)]
zzz[uup]<-3
zzz[ddown]<-1
zzz[1]<-1
zzz<-ts(zzz,start=c(1994,4),freq=12)
table(zzz)

model_xS_R3M<-TVAR(dane_monet,lag=p_lag,nthresh=3,model="TAR",thVar=zzz)
reg_xS_R3M<-regime(model_xS_R3M)

model_xS_R2<-TVAR(dane_monet,lag=p_lag,nthresh=1,model="TAR",thVar=pengab,trim=0.01)
pdown<-model_xS_R2$model.specific$Thres[1]
model_xS_R22<-TVAR(dane_monet[c(1,1,na.omit(regime(model_xS_R2)))>1,],trim=0.01,lag=p_lag,nthresh=1,model="TAR",thVar=-pengab[c(1,1,na.omit(regime(model_xS_R2)))>1])
pup<-(-model_xS_R22$model.specific$Thres[1])
model_xS_R3<-TVAR(dane_monet,lag=p_lag,nthresh=2,model="TAR",thVar=pengab,trim=0.01)

#zzz<-reg_xS_R3
zzzp<-rep(2,length(pengab))
uupp<-which(pengab>pup)+1
uupp<-uup[uupp<=length(pengab)]
ddownp<-which(pengab<pdown)+1
ddownp<-ddownp[ddownp<=length(pengab)]
zzzp[uupp]<-3
zzzp[ddownp]<-1
zzzp[1]<-1
zzzp<-ts(zzzp,start=c(1994,4),freq=12)
table(zzzp)

model_xS_R3P<-TVAR(dane_monet,lag=p_lag,nthresh=3,model="TAR",thVar=zzzp)
reg_xS_R3P<-regime(model_xS_R3P)

model_oS<-TVAR(dane_monet,lag=p_lag,nthresh=2,model="TAR")
reg_oS<-regime(model_oS)

Amtx<-matrix(NA,ncol(dane_monet),ncol(dane_monet))
Amtx[upper.tri(Amtx)]<-0
diag(Amtx)<-1
Bmtx<-matrix(0,ncol(dane_monet),ncol(dane_monet))
diag(Bmtx)<-NA


md<-VAR(dane_monet,p=p_lag)
podzial<-model_split(model_xS_R2,md,reg_xS_R2[-c(1:p_lag)],Amtx,Bmtx," ",2)
mddw_xS_R2<-podzial$down
mdup_xS_R2<-podzial$middle

#, bo w sumie modele z tego z dupy wychodza
podzial<-model_split(model_xS_R3,md,reg_xS_R3[-c(1:p_lag)],Amtx,Bmtx," ")
mdup_xS_R3<-podzial$up
mdmd_xS_R3<-podzial$middle
mddw_xS_R3<-podzial$down

podzial<-model_split(model_xS_R3M,md,zzz[-c(1:p_lag)],Amtx,Bmtx," ")
mdup_xS_R3M<-podzial$up
mdmd_xS_R3M<-podzial$middle
mddw_xS_R3M<-podzial$down
#mdup_xS_R3M_svar<-szbvar(ts(podzial$up$var$y,freq=12),2,,0.5,0.5,1,100,0,0,0,prior=1)
#mdmd_xS_R3M_svar<-podzial$middle
#mddw_xS_R3M_svar<-podzial$down

podzial<-model_split(model_xS_R3P,md,zzzp[-c(1:p_lag)],Amtx,Bmtx," ")
mdup_xS_R3P<-podzial$up
mdmd_xS_R3P<-podzial$middle
mddw_xS_R3P<-podzial$down

podzial<-model_split(model_oS,md,reg_oS[-c(1:p_lag)],Amtx,Bmtx," ")
mdup_oS<-podzial$up
mdmd_oS<-podzial$middle
mddw_oS<-podzial$down
md<-SVAR(md,Amat=Amtx,Bmat=Bmtx,max.iter=1000)

md

mddw_xS_R2
mdup_xS_R2

mdup_xS_R3
mdmd_xS_R3
mddw_xS_R3

mdup_xS_R3M
mdmd_xS_R3M
mddw_xS_R3M

mdup_oS
mdmd_oS
mddw_oS

max(roots(md$var))

max(roots(mdup_xS_R2$var))
max(roots(mddw_xS_R2$var))

max(roots(mdup_xS_R3$var))
max(roots(mdmd_xS_R3$var))
max(roots(mddw_xS_R3$var))

max(roots(mdup_xS_R3M$var))
max(roots(mdmd_xS_R3M$var))
max(roots(mddw_xS_R3M$var))

max(roots(mdup_xS_R3P$var))
max(roots(mdmd_xS_R3P$var))
max(roots(mddw_xS_R3P$var))

max(roots(mdup_oS$var))
max(roots(mdmd_oS$var))
max(roots(mddw_oS$var))

#input_model_VAR<-list(md$var,mdup_xS$var,mdmd_xS$var,mddw_xS$var)
input_model_R2<-list(md,md,mddw_xS_R2,mdup_xS_R2)
input_model_R3<-list(md,mdup_xS_R3,mdmd_xS_R3,mddw_xS_R3)
input_model_0<-list(md,mdup_xS_R3M,mdmd_xS_R3M,mddw_xS_R3M)
#input_model<-list(md,mdup_oS,mdmd_oS,mddw_oS)


#### PLOTS
if(!master){
dev.new()
plot(model_xS_R2)

dev.new()
plot(reg_xS_R2,main="Regimes raw sentiments #2",ylab="Regime")

dev.new()
plot(model_xS_R3)

dev.new()
plot(reg_xS_R3,main="Regimes raw sentiments",ylab="Regime")

dev.new()
#par(mfrow=c(1,2))
plot(sent,lwd=2,main="Major events",ylab="Sentiments",xlab="Years")
abline(v=1997+(8/12),col="blue")
abline(v=2001+(8/12),col="blue")
abline(v=2003+(5/12),col="darkgreen")
abline(v=2004+(4/12),col="darkgreen")
abline(v=2005+(8/12),col="blue")
abline(v=2008+(8/12),col="red")
abline(v=2010+(4/12),col="red")
abline(v=2011+(9/12),col="blue")
abline(v=2015+(4/12),col="blue")
abline(v=2015+(9/12),col="blue")
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom",legend=c("Elections","EU Accession","Economic crises"),col=c("blue","darkgreen","red"), pch =15,bg="white",horiz=TRUE,xpd=TRUE, cex = 1.2)
dev.print(pdf,"SentyEvent.pdf")

dev.new()
plot(sent,lwd=2,main="Regimes",ylab="Sentiments",xlab="Years")
abline(model_xS_R3$model.specific$Thres[1],0,col="red",lty=4)
abline(model_xS_R3$model.specific$Thres[2],0,col="blue",lty=4)
abline(down,0,col="red")
abline(up,0,col="blue")
dev.print(pdf,"SentyRegimes.pdf")


dev.new()
plot((sent-mean(sent))/sd(sent),lwd=2,main="ESI vs Sentimets",ylab="Sentiments",xlab="Years",ylim=c(-3,3))
lines((pengab-mean(pengab))/sd(pengab),lwd=2,col="blue")
dev.print(pdf,"SentyvsPENGAB.pdf")



dev.new()
plot(zzz,type="l",ylab="Regime",main="Regimes manual sentiments")

dev.new()
plot(reg_xS_R3M,main="Regimes expert smoothing",ylab="Regime")

dev.new()
plot(reg_oS, main="Regimes raw data",ylab="Regime")

if (FALSE){
#plot_own_irf(input_model,c("wibor","cpi"),"Wibor impulse on CPI - VAR")

#2 regimes

plot_own_irf(input_model_R2,c("wibor","cpi"),"Monetary impulse on CPI - TVAR #2")
plot_own_irf(input_model_R2,c("wibor","gdp"),"Monetary impulse on Output - TVAR #2")
plot_own_irf(input_model_R2,c("wibor","reer"),"Monetary impulse on REER - TVAR #2")


plot_own_irf(input_model_R3,c("wibor","cpi"),"Monetary impulse on CPI - TVAR #3")
#plot_own_irf(input_model_R3,c("pkb","cpi"),"GDP impulse on CPI - TVAR #3")
plot_own_irf(input_model_R3,c("wibor","pkb"),"Wibor impulse on GDP - TVAR #3")
plot_own_irf(input_model_R3,c("wibor","reer"),"Wibor impulse on REER - TVAR #3")


plot_own_irf(input_model_0,c("wibor","cpi"),"Monetary impulse on CPI - TVAR #3 manual")
#plot_own_irf(input_model_0,c("pkb","cpi"),"GDP impulse on CPI - TVAR #3 manual")
plot_own_irf(input_model_0,c("wibor","pkb"),"Wibor impulse on GDP - TVAR #3 manual")
plot_own_irf(input_model_0,c("wibor","reer"),"Wibor impulse on REER - TVAR #3 manual")


#plot_own_irf(input_model,c("wibor","cpi"),"Monetary impulse on CPI - VAR oS")
#plot_own_irf(input_model,c("pkb","cpi"),"GDP impulse on CPI - VAR oS")
#plot_own_irf(input_model,c("wibor","pkb"),"Wibor impulse on GDP - VAR oS")
}
}

grangertest(sent ~ dane_monet[,3], order=3)
grangertest(dane_monet[,3]~sent, order=3)

rmse<-function(x){
	return(sqrt(mean(x^2)))
}
normz<-function(x){
	return((x-mean(x))/sd(x))
}

get_sents<-function(f_dane,upsilon,opoz=1,okres=1){
	model_var<-VAR(f_dane,p=opoz)
	Bmacierz<-matrix(0,ncol(f_dane),ncol(f_dane))
	diag(Bmacierz)<-NA
	model_svar<-SVAR(model_var,max.iter=1000,Amat=upsilon,Bmat=Bmacierz,estmethod="direct")
	
	t(residuals(model_var))->res
	signals<-(solve(model_svar$A)%*%model_svar$B%*%res)
	signals<-t(apply(signals,1,normz))

	x2<-NULL

	for(i in 1:(ncol(f_dane)/2)){
	x2<-cbind(
		x2,
		signals[(ncol(f_dane)/2)+i,]-apply(as.matrix(signals)[(1:(ncol(f_dane)/2))[-i],],2,sum)
		)
	}
	x2<-apply(x2,2,normz)
	ax2<-apply(x2,1,sum)
	ax2<-c(
		forecast::forecast(arima(ts(rev(ax2)),order=c(3,0,0)),h=3)$mean,
		ax2,
		forecast::forecast(arima(ts(ax2),order=c(3,0,0)),h=3)$mean
	)	
	szereg<-hpfilter(ts(ax2),type="lambda",freq=okres)
	szereg<-head(tail(szereg$trend,-3),-3)
	return(list(model=model_svar,org=x2,sent=szereg))
}

inter_daty<-function(pocz,kon){
	dni<-seq(as.Date(pocz,"%d-%m-%Y"),as.Date(kon,"%d-%m-%Y"),by=1)
	dni<-dni[weekdays(dni)!="Sunday"]
	dni<-dni[!(dni %in% holidays)]
	dni<-cbind(dni,1)
	dni[weekdays(as.Date(dni[,1]))=="Saturday",2]<-sob
	dni<-aggregate(dni[,2],by=list(format(as.Date(dni[,1]),"%Y-%m")),sum)
	return(dni)
}

zrob_zmienna<-function(input,czy.log=FALSE){
	x<-ts(c(1,cumprod(input/100)),start=c(2006,12),freq=12)
	if (czy.log) {
		x<-ts(log(c(1,cumprod(input/100))),start=c(2006,12),freq=12)
	}
	xd<-stl(x,s.window="periodic")
	x_xS<-xd$time.series[,2]+xd$time.series[,3]
	x_tr<-xd$time.series[,2]
	return(list(org=x,no.seas=x_xS,seas=x_tr))
}


plot_own_irf_aux<-function(model,imp,res,tyt,trim=FALSE,cien=FALSE,wys=NULL){
	Amtx<-model$A
	Amtx[Amtx!=0]<-NA
	Bmtx<-model$B
	Bmtx[Bmtx!=0]<-NA

	assign("Amtx",Amtx,envir = .GlobalEnv)
	assign("Bmtx",Bmtx,envir = .GlobalEnv)

	skalar<-model$B[res,res]
	rys<-vars::irf(model,impulse=c(imp),response=c(res),n.ahead=84,ci=0.95,runs=1000,seed=2017)
	rys2<-vars::irf(model,impulse=c(imp),response=c(res),n.ahead=84,ci=0.69,runs=1000,seed=2017)

	y_przedzial<-wys

	if (is.null(wys)){
		y_przedzial<-c(
			min(0,rys$irf[[1]]/skalar,rys$Lower[[1]]/skalar)*1.02,
			max(0,rys$irf[[1]]/skalar,rys$Upper[[1]]/skalar)*1.02
		)
	}
	
	

	if (trim){
		y_przedzial<-c(
			min(0,rys$irf[[1]]/skalar)*1.02,
			max(0,rys$irf[[1]]/skalar)*1.02
		)
	}

	plot(
		rys$irf[[1]]/skalar,
		type="n",
		ylab=res,
		ylim=y_przedzial,
		main=tyt,
		col="blue",
		xlab="Months",
		lwd=2
	)

	if (cien){
		polygon(c(1:61,61:1),c(rys$Lower[[1]]/skalar,rev(rys$Upper[[1]]/skalar)),col="chocolate",border=NA)
		polygon(c(1:61,61:1),c(rys2$Lower[[1]]/skalar,rev(rys2$Upper[[1]]/skalar)),col="red",border=NA)
	}

	abline(0,0,lwd=1)
	lines(rys$Lower[[1]]/skalar,lty=4,col="chocolate")
	lines(rys$Upper[[1]]/skalar,lty=4,col="chocolate")
	lines(rys2$Lower[[1]]/skalar,lty=4,col="red")
	lines(rys2$Upper[[1]]/skalar,lty=4,col="red")
	lines(rys$irf[[1]]/skalar,col="royalblue",lwd=2)
	lines(((rys2$Lower[[1]]+rys2$Upper[[1]])/skalar)/2,col="blue",lty=4)
	
}

plot_own_irf_aux2<-function(model,imp,res,tyt){
	rys<-vars::irf(mdup,impulse=c("reer"),response=c("cpi"),n.ahead=60)

	rysrys<-NULL
	for(i in 1:3){
		rysrys<-cbind(rysrys,rys[[i]][[1]])
	}
	rysrys<-as.data.frame(rysrys)
	names(rysrys)<-c("Irf","Lower","Upper")

	ggplot(rysrys)+
		geom_line(aes(0:36,0))+
		geom_line(colour="blue",size=1.2,aes(0:60,Irf))+
		geom_line(colour="red", linetype="dashed",aes(0:60,Lower))+
		geom_line(colour="red", linetype="dashed",aes(0:60,Upper))
}


plot_own_irf<-function(f_model,f_impres,f_tyt,master=FALSE){
	dev.new(width=15,height=15)
	par(mfrow=c(2,2),omi=c(0.2,0.2,0.8,0.2))
	podtytul<-c("ALL","UP","MIDDLE","DOWN")

	for(i in 1:4){
		plot_own_irf_aux(f_model[[i]],f_impres[1],f_impres[2],podtytul[i],max(roots(f_model[[i]]$var))>1)
		#podtytul[i]=="MIDDLE"
	}

	title(f_tyt,outer=TRUE)
}



model_split<-function(f_tvar,f_podstawa,f_regime,Amtx,Bmtx,ctrl,n_regime=3){
	output_nazwy<-c("down","middle","up")
	
	down<-NULL
	middle<-NULL
	up<-NULL

	for (i in 1:n_regime){
		model<-f_podstawa
		if (ctrl=="COM"){
			model$varresult$commodity$coefficients<-c(f_tvar$coefficients[[i]][1,-1],f_tvar$coefficients[[i]][1,1])
			model$varresult$pkb$coefficients<-c(f_tvar$coefficients[[i]][2,-1],f_tvar$coefficients[[i]][2,1])
			model$varresult$cpi$coefficients<-c(f_tvar$coefficients[[i]][3,-1],f_tvar$coefficients[[i]][3,1])
			model$varresult$wibor$coefficients<-c(f_tvar$coefficients[[i]][4,-1],f_tvar$coefficients[[i]][4,1])
			model$varresult$reer$coefficients<-c(f_tvar$coefficients[[i]][5,-1],f_tvar$coefficients[[i]][5,1])

			model$varresult$commodity$fitted.values<-c(as.matrix(model$datamat[which(f_regime==i),-c(1:model$K)])%*% as.matrix(model$varresult$commodity$coefficients))
			model$varresult$pkb$fitted.values<-c(as.matrix(model$datamat[which(f_regime==i),-c(1:model$K)])%*% as.matrix(model$varresult$pkb$coefficients))						
			model$varresult$cpi$fitted.values<-c(as.matrix(model$datamat[which(f_regime==i),-c(1:model$K)])%*% as.matrix(model$varresult$cpi$coefficients))
			model$varresult$wibor$fitted.values<-c(as.matrix(model$datamat[which(f_regime==i),-c(1:model$K)])%*% as.matrix(model$varresult$wibor$coefficients))
			model$varresult$reer$fitted.values<-c(as.matrix(model$datamat[which(f_regime==i),-c(1:model$K)])%*% as.matrix(model$varresult$reer$coefficients))

			model$varresult$commodity$residuals<-model$varresult$commodity$fitted.values-model$datamat[which(f_regime==i),1]
			model$varresult$pkb$residuals<-model$varresult$pkb$fitted.values-model$datamat[which(f_regime==i),2]
			model$varresult$cpi$residuals<-model$varresult$cpi$fitted.values-model$datamat[which(f_regime==i),3]
			model$varresult$wibor$residuals<-model$varresult$wibor$fitted.values-model$datamat[which(f_regime==i),4]
			model$varresult$reer$residuals<-model$varresult$reer$fitted.values-model$datamat[which(f_regime==i),5]
		} else if (ctrl=="EA"){
			model$varresult$ea$coefficients<-c(f_tvar$coefficients[[i]][1,-1],f_tvar$coefficients[[i]][1,1])
			model$varresult$pkb$coefficients<-c(f_tvar$coefficients[[i]][2,-1],f_tvar$coefficients[[i]][2,1])
			model$varresult$cpi$coefficients<-c(f_tvar$coefficients[[i]][3,-1],f_tvar$coefficients[[i]][3,1])
			model$varresult$wibor$coefficients<-c(f_tvar$coefficients[[i]][4,-1],f_tvar$coefficients[[i]][4,1])
			model$varresult$reer$coefficients<-c(f_tvar$coefficients[[i]][5,-1],f_tvar$coefficients[[i]][5,1])

			model$varresult$ea$fitted.values<-c(as.matrix(model$datamat[which(f_regime==i),-c(1:model$K)])%*% as.matrix(model$varresult$ea$coefficients))
			model$varresult$pkb$fitted.values<-c(as.matrix(model$datamat[which(f_regime==i),-c(1:model$K)])%*% as.matrix(model$varresult$pkb$coefficients))						
			model$varresult$cpi$fitted.values<-c(as.matrix(model$datamat[which(f_regime==i),-c(1:model$K)])%*% as.matrix(model$varresult$cpi$coefficients))
			model$varresult$wibor$fitted.values<-c(as.matrix(model$datamat[which(f_regime==i),-c(1:model$K)])%*% as.matrix(model$varresult$wibor$coefficients))
			model$varresult$reer$fitted.values<-c(as.matrix(model$datamat[which(f_regime==i),-c(1:model$K)])%*% as.matrix(model$varresult$reer$coefficients))

			model$varresult$ea$residuals<-model$varresult$ea$fitted.values-model$datamat[which(f_regime==i),1]
			model$varresult$pkb$residuals<-model$varresult$pkb$fitted.values-model$datamat[which(f_regime==i),2]
			model$varresult$cpi$residuals<-model$varresult$cpi$fitted.values-model$datamat[which(f_regime==i),3]
			model$varresult$wibor$residuals<-model$varresult$wibor$fitted.values-model$datamat[which(f_regime==i),4]
			model$varresult$reer$residuals<-model$varresult$reer$fitted.values-model$datamat[which(f_regime==i),5]
		} else {
			model$varresult$pkb$coefficients<-c(f_tvar$coefficients[[i]][1,-1],f_tvar$coefficients[[i]][1,1])
			model$varresult$cpi$coefficients<-c(f_tvar$coefficients[[i]][2,-1],f_tvar$coefficients[[i]][2,1])
			model$varresult$wibor$coefficients<-c(f_tvar$coefficients[[i]][3,-1],f_tvar$coefficients[[i]][3,1])
			model$varresult$reer$coefficients<-c(f_tvar$coefficients[[i]][4,-1],f_tvar$coefficients[[i]][4,1])

			model$varresult$pkb$fitted.values<-c(as.matrix(model$datamat[which(f_regime==i),-c(1:model$K)])%*% as.matrix(model$varresult$pkb$coefficients))
			model$varresult$cpi$fitted.values<-c(as.matrix(model$datamat[which(f_regime==i),-c(1:model$K)])%*% as.matrix(model$varresult$cpi$coefficients))
			model$varresult$wibor$fitted.values<-c(as.matrix(model$datamat[which(f_regime==i),-c(1:model$K)])%*% as.matrix(model$varresult$wibor$coefficients))
			model$varresult$reer$fitted.values<-c(as.matrix(model$datamat[which(f_regime==i),-c(1:model$K)])%*% as.matrix(model$varresult$reer$coefficients))

			model$varresult$pkb$residuals<-model$varresult$pkb$fitted.values-model$datamat[which(f_regime==i),1]
			model$varresult$cpi$residuals<-model$varresult$cpi$fitted.values-model$datamat[which(f_regime==i),2]
			model$varresult$wibor$residuals<-model$varresult$wibor$fitted.values-model$datamat[which(f_regime==i),3]
			model$varresult$reer$residuals<-model$varresult$reer$fitted.values-model$datamat[which(f_regime==i),4]
		}		
		
		model$y<-model$y[which(f_regime==i),]

		model$obs<-(sum(f_regime==i))
		model$totobs<-(sum(f_regime==i))+f_podstawa$p

		model$datamat<-model$datamat[which(f_regime==i),]

		model<-SVAR(model,Amat=Amtx,Bmat=Bmtx,max.iter=1000)
		assign(output_nazwy[i],model)
	}
	return(list(down=down,middle=middle,up=up))
}


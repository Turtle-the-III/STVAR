GLOBAL_master<-TRUE

setwd("C:/Users/SZV6W1/Documents/ERFIN")
source("./Threshold.R",echo=FALSE)
source("./Threshold_BaseA.R",echo=FALSE)
source("./Threshold_BaseB.R",echo=FALSE)

input_list<-list(
	input_model_0,
	input_model_A,
	input_model_B
)

lapply(input_list,function(x){
		lapply(x,function(x){
			max(roots(x$var))
			}
		)}
)

dev.new(width=22.5,height=29)
par(mfrow=c(4,3),omi=c(0.4,0.4,0.4,0.1),mai=c(0.2,0.15,0.2,0.15))

for (i in 0:11){
  plot_own_irf_aux(input_list[[(i %% 3)+1]][[floor(i/3)+1]],"wibor","pkb","",,FALSE,c(-1,0.2))
}
title("WIBOR shock on Output",outer=TRUE)

mtext('BASE',at=.15,side=3,outer=TRUE,line=-1) 
mtext('SCEN #1',at=.50,side=3,outer=TRUE,line=-1) 
mtext('SCEN #2',at=.85,side=3,outer=TRUE,line=-1)  

mtext('DOWN',at=.15,side=2,outer=TRUE,line=1) 
mtext('MIDDLE',at=.35,side=2,outer=TRUE,line=1) 
mtext('UP',at=.65,side=2,outer=TRUE,line=1) 
mtext('ALL',at=.85,side=2,outer=TRUE,line=1) 

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom",legend=c("IRF","68% CI","95% CI"),col=c("royalblue","red","chocolate"), pch =15,bg="white",horiz=TRUE,xpd=TRUE, cex = 1.2)

dev.print(pdf,"WIBORonGDP.pdf")
dev.off()


dev.new(width=22.5,height=29)
par(mfrow=c(4,3),omi=c(0.4,0.4,0.4,0.1),mai=c(0.2,0.15,0.2,0.15))
for (i in 0:11){
	plot_own_irf_aux(input_list[[(i %% 3)+1]][[floor(i/3)+1]],"wibor","cpi","",,,c(-1.5,0.75))
}
title("WIBOR shock on CPI",outer=TRUE)

mtext('BASE',at=.15,side=3,outer=TRUE,line=-1) 
mtext('SCEN #1',at=.50,side=3,outer=TRUE,line=-1) 
mtext('SCEN #2',at=.85,side=3,outer=TRUE,line=-1)  

mtext('DOWN',at=.15,side=2,outer=TRUE,line=1) 
mtext('MIDDLE',at=.35,side=2,outer=TRUE,line=1) 
mtext('UP',at=.65,side=2,outer=TRUE,line=1) 
mtext('ALL',at=.85,side=2,outer=TRUE,line=1) 

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom",legend=c("IRF","68% CI","95% CI"),col=c("royalblue","red","chocolate"), pch =15,bg="white",horiz=TRUE,xpd=TRUE, cex = 1.2)

dev.print(pdf,"WIBORonCPI.pdf")
dev.off()


dev.new(width=22.5,height=29)
par(mfrow=c(4,3),omi=c(0.4,0.4,0.4,0.1),mai=c(0.2,0.15,0.2,0.15))

for (i in 0:11){
	plot_own_irf_aux(input_list[[(i %% 3)+1]][[floor(i/3)+1]],"wibor","reer","",,FALSE,c(-0.4,0.8))
}
title("WIBOR shock on REER",outer=TRUE)

mtext('BASE',at=.15,side=3,outer=TRUE,line=-1) 
mtext('SCEN #1',at=.50,side=3,outer=TRUE,line=-1) 
mtext('SCEN #2',at=.85,side=3,outer=TRUE,line=-1)  

mtext('DOWN',at=.15,side=2,outer=TRUE,line=1) 
mtext('MIDDLE',at=.35,side=2,outer=TRUE,line=1) 
mtext('UP',at=.65,side=2,outer=TRUE,line=1) 
mtext('ALL',at=.85,side=2,outer=TRUE,line=1) 

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom",legend=c("IRF","68% CI","95% CI"),col=c("royalblue","red","chocolate"), pch =15,bg="white",horiz=TRUE,xpd=TRUE, cex = 1.2)

dev.print(pdf,"WIBORonREER.pdf")
dev.off()

rm(list=ls(all=TRUE))


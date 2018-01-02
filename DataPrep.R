wynagrodzenie<-log(dane_m[,"wag"]/dane_m[,"cpi"])
#lm(wynagrodzenie[73:length(wynagrodzenie)]~c(73:length(wynagrodzenie)))
#lm(wynagrodzenie[1:72]~c(1:72))
#lin<-2.669539+0.001928*73
#lin2<-2.331842+0.004056*73
wynagrodzenie[1:72]<-wynagrodzenie[1:72]+0.182353

zatrudnienie<-log(dane_m[,"emp"])
#lm(wynagrodzenie[73:length(wynagrodzenie)]~c(73:length(wynagrodzenie)))
#lm(zatrudnienie[1:84]~c(1:84))
#lm(zatrudnienie[85:135]~c(85:135))
#lin<-8.6452037+0.0001796*84
#lin2<-8.843961-0.003019*84
#plot(zatrudnienie,type="l")
#abline(8.6452037,0.0001796)
#aux<-rep(0,length(zatrudnienie))
#aux[1:85]<-1
#auto.arima(zatrudnienie,xreg=aux,max.d=0)
zatrudnienie[1:84]<-zatrudnienie[1:84]-0.0699251

wspol_kapital<-log(dane_m[,"car"])
konsumpcja<-log(dane_m[,"rt"])
produkcja<-log(dane_m[,"pi"])
kredyty<-log(dane_m[,"loa"]/dane_m[,"cpi"])

pkb<-log(dane_m[,"pi"])
cpi<-log(dane_m[,"cpi"])
wibor<-dane_m[,"wibor_1m"]
reer<-log(dane_m[,"reer"])
commodity<-log(dane_m[,"all_com_pri_ind"])
m3<-log(dane_m[,"m3"])
ea<-log(dane_m[,"pi_ea"])
germany<-log(dane_m[,"pi_germany"])
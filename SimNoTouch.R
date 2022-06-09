set.seed(442)


S0<-100


sigma<-0.2

(r<-sigma^2/2)
mu<-r

capT<-1
strike<-102

Nhedge<-5000
Nrep<-10000

# HEDGE
# =====

St<-rep(S0, length=Nrep)
dt<-capT/Nhedge

VpfNoTouch <-rep(1, length=Nrep)


for(i in 2:Nhedge){
    St<-St*exp((mu-0.5*sigma^2)*dt +sigma*sqrt(dt)*rnorm(Nrep))	
    VpfNoTouch <- VpfNoTouch*(St<strike)
}




print(paste("Theoretical price =", round(exp(-r*capT)*(pnorm(log(strike/S0)/(sigma*sqrt(capT)))-pnorm(-log(strike/S0)/(sigma*sqrt(capT)))),6)))
print(paste("Average discounted notouch payoff =",round(exp(-r*capT)*mean(VpfNoTouch),6)))




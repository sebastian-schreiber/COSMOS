SIR=function(beta,gamma,N,I0,S0,T){
  S=matrix(NA,T,length(I0))
  S[1,]=S0
  I=matrix(NA,T,length(I0))
  I[1,]=I0
  for(t in 1:(T-1)){
    S[t+1,]=S[t,]*exp(-beta*I[t,]/N)
    I[t+1,]=S[t,]*(1-exp(-beta*I[t,]/N))+(1-gamma)*I[t,] 
  }
  R=N-S-I
  par(cex.axis=1.5,cex.lab=1.5)
  matplot(S,type="l",col="green",xlab="days",bty="n",ylab="abundances",lwd=2,ylim=c(0,N))
  matplot(I,type="l",col="red",add=TRUE,lwd=2)
  matplot(R,type="l",col="black",add=TRUE,lwd=2)
  legend("right",c("Susceptible","Infected","Recovered"),lty=1,col=c("green","red","black"),bty="n",cex=1.5,lwd=2)
}
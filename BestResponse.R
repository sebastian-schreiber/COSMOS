best.response=function(p1,p2,delta=0.001,x0=seq(0,1,length=15),T=1500){
  n=length(x0)
  x=matrix(0,T+1,n)
  x[1,]=x0
  for(t in 1:T){
    temp=x[t,]*(1-delta)
    become1=which(p1(x[t,])>p2(x[t,]))
    if(length(become1>0))temp[become1]=temp[become1]+delta
    x[t+1,]=temp
  }
  par(cex.lab=1.5,cex.axis=1.5)
  matplot(x,type="l",lty=1,lwd=2,xlab="time",ylab="frequency")
}
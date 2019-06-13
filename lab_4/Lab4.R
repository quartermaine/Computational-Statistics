############################################

set.seed(123456)
target_dist<-function(x){
  return(x^5*exp(-x))
  }

Rej<-0
f.MCMC.MH<-function(nstep,X0,props,output=T,draw_plot=T){
  X0<-X0
  vN<-1:nstep
  vecX<-rep(X0,nstep);
  
  for(i in 2:nstep){
    #browser()
    X<-vecX[i-1]
    Xcand<-rlnorm(1,meanlog=X,sdlog=props)
  
    u<-runif(1)
    num <- (target_dist(Xcand)*dlnorm(X,meanlog=Xcand,sdlog=props))
    den <- (target_dist(X)*dlnorm(Xcand,meanlog=X,sdlog=props))
    
    #a<-min(c(1,(target_dist(Y)*dlnorm(X,meanlog=Y,sdlog=props)/(target_dist(X)*dlnorm(Y,meanlog=X,sdlog=props)))))
    a<-min(1,num/den)
    
    if (u <=a){
        vecX[i]<-Xcand
    }
    else{
      vecX[i]<-X
      Rej<-Rej+1
      }
  
  }
  
  if(draw_plot==T){
    
  plot(vN,vecX,pch=19,cex=0.3,col="black",xlab="t",ylab="X(t)",
       main="",ylim=c(1,5),type="l")
  abline(h=0)
  abline(h=1,col="red")
  abline(h=5,col="red")
  }
  
 if(output==T){
  return(list("sample"=vecX,"rejections"=Rej))
 }
  
}



set.seed(123456)
f<-f.MCMC.MH(5000,rlnorm(1,0,1),1)

hist(f$sample,prob=T)
lines(density(f$sample))
f$rejections/5000

#target
x=seq(0,20,0.01)
y=target_dist(x)
plot(x,y,type="l",main="Target function",
     col="darkslateblue",lwd=2)
grid(25,25)

############################################


Rej1<-0
f.MCMC.MH1<-function(nstep,X0,output=T,draw_plot=T){
  vN<-1:nstep
  vecX<-rep(X0,nstep);
  for (i in 2:nstep){
    Xt<-vecX[i-1]
    Xcand<-rchisq(1,df=floor(Xt+1))
    
    u<-runif(1)
    num <- target_dist(Xcand)*dchisq(Xt, df=floor(Xcand+1))
    den <- target_dist(Xt)*dchisq(Xcand, df=floor(Xt+1))
    #a<-min(c(1,(target_dist(Y)*dchisq(Xt,df=floor(Y+1))/(target_dist(Xt)*dchisq(Y,df=floor(Xt+1))))))
    a<-min(1,num/den)
    if (u <=a){
      vecX[i]<-Xcand
    }
    else{
      vecX[i]<-Xt
      Rej1<-Rej1+1}
  }
  
  
  if(draw_plot==T){
  plot(vN,vecX,pch=19,cex=0.3,col="black",
       xlab="t",ylab="X(t)",main="",ylim=c(0,25),type="l")
  abline(h=0)
  abline(h=1.96)
  abline(h=-1.96)
  }
  if(output==T){
    return(list("sample"=vecX,"rejections"=Rej1))
  }
  
}



f2<-f.MCMC.MH1(5000,rchisq(1,1),T,T)


hist(f2$sample,probability = T)
lines(density(f2$sample))
f2$rejections/5000#rejection rate


#############################################

library(coda)
set.seed(123456)
nsample<-5000
steps<-seq(1,10,1)

X1<-matrix(0,length(steps),nsample)

for(step in steps){
  X1[steps,]<-f.MCMC.MH1(nsample,step,output=T,draw_plot=F)$sample
  
}

f1<-list()

for (i in 1:dim(X1)[1]){
  
  f1[[i]]<-as.mcmc(X1[i,])
}

print(gelman.diag(f1))



############################################

set.seed(123456)
s1<-f.MCMC.MH(5000,rlnorm(1,0,1),1,output=T,draw_plot=F)
mean(s1$sample)

s2<-f.MCMC.MH1(5000,rchisq(1,floor(1)),T,F)
mean(s2$sample)

###########################################


f.MCMC.Gibbs <- function(nstep,X0,sd){
  
  mX<-matrix(0,nrow=length(X0),ncol=nstep)
  
  for (t in 1:(nstep-1)){
    for (i in 1:length(X0)){
      if (i==1){
        y <- (mX[i+1,t]+X0[i])/2
        mX[i,t+1] <- rnorm(1,y,sd/sqrt(2))
      }
      else if (i ==50){
        y <- (mX[i-1,t+1]+X0[i])/2
        mX[i,t+1] <- rnorm(1, y, sd/sqrt(2))
      }
      else{
        y <- (mX[i+1,t]+ mX[i-1,t+1] +X0[i])/3
        mX[i,t+1] <- rnorm(1,y,sd/sqrt(3))
      }
    }
  }
 return(mX)
}


chemical<- data.frame(X,Y)
X0<-chemical$Y
gib <- f.MCMC.Gibbs(1000, X0, 0.2)


plot(chemical$X, chemical$Y, main="Day Vs Concentration", col="mediumslateblue",
     xlab="Day", ylab="Concentration", type="l",lwd=2)
points(chemical$X,rowMeans(gib), col="mediumvioletred", type="l",lwd=2)
legend("topleft", legend=c("concentration Y","sample mean"), col=c("mediumslateblue","mediumvioletred"),lty=19)
grid(25,25)

############################################################################


vN<-1:length(gib[50,])
plot(vN,gib[50,],pch=19,cex=0.3,col="black",xlab="t",ylab="X(t)",
     main="Trace Plot",ylim=c(0,2.5),type="o")
points(gib[50,1:2],col="mediumslateblue",type="o")
abline(h=1,col="red")
abline(h=2,col="red")















#Question 1

mortality<-read.csv2("mortality_rate.csv")
mortality$LMR<-log(mortality$Rate)

n=dim(mortality)[1]
set.seed(123456)
id=sample(1:n , floor(n*0.5))
train=mortality[id, ]
test=mortality[-id, ]

#pars=list(X,Y,X_test,Y_test)

myMSE<-function(lambda,pars,iterCounter = T){
  model<-loess(pars$Y~pars$X,enp.target = lambda)
  preds<-predict(model,newdata=pars$X_test)
  mse<-sum((pars$Y_test-preds)^2)/length(pars$Y_test)
  
  # If we want a iteration counter
  if(iterCounter){
    if(!exists("iterForMyMSE")){
      # Control if the variable exists in the global environemnt,
      # if not, create a variable and set the value to 1. This
      # would be the case for the first iteration
      # We will call the variable 'iterForMyMSE'
      assign("iterForMyMSE",
             value = 1,
             globalenv())
    } else {
      # This part is for the 2nd and the subsequent iterations.
      # Starting of with obtaining the current iteration number
      # and then overwrite the current value by the incremental
      # increase of the current value
      currentNr <- get("iterForMyMSE")
      assign("iterForMyMSE",
             value = currentNr + 1,
             globalenv())
    }
  }
  return(mse)
}


steps=seq(0.1,40,0.1)
mses<-double(length(steps))

mypars<-list(X=train$Day,Y=train$LMR,X_test=test$Day,Y_test=test$LMR)

for(i in steps){
  mses[which(i==steps)]<-myMSE(i,mypars)
  
}

optimal_l=steps[which.min(mses)]
optimal_mse=min(mses)

plot(steps,mses,main = "Mse vs lambda",
     col=ifelse(steps==optimal_l,"red","black"))
arrows(optimal_l+0.3, optimal_mse+0.3, x1 = optimal_l, y1 = optimal_mse,
       length = 0.15, angle = 30)
text(optimal_l+0.3,optimal_mse+0.32,labels=c("optimal mse value"))


#require(graphics)


cat("The number of ietrations is :",iterForMyMSE)

####################################################################
xmin <- optimize(myMSE,pars=mypars,
                 interval=c(0.1,40),maximum  = FALSE,tol=0.01)

cat("The new number of iterations is :", iterForMyMSE)


###################################################################


optim(c(35), myMSE,pars=mypars,
      method = c( "BFGS"))

cat("The new number of iterations is :", iterForMyMSE)

#Question 2

#load data


##################################################################
dat<-data

hist(dat)


#################################################################
#deriving the parameters estimators from normal function



################################################################

estimators<-function(x){
  n<-length(x)
  mean<-sum(x)/n
  sigma<-sum((x-mean)^2)/n
  return(c("mean"=mean,"sd"=sqrt(sigma)))
  }


res=estimators(dat)

##################################################################

loglike<-function(args,x){
  
  n<-length(x)
  logminus<- (-n*log(2*pi*args[2]^2)/2)-(sum((args[1]-x)^2)/(2*args[2]^2))
  logminus<- -1*logminus
  return(logminus)
}

gradient<-function(args,x){
  n<-length(x)
  gr_mu<-sum(x)-n*args[1]
  gr_sigma<-n*args[2]^2-sum((x-args[1])^2)
  return(c(gr_mu,gr_sigma))
}

#
myres<-optim(c(0,1),fn=loglike,gr=gradient,x=dat)

#with BFGS
myres1<-optim(c(0,1),fn=loglike,gr=gradient,x=dat,method = "BFGS")

#with Conjugate Gradient 
myres2<-optim(c(0,1),fn=loglike,gr=gradient,x=dat,method = "CG")











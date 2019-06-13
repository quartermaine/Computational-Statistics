
lottery<-readxl::read_excel("lottery.xls")
lottery<-as.data.frame(lottery)
head(lottery)

###############

Y=lottery$Draft_No
X=lottery$Day_of_year

plot(X,Y,type="l")
#
loessMod<-loess(Y~X)
loessMod10 <- loess(Y ~ X, span=0.15) # 15% smoothing span
loessMod25 <- loess(Y ~ X, span=0.25) # 25% smoothing span
loessMod50 <- loess(Y ~ X, span=0.50) #50% smoothing span

##
smoothed<-predict(loessMod)
smoothed10 <- predict(loessMod10) 
smoothed25 <- predict(loessMod25) 
smoothed50 <- predict(loessMod50) 

##
plot(x=X,y=Y, type="l", main="Loess Smoothing and Prediction", xlab="", ylab="")
lines(smoothed10, x=X, col="red",lwd=2)
lines(smoothed25, x=X, col="green",lwd=2)
lines(smoothed50, x=X, col="blue",lwd=2)
lines(smoothed,  x=X,col="purple",lwd=2)


###############

set.seed(12345)

B=2000
non_par_boot<-rep(0,B)

for(i in 1:B){
  
  Y_samp<-sample(1:length(Y),length(Y),replace=T)
  
  dat<-cbind(X,Y_samp)
  
  X_b<-dat[which.max(dat[,2])]
  X_a<-dat[which.min(dat[,2])]
  
  model<-loess(Y_samp ~ X,data=as.data.frame(dat),method="loess") 
  
  Y_xb<-model$fitted[X_b]
  Y_xa<-model$fitted[X_a]
  
  Tau<-(Y_xb-Y_xa)/(X_b-X_a)
  non_par_boot[i]<-Tau
  
  
}


hist(non_par_boot,breaks = 30,col="gray",include.lowest = TRUE,
     border="blue")



p_val<-sum(non_par_boot>=0)/B
p_val


###############



set.seed(12345)


B=2000

perm_func<-function(Y_value,X_value,B){
  
  call = match.call()
  
  dat_origin<-cbind(X_value,Y_value)
  Xb_origin<-X_value[which.max(Y_value)]
  Xa_origin<-X_value[which.min(Y_value)]
  
  model_origin<-loess(Y_value ~ X_value,data=as.data.frame(dat_origin),method="loess")

  Y_xb_origin<-model_origin$fitted[Xb_origin]
  Y_xa_origin<-model_origin$fitted[Xa_origin]

  Tau_origin<-(Y_xb_origin-Y_xa_origin)/(Xb_origin-Xa_origin)
  
  
  perm_Tau<-rep(0,B)
  
  for(i in 1:B){
    Y_perm<-sample(1:length(Y_value),length(Y_value),replace=F)
    dat_perm<-cbind(X_value,Y_perm)
  
    Xb<-dat_perm[which.max(dat_perm[,2])]
    Xa<-dat_perm[which.min(dat_perm[,2])]
  
    model_perm<-loess(Y_perm ~ X_value,data=as.data.frame(dat_perm),method="loess") 
  
    Y_xb<-model_perm$fitted[Xb]
    Y_xa<-model_perm$fitted[Xa]
  
    Tau_perm<-(Y_xb-Y_xa)/(Xb-Xa)
    perm_Tau[i]<-Tau_perm
  }
  
  perm_Tau<-perm_Tau
  p_value_perm<-sum(abs(perm_Tau)>=abs(Tau_origin))/B
  
  return(list(perm_Tau=perm_Tau,
              p_value_perm=p_value_perm,
         call=call))
    
}



res=perm_func(lottery$Draft_No,lottery$Day_of_year,B)
  


hist(res$perm_Tau,breaks = 30,col="gray",include.lowest = TRUE,
     border="blue")

res


################
set.seed(12345)

gen_data<-function(n,data,alpha){
  Y_gen<-c()
  
  for (i in 1:n){
    beta<-rnorm(1,183,10)
    x<-data[i]
    Y_gen[i]<-max(0,min(alpha*x+beta,366))
  }
  return(Y_gen)
}

Y_gen_data<-gen_data(366,lottery$Day_of_year,0.1)

res1<-perm_func(Y_gen_data,lottery$Day_of_year,200)

hist(res1$perm_Tau,breaks = 30,col="gray",include.lowest = TRUE,
     border="blue")

res1

################
set.seed(12345)

step<-seq(0.2,10,by=0.1)
tafs<-rep(0,length(step))

for (j in 1:length(step)){
  new_y_data<-gen_data(366,lottery$Day_of_year,step[j])
  p<-perm_func(new_y_data,lottery$Day_of_year,200)
  tafs[j]<-p$p_value_perm
}

tafs

signif_p <- sum(tafs<0.05)
signif_p




###############################################

prices<-readxl::read_excel("prices1.xls")
prices<-as.data.frame(prices)
head(prices)


###############

hist(prices$Price)
mean(prices$Price)

###############

set.seed(123456)
library(boot)


meanfun <- function(dat, idx) mean(dat[idx], na.rm = TRUE)

bot <- boot(prices$Price, statistic=meanfun, R=5000)
plot(bot)
#
bca_ci=boot.ci(bot, conf=0.95, type="bca",index=1)
(bca_ci)

ci_H=bca_ci$bca[ , c(4, 5)]

hist(bot$t[,1],main = 'Histogram with bca ci',xlab = 'mean values', col = 'aquamarine', prob = T)
lines(density(bot$t[,1]), col = 'midnightblue',lwd=2)
abline(v = ci_H, col = 'mediumvioletred',lwd=2)
#

perc_ci=boot.ci(bot,conf=0.95,type="perc",index = 1)

(perc_ci)

ci_H1=perc_ci$percent[ , c(4, 5)]

hist(bot$t[,1],main = 'Histogram with prec ci',xlab = 'mean values', col = 'mediumpurple', prob = T)
lines(density(bot$t[,1]), col = 'gold',lwd=2)
abline(v = ci_H1, col = 'mediumspringgreen',lwd=2)

#
norm_ci=boot.ci(bot,conf=0.95,type = "norm",index=1)

(norm_ci)
ci_H2=norm_ci$normal

hist(bot$t[,1], main = 'Histogram with normal ci',xlab = 'mean values', col = 'slategray1', prob = T)
lines(density(bot$t[,1]), col = 'purple4',lwd=2)
abline(v = ci_H2, col = 'mediumvioletred',lwd=2)


##Bias correction 
2*mean(prices$Price)-sum(bot$t)/5000

##variance of mean price
sum((bot$t-mean(bot$t))^2)/(5000-1)


###############################################################
#jackknife



jk_func = function (x, theta, ...) 
{
  call = match.call()
  n = length(x)
  u = rep(0, n)
  for (i in 1:n) {
    u[i] = theta(x[-i], ...)
  }
  theta.hat = theta(x, ...)
  pseudo.values = n*theta.hat - (n-1)*u
  theta.jack = mean(pseudo.values)
  jack.se = sqrt(sum((pseudo.values - theta.jack)^2)/(n*(n-1)))
  # jack.bias = theta.jack - theta.hat
  jack.bias = (n-1)*(theta.hat - mean(u))
  # 
  return(list(theta.hat = theta.hat,       
              theta.jack = theta.jack,     
              jack.bias = jack.bias,
              # jack.var = jack.se^2,
              jack.se = jack.se,            
              leave.one.out.estimates = u, 
              pseudo.values = pseudo.values,
              call = call))
}





n <- length(prices$Price)
theta <- median(prices$Price)
jk <- sapply(1 : n,function(i) mean(prices$Price[-i]))
mean_jk <- mean(jk)

bias_jk <- (n - 1) * (mean_jk - theta)
var_jk <- (n - 1) * mean((jk - mean_jk)^2)

#mean_jk
var_jk

###############################################################


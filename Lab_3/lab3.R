
population<-read.csv("population.csv",sep=";",encoding = "latin1")
population$Municipality<-as.character(population$Municipality)

population$prop<-population$Population/sum(population$Population)

#############################################################
random<-function(n){
  vec<-rep(0,n)
  
  m<-2**30
  a <- 1103515245
  c <- 123456
  
  d <- as.numeric(Sys.time()) * 1000
  
  for (i in 1:n){
    d<-(a*d+c)%%m
    vec[i]<-d/m
  }
  return(vec)
  
}


sample_func<-function(n,data){
    uniform<-runif(n)
    t2= sapply(uniform,function (x){ sum(x<=cumsum(data$prop))})
    n=length(data$prop)
    indx=n-t2
  
  
  
  return(list("index"=indx,"sample_city"=data[indx,]))
 
}




#using dpyr and setdiff

set.seed(123456)
i<-1
pop1=population[order(population$prop),]

repeat{
  mysample=sample_func(1,pop1)
  pop1=pop1[-mysample$index,]
  if (i==20){
    break
  }
  i=i+1
}


l=dplyr::setdiff(population,pop1)

############################################################

set.seed(123456)


i<-1
pop1=population[order(population$prop),]
samp=data.frame()
repeat{
  mysample=sample_func(1,pop1)
  pop1=pop1[-mysample$index,]
  if (i==1){
  
  samp=mysample$sample_city
}
else if(i!=20){
  samp=rbind(samp,mysample$sample_city)
}
  else{
    break
  }
  i=i+1
}


par(mfrow=c(1,2))
hist(samp$Population,main = "Sample data",xlab = "Poulation")
hist(population$Population,main="Original data",xlab = "Population")


##############################################################



set.seed(123456)

rlaplace = function(n,mu,sigma){
  U = random(n)
  sign = ifelse(U-0.5>0,1,-1)     
  y = mu + sign*sigma*log(1-2*abs(U-0.5))  
  return(y)
}

rlaplace1<-function(n){
  U=random(n)
  sign=ifelse(U>0.5,1,-1)
  y=-1*sign*log(1-2*abs(U-0.5))
  
  return(list("U"=U,"y"=y))
}
  
plot(density(rlaplace1(10000)$y))

plot(density(rlaplace(10000,0,1)))


#hist(rmutil::rlaplace(10000, 0, 1))


##############################################################


f<-function(x){
  exp(-x^2/2)/sqrt(2*pi)
  
}

g<-function(x){
  exp(-x)/2
  
}


norm_gen<-function(n,c){
  
  fvec<-rep(0,n)
  
  for (i in 1:length(fvec)){
    repeat{
    y<- -log(2-2*runif(1))
    u<-runif(1)
    #if (u<= (exp(y-y^2/2)*sqrt(8/pi))/c )
    if (u<=f(y)/(c*g(y))){
      break
      }
    }
    if (runif(1)<0.5){
      Z=y
    }
    else{Z=-y}
  
  fvec[i]<-Z
  }
  
  return(fvec)
}

hist(norm_gen(2000,f(1)/g(1)))

f(1)/g(1)
#sqrt(8*exp(1)/pi)








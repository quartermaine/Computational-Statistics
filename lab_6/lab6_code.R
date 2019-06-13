
library(animation)

func<-function(x){
  
  result1<-x^2/exp(x)
  result2<-2*exp( -(9*sin(x)) / (x^2+x+1) )
  
  return(result1-result2)
  
}


crossover<-function(x,y){
  kid<-(x+y)/2
  return(kid)
}


mutate<-function(x){
  result<-x^2 %% 30
  
  return(result)
  
}


set.seed(1234567)
funcX<-function(maxiter,mutprob,animated=F){
 
  plotf<-function(){
    plot(func(seq(0,30)),type = "l",col="blue")
    points(initial_pop,func(initial_pop),col="red")
    return()
  }
  
  
  #
  initial_pop=seq(0,30,5)
  #
  Values<-sapply(initial_pop,func)
  #
  max_Value<-rep(0,maxiter)
  
  if (animated==F){
    for (i in 1:maxiter){
      
      parents<-initial_pop[c(sample(1:length(initial_pop),2,replace = F))]
      victim<-order(Values)[1]
      new_kid<-crossover(parents[1],parents[2])
      
      if (runif(1)>mutprob){
        new_kid<-mutate(new_kid)
      }
      initial_pop[victim]<-new_kid
      Values<-sapply(initial_pop,func)
      max_Value[i]<-max(Values)
      
    
    
    }
    plotf()
    return(max(Values))
    
  }
  
  else{
    require(animation)
    saveGIF({
  ani.options(interval=.3)
  col.range <- heat.colors(15)
  
  for (i in 1:maxiter){
    
    parents<-initial_pop[c(sample(1:length(initial_pop),2,replace = F))]
    victim<-order(Values)[1]
    new_kid<-crossover(parents[1],parents[2])
  
    if (runif(1)>mutprob){
      new_kid<-mutate(new_kid)
    }
    
    initial_pop[victim]<-new_kid
    Values<-sapply(initial_pop,func)
    max_Value[i]<-max(Values)
    
    plot(func(seq(0,30)),type = "l",col="blue",main=paste0("plot for",i,"th iteration"))
    points(initial_pop,func(initial_pop),col="red")
    
    }#end of for loop
    
    })#end of animation
  }
  
  
}


funcX(100,0.9,animated=F)


##########################################################################################















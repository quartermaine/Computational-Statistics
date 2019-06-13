


#Question 1

x1<-1/3 ; x2<-1/4
if(x1-x2==1/12){
  print("Subtraction is correct")
}else{ 
  print("Subtraction is wrong")
}



x1<-1 ; x2<-1/2
if(x1-x2==1/2){
  print ("Subtraction is correct")
}else{
  print ("Subraction is wrong")
}

#the problem is that the float numbers can't be represented exactly in the binary so if we print(x1-x2,digits=16) and print(1/12,digits=16) we will see that 
# the resulting floats are not the same which leads to fail the if statement in first question.

#we can use all_equals instead in the first case instead of ""==" to compare the numbers and we will see that the if statement will be executed.


##Question 2  

derivative <-function(f,epsilon){
  d<-((f+epsilon)-f)/epsilon
  return(d)
}

derivative(1,10^-15)
derivative(100000,10^-15)
derivative(1000000,10^-15)
derivative(1000000000,10^-15)

#we can see that as the number of x increases r doesn't take into account the decimals after a specific number of x and rounds the number to the nearest integer 
#and the numeretor of the derivative formula becomes 0 .This is happening becausw of the limitation of bits that r can store in an object.

#the true value of the derivative  for the f(x)=x is 1.



##Question 3
set.seed(12345)
myvar<-function(vec){
  
  n<-length(vec)
  variance<-(sum(vec^2)-(sum(vec)^2)/n)/(n-1)
  return(variance)
  
}

myvec<-rnorm(10000,10^8,1)


y<-double(10000)

for (i in 1:length(myvec)){
  x<-myvec[1:i]
  y[i]<-myvar(x)-var(x,na.rm = T)
}

plot(seq(1:10000),y1,col=ifelse(y1>1.2e-16 | y1< -1.2e-16, "red","black"),
     main=paste0("Yi"))



myvar1<-function(v){
  n<-length(v)
  variance<-sum((v-mean(v))^2)/(n-1)
  return(variance)
}


y1<-double(10000)

for (i in 1:length(myvec)){
  x1<-myvec[1:i]
  y1[i]<-myvar1(x1)-var(x1)
}

plot(seq(1:10000),y1)



#



##Question 4

tecator<-readxl::read_excel("tecator.xls")

tecator<-as.data.frame(tecator)


X<-tecator[,!names(tecator)%in%c("Sample","Protein")]
X$intercept<-1
X<-as.matrix(X)
y<-tecator$Protein

A<-t(X)%*%X
b<-t(X)%*%y

solve(A,b)



#we can see that we can't get an answer using solve and we get a very big value of kappa 
#this result can be derived due to the fact that a lot of columns are highly correlated together so during the calculations we gona end 
#with less number of equatios than the number of unknown parameters.

kappa(A)

#printing the number of kappa for the value of A matrix we see that is very big and that implies that the matrix is said to be ill-conditioned small a very small
#change in matrix A will cause a large error in b and makes the solution unstable.

#with scaled data
X1<-as.data.frame(scale(tecator[,!names(tecator)%in%c("Sample","Protein")]))
X1$intercept<-1
X1<-as.matrix(X1)

y1<-tecator$Protein


A1<-t(X1)%*%X1
b1<-t(X1)%*%y1

solve(A1,b1)





#using the scaled data we where able to find coefficients for all the features in X matrix because when we use scaling we 
#


kappa(A1)

#printing the number of kappa again we can see that is still high but much less that the previous used with tha unscaled data and we where able to solve the
#linear system


#S0 denotes initial stock price
#K denotes stock prices
#r denotes continuously compounded interest rate
#sigma denotes the volatility
#Tn denotes the time to maturity or the number of perio we simulate the stock price
#current stock price S rise to Su or falls to Sd at the end of the next period
#u denotes upward move of the stock
#d denotes the downward move of the stock
#lambda is an additional parameter for binomial models
#when lambda.n=0 we have the CRR model
#when lambda.n=r/delta^2 we get the walsh model
#when lambda.n= (r/delta^2)-(1/2) we get the Jarrow and Rudd model 
#t denotes the time

# create a function for risk neutral probability q

qprob<- function(r,delta,sigma){
  u<- exp(sigma*sqrt(delta))
  d<- exp(-sigma*sqrt(delta))
  q<- (exp(r*delta)-d)/(u-d)
  return(q)
}
qprob(r=0.025,delta = 1,sigma = log(1.0488))

#Build a function to simulate the price of stock the stock price in Tn period

build.stock.sim<- function(S,sigma,delta,Tn){
  sim.bin.tree<- matrix(0,nrow = Tn+1,ncol = Tn+1)
  u<- exp(sigma*sqrt(delta))
  d<- exp(-sigma*sqrt(delta))
  for(i in 1:(Tn+1)){
    for(j in 1:i){
      sim.bin.tree[i,j]<- S*u^(j-1)*d^((i-1)-(j-1))
    }
  }
  return(sim.bin.tree)
}

build.stock.sim(100,log(1.0488),1,2)
build.stock.sim(S=80,sigma = 0.1,delta = 1/2,Tn=2)
build.stock.sim(S=80,sigma = 0.1,delta = 1/4,Tn=4)

# combine the two for a binomial function to value options in each leaf node of the tree
bin_option_function<- function(tree,sigma,delta,r,K,type){
  q<- qprob(r,delta,sigma)
  simul.tree<- matrix(0,nrow = nrow(tree),ncol = ncol(tree))
  if(type=='put'){
    simul.tree[nrow(simul.tree),]=pmax(K-tree[nrow(tree),],0)
  }else{
    simul.tree[nrow(simul.tree),]=pmax(tree[nrow(tree),] - K,0)
  }
  for(i in (nrow(tree)-1):1){
    for (j in 1:i) {
      simul.tree[i,j]<- ((1-q)*simul.tree[i+1,j] + q*simul.tree[i+1,j+1])/exp(r*delta)
      
    }
  }
  return(simul.tree)
}
tree<- build.stock.sim(S=100,sigma =0.2 ,delta =1,Tn=.5)

bin_option_function(tree = tree,sigma = 0.2,r=0.06,K=95,type = "call",delta = 1)
#join the functions above and create a function  that returns a tree with the value of the option at different time period.

binomial_tree_option<- function(type,sigma,Tn,r,K,S,lambda.n=0,t){
  q<- qprob(r=r,delta = (t/Tn),sigma = sigma)
  tree<- build.stock.sim(S=S,sigma = sigma,delta = (t/Tn),Tn=Tn)
  stocks.option<- bin_option_function(type = type,tree,sigma = sigma,delta=(t/Tn),r=r,K=K)
  return(list(q=q,stocks=tree,option=stocks.option,price=stocks.option[1,1]))
}
binomial_tree_option(S=100,K=95,t=1,type="call",Tn=0.5,sigma=0.2,r=0.006)


binomial_tree_option(type='call', sigma=0.15, t=1, r=0.1, K=100, S=110, Tn=5)

strike<- seq(1,5)

binomial.strike.function<- function(strike){
  option<-binomial_tree_option(type = "call",sigma = 0.2,Tn=6400,K=strike,S=100,r=0.06,t=0.5)
  return(option$price)
}
binomial.strike.function(strike)
#use sapply to pass each value of the strike array into the binomial strike function

values<- sapply(strike,binomial.strike.function)

#create a data frame for the strikes and option prices
dat.option<- as.data.frame(Strikes=strike,Values=values) 

#use parallel package to fasten the implementation
library(parallel)

cl<- makeCluster(8)
clusterEvalQ(cl,source("binomial model.R"))
no_steps<- seq(10,1000)
no_steps<- sample(no_steps)

valuesPar<-parSapply(cl=cl,no_steps,binomial.strike.function)

dataoption<- as.data.frame(list(No_of_steps=no_steps,Values=values))

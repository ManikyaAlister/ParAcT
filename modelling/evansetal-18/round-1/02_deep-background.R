
log.dens.prior=function(x,hyper){
  out=0
  for (p in names(x)) out =
      out+dtnorm(x[p],hyper[[p]][1],hyper[[p]][2],lower.bounds[p],upper.bounds[p],log=TRUE)
  out
}

crossover=function(i,pars,use.theta,use.like,data,hyper,par.names){
  use.weight=use.like[i] + log.dens.prior(use.theta[i,],hyper)
  gamma = 2.38/sqrt(2*length(pars))
  index=sample(c(1:n.chains)[-i],2,replace=F)
  theta=use.theta[i,]      			
  theta[pars]=use.theta[i,pars] + gamma*(use.theta[index[1],pars]-use.theta[index[2],pars]) + runif(1,-mutation,mutation)
  prior.like=log.dens.prior(theta,hyper)
  if (prior.like==-Inf) {
    like=-Inf
  } else {
    like=log.dens.like(theta,data,par.names=par.names)
  }
  weight=like + prior.like
  if(is.na(weight))weight=-Inf
  if(runif(1) < exp(weight-use.weight)) {							
    use.theta[i,]=theta
    use.like[i]=like
  }
  c(use.like[i],use.theta[i,])
}

migration.crossover=function(pars,use.theta,use.like,data,hyper,par.names){
  n.migration.chains=ceiling(runif(1,0,n.chains))
  use.chains=sample(1:n.chains,n.migration.chains)
  migration.use.weight=rep(NA,n.migration.chains)
  migration.weight=rep(NA,n.migration.chains)
  for (mi in 1:n.migration.chains) {
    migration.use.weight[mi]=use.like[use.chains[mi]] + log.dens.prior(use.theta[use.chains[mi],],hyper)
    newChain = mi - 1
    if (newChain == 0) newChain = n.migration.chains
    migration.weight[mi]=use.like[use.chains[newChain]] + log.dens.prior(use.theta[use.chains[newChain],],hyper)
    if(runif(1) < exp(migration.weight[mi]-migration.use.weight[mi])) {        			
      use.theta[use.chains[mi],]=use.theta[use.chains[newChain],]
      use.like[use.chains[mi]]=use.like[use.chains[newChain]]
    }
  }
  cbind(use.like,use.theta)
}



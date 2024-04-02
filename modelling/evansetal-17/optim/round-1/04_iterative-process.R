
iterative_process = function(theta.names = param.names){
  # source priors
  source(here(paste0(
    "modelling/evansetal-17/optim/round-1/03_priors.R"
  )))
  
  for(i in 1:n.chains){
    while (weight[1,i]==-Inf) {
      theta[i,,1]=rtnorm(n=n.pars,mean=start.points,sd=start.points.sd,lower.bounds,upper.bounds)
      weight[1,i]=log.dens.like(theta[i,,1],data=data,par.names=theta.names)
    }
  }
  
  save.image(savefile)
  
  
  for(i in 2:nmc){
    if (i %% 60 == 0) cat(" ",round((i/nmc)*100,0),"%")
    if (i %% migration.freq == 0 & i > migration.start & i < migration.end) {
      temp=migration.crossover(pars=1:n.pars,use.theta=theta[,,i-1],use.like=weight[i-1,],data=data,hyper=prior,par.names=theta.names)
    } else {
      temp=t(sapply(1:n.chains,crossover,pars=1:n.pars,use.theta=theta[,,i-1],use.like=weight[i-1,],data=data,hyper=prior,par.names=theta.names))
    }
    weight[i,]=temp[,1]
    theta[,,i]=temp[,2:(n.pars+1)]
  }
  
  save.image(savefile)
  
  
  theta=theta[,,burnin:nmc]
  weight=weight[burnin:nmc,]
}












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


# u=(burnin+1):nmc
# 
# prior.means=prior.sds=NULL
# 
# for (p in theta.names) {
#   prior.means=c(prior.means,prior[[p]][1])
#   prior.sds=c(prior.sds,prior[[p]][2])
# }
# 
# names(prior.means)=names(prior.sds)=theta.names
# 
# N2=length(u)*n.chains
# proposal.means=apply(theta[,,u],2,mean)
# proposal.sds=apply(theta[,,u],2,sd)
# log.proposalLikelihoods=array(NA,c(N2))
# log.proposalDensStore=array(NA,c(N2))
# N1=length(u)*n.chains
# log.proposalDensStorePost=array(NA,c(N1))
# log.posteriorLikelihoods=NULL
# posteriorParamsStore=NULL
# for (i in u) {
#   log.posteriorLikelihoods=c(log.posteriorLikelihoods,as.vector(weight[i,]))
#   posteriorParamsStore=rbind(posteriorParamsStore,theta[,,i])
# }
# colnames(posteriorParamsStore)=theta.names
# 
# for (i in 1:N2) {
#   use.params=rtnorm(n.pars,proposal.means,proposal.sds,lower.bounds,upper.bounds)
#   log.proposalDensStore[i]=sum(dtnorm(use.params,proposal.means,proposal.sds,lower.bounds,upper.bounds,log=TRUE))
#   log.proposalLikelihoods[i]=log.dens.like(x=use.params,data=data,par.names=theta.names)+sum(dtnorm(use.params,prior.means,prior.sds,lower.bounds,upper.bounds,log=TRUE))
# }
# for (i in 1:N1) {
#   log.proposalDensStorePost[i]=sum(dtnorm(posteriorParamsStore[i,],proposal.means,proposal.sds,lower.bounds,upper.bounds,log=TRUE))
#   log.posteriorLikelihoods[i]=log.posteriorLikelihoods[i]+sum(dtnorm(posteriorParamsStore[i,],prior.means,prior.sds,lower.bounds,upper.bounds,log=TRUE))
# }
# 
# S1=N1/(N1+N2)
# S2=N2/(N1+N2)
# 
# 
# tolerance = 0.01
# bridge.iterations=100000
# 
# lml=rep(NA,bridge.iterations)
# 
# lml[1]=0
# 
# numTop=log.proposalLikelihoods
# numBot=log.proposalDensStorePost
# 
# for (i in 2:bridge.iterations) {
#   
#   tmp1=S1+log.proposalLikelihoods
#   maxLogDens=max(tmp1)
#   minLogDens=min(tmp1)
#   if (maxLogDens > 700) {
#     densTransform1=maxLogDens-700
#     tmp1=tmp1-densTransform1
#   } else if (maxLogDens < -710) {
#     densTransform1=maxLogDens-700
#     tmp1=tmp1-densTransform1
#   } else {
#     densTransform1=0
#   }
#   
#   tmp2=S2+lml[i-1]+log.proposalDensStore
#   maxLogDens=max(tmp2)
#   minLogDens=min(tmp2)
#   if (maxLogDens > 700) {
#     densTransform2=maxLogDens-700
#     tmp2=tmp2-densTransform2
#   } else if (maxLogDens < -710) {
#     densTransform2=maxLogDens-700
#     tmp2=tmp2-densTransform2
#   } else {
#     densTransform2=0
#   }
#   
#   denTop=log(exp(tmp1) + exp(tmp2))+densTransform1+densTransform2
#   
#   
#   
#   tmp3=S1+log.posteriorLikelihoods
#   maxLogDens=max(tmp3)
#   minLogDens=min(tmp3)
#   if (maxLogDens > 700) {
#     densTransform3=maxLogDens-700
#     tmp3=tmp3-densTransform3
#   } else if (maxLogDens < -710) {
#     densTransform3=maxLogDens-700
#     tmp3=tmp3-densTransform3
#   } else {
#     densTransform3=0
#   }
#   
#   tmp4=S2+lml[i-1]+log.proposalDensStorePost
#   maxLogDens=max(tmp4)
#   minLogDens=min(tmp4)
#   if (maxLogDens > 700) {
#     densTransform4=maxLogDens-700
#     tmp4=tmp4-densTransform4
#   } else if (maxLogDens < -610) {
#     densTransform4=maxLogDens-700
#     tmp4=tmp4-densTransform4
#   } else {
#     densTransform4=0
#   }
#   
#   demBot=log(exp(tmp3) + exp(tmp4))+densTransform3+densTransform4
#   
#   
#   
#   tmp5=numTop-denTop
#   maxLogDens=max(tmp5)
#   minLogDens=min(tmp5)
#   if (maxLogDens > 700) {
#     densTransform5=maxLogDens-700
#     tmp5=tmp5-densTransform5
#   } else if (maxLogDens < -610) {
#     densTransform5=maxLogDens-700
#     tmp5=tmp5-densTransform5
#   } else {
#     densTransform5=0
#   }
#   
#   tmp6=numBot-demBot
#   maxLogDens=max(tmp6)
#   minLogDens=min(tmp6)
#   if (maxLogDens > 700) {
#     densTransform6=maxLogDens-700
#     tmp6=tmp6-densTransform6
#   } else if (maxLogDens < -710) {
#     densTransform6=maxLogDens-700
#     tmp6=tmp6-densTransform6
#   } else {
#     densTransform6=0
#   }
#   
#   lml[i]=log(mean(exp(tmp5))) - log(mean(exp(tmp6)))+densTransform5-densTransform6
#   
#   if (abs(lml[i]-lml[i-1]) < tolerance) break
#   if (lml[i] < -710) {
#     lml[i]=-Inf
#     break
#   }
# }
# 
# logMarginalLikelihood=lml[i]


theta=theta[,,burnin:nmc]
weight=weight[burnin:nmc,]










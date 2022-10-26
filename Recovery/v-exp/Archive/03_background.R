
n.pars=length(theta.names)
n.chains=n.pars*3

nmc=3000
burnin=1000
mutation=.001
migration.freq=10
migration.start=300
migration.end=800

theta=array(NA,c(n.chains,n.pars,nmc))
weight=array(-Inf,c(nmc,n.chains))





tmpP1=grep("a",theta.names,perl=TRUE)
tmpP2=grep("t0",theta.names,perl=TRUE)
tmpP3=grep("v",theta.names,perl=TRUE)
tmpP4=grep("z",theta.names,perl=TRUE)


start.points=rep(NA,n.pars)
start.points[tmpP1]=1
start.points[tmpP2]=0.3
start.points[tmpP3]=3
start.points[tmpP4]=0.5

start.points.sd=rep(NA,n.pars)
start.points.sd[tmpP1]=0.5
start.points.sd[tmpP2]=0.1
start.points.sd[tmpP3]=1
start.points.sd[tmpP4]=0.15

lower.bounds=rep(NA,n.pars)
lower.bounds[tmpP1]=0
lower.bounds[tmpP2]=0
lower.bounds[tmpP3]=-Inf
lower.bounds[tmpP4]=0

upper.bounds=rep(NA,n.pars)
upper.bounds[tmpP1]=Inf
upper.bounds[tmpP2]=Inf
upper.bounds[tmpP3]=Inf
upper.bounds[tmpP4]=1





colnames(theta) = 
  names(start.points) = 
  names(start.points.sd) = 
  names(lower.bounds) = 
  names(upper.bounds) = 
  theta.names





prior=list()

tmp=grep("a",theta.names,value=TRUE)
for (n in 1:length(tmp)) {
  tmp2=tmp[n]
  prior[[tmp2]]=c(2,1)
}

tmp=grep("v",theta.names,value=TRUE)
for (n in 1:length(tmp)) {
  tmp2=tmp[n]
  prior[[tmp2]]=c(3,1)
}

tmp=grep("t0",theta.names,value=TRUE)
for (n in 1:length(tmp)) {
  tmp2=tmp[n]
  prior[[tmp2]]=c(0.3,0.1)
}

tmp=grep("z",theta.names,value=TRUE)
for (n in 1:length(tmp)) {
  tmp2=tmp[n]
  prior[[tmp2]]=c(0.5,0.1)
}



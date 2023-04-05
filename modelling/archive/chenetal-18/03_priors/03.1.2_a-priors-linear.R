
n.pars=length(theta.names)
n.chains=n.pars*3

nmc=4000
burnin=2000
mutation=.001
migration.freq=25
migration.start=700
migration.end=1700

theta=array(NA,c(n.chains,n.pars,nmc))
weight=array(-Inf,c(nmc,n.chains))


tmpP1.2 = grep("a.b",theta.names,perl=TRUE)
tmpP1.3 = grep("a.c",theta.names,perl=TRUE)
tmpP2=grep("t0",theta.names,perl=TRUE)
tmpP3=grep("v",theta.names,perl=TRUE)
tmpP4=grep("z",theta.names,perl=TRUE)


start.points=rep(NA,n.pars)
start.points[tmpP1.2]=1
start.points[tmpP1.3]=.2
start.points[tmpP2]=0.3
start.points[tmpP3]=3
start.points[tmpP4]=0.5

start.points.sd=rep(NA,n.pars)
start.points.sd[tmpP1.2]=0.5
start.points.sd[tmpP1.3]=0.5
start.points.sd[tmpP2]=0.1
start.points.sd[tmpP3]=1
start.points.sd[tmpP4]=0.15

lower.bounds=rep(NA,n.pars)
lower.bounds[tmpP1.2]=0
lower.bounds[tmpP1.3]=0
lower.bounds[tmpP2]=0
lower.bounds[tmpP3]=-Inf
lower.bounds[tmpP4]=0

upper.bounds=rep(NA,n.pars)
upper.bounds[tmpP1.2]=Inf
upper.bounds[tmpP1.3]=Inf
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

tmp=grep("a.b",theta.names,value=TRUE)
for (n in 1:length(tmp)) {
  tmp2=tmp[n]
  prior[[tmp2]]=c(2,1)
}

tmp=grep("a.c",theta.names,value=TRUE)
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
if (length(tmp)>0) {
  for (n in 1:length(tmp)) {
    tmp2=tmp[n]
    prior[[tmp2]]=c(0.5,0.1)
  }
}





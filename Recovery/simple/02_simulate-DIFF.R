
dyn.load("Recovery/DIFF.so")

use.interval=0.0001
use.table=qnorm(seq(use.interval,1-use.interval,use.interval))
n.table.options=length(use.table)


simulate.DIFF=function(N,params,maxCounter,stepSize,use.table,n.table.options) {
  
  rts=rep(0,N)
  resps=rep(0,N)
  
  z=(params["z"]*(params["a"]+params["a"]))-params["a"]
  
  tmp=.C("DIFF",z=z,v=params["v"],
         aU=params["a"],aL=-params["a"],ter=params["ter"],
         sv=params["sv"],sz=params["sz"],ster=params["ster"],
         s=params["stoch.s"],h=stepSize,resp=resps,rt=rts,
         n=N,maxiter=as.double(round(maxCounter)),
         rangeLow=as.integer(0),rangeHigh=as.integer(n.table.options-1),
         randomTable=as.double(use.table)) 
  
  out=list(rt=tmp$rt,resp=tmp$resp)
}


# N=100000
# maxCounter=20000
# stepSize=1
# #params=c(v=0,a=10,ter=0.3,sv=0,ster=0,sz=0,stoch.s=0.1,z=10/2,gamma=0,alpha=0,tau=0)
# params=c(v=0.45,a=60,ter=330,sv=0,ster=80,sz=20,stoch.s=4,z=0,gamma=-15,alpha=2.5,tau=30)
# 
# blah=proc.time()
# tmp=simulate.DMC(N=N,params=params,maxCounter=maxCounter,stepSize=stepSize,use.table=use.table,n.table.options=n.table.options)
# print(proc.time()-blah)



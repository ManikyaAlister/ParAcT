rm(list = ls())
lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
dataset = "knowlesetal-19"


# Load observed data 
nSub = 7
all.data=list() 
for (useSub in 1:nSub) {
  
  load(here(paste0("data/",dataset,"/clean/P",useSub,".Rdata")))
  
  all.data[[useSub]]=data
}


# Function for calculating the quantiles of an RT distribution
quantiles = function(all.data, qs=seq(0.1,0.9,0.1),conds = 1) {

tmp=lapply(all.data,function(x) tapply(x$Time,list(x$Resp,x$Cond),quantile,qs)) 

for (s in 1:nSub) {
  if (nrow(tmp[[s]])==1) {
    tmp[[s]]=rbind(list(rep(NA,length(qs)),rep(NA,length(qs))),tmp[[s]])
    rownames(tmp[[s]])=c(1,2)
  }
   for (i in 1:2) {
     for (j in conds) {
       if (is.null(tmp[[s]][paste(i),paste(j)][[1]])) tmp[[s]][paste(i),paste(j)][[1]]=rep(NA,length(qs))
     }
   }
}

allQ=array(unlist(tmp),c(length(qs),2,2,nSub))

tmp=lapply(all.data,function(x) tapply(x$Resp==2,x$Cond,mean))

allP=array(unlist(tmp),c(2,nSub))

#Means for plotting
means = list(
q.mean=apply(allQ[,2,,],1,mean),
p.mean=mean(allP)
)

return(means)


}

# Observed data 
observed = quantiles(all.data)

# Simulate data

# load simulated data
models = c("simple","a-linear","a-power","a-exp-mir","a-delayed-power","a-delayed-exp","v-linear","v-power","v-exp","v-delayed-power","v-delayed-exp","v-a-exp")

allSimQuantiles = list()

for (model in models){
  all.data = list()
  for (useSub in 1:nSub) {
    load(here(paste0("modelling/",dataset,"/round-1/08_model-predictions/P",useSub,"_",model,".Rdata")))
    all.data[[useSub]]=sim
  }
  quant = quantiles(all.data)
  allSimQuantiles[[model]] = quant
  allSimQuantiles[[model]]$name = model
}

# function to plot observed data against predicted data
plotQuantiles = function(simQuants, qs = seq(0.1,0.9,0.1)) { # need to have the observed quantiles means loaded in environment (q.means & p.mean)
  plot(observed$q.mean, qs*observed$p.mean, pch = 16, main = simQuants$name) # observed data 
  lines(observed$q.mean, qs*observed$p.mean)
  points(simQuants$q.mean, qs*simQuants$p.mean, pch = 1) # simulated data 
  lines(simQuants$q.mean, qs*simQuants$p.mean)
}






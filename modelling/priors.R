
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

# simple model
tmpP1=grep("a",theta.names,perl=TRUE)
tmpP2=grep("t0",theta.names,perl=TRUE)
tmpP3=grep("v",theta.names,perl=TRUE)
tmpP4=grep("z",theta.names,perl=TRUE)

# across trial variability in the full ddm
tmpP3.1=grep("sv",theta.names,perl=TRUE)
tmpP4.1=grep("sz",theta.names,perl=TRUE)



# linear model
tmpP1.2 = grep("a.b",theta.names,perl=TRUE)
tmpP1.3 = grep("a.c",theta.names,perl=TRUE)
tmpP3.2 = grep("v.c",theta.names,perl=TRUE)
tmpP3.3 = grep("v.b",theta.names,perl=TRUE)

# power + exponential models
tmpP1.4= grep("a.asym",theta.names,perl=TRUE)
tmpP1.5 = grep("a.start",theta.names,perl=TRUE)
tmpP1.6 = grep("a.rate",theta.names,perl=TRUE)
tmpP3.4= grep("v.asym",theta.names,perl=TRUE)
tmpP3.5 = grep("v.start",theta.names,perl=TRUE)
tmpP3.6 = grep("v.rate",theta.names,perl=TRUE)

# transition/delay models
tmpP1.7 = grep("a.delay",theta.names,perl=TRUE)
tmpP3.7 = grep("v.delay",theta.names,perl=TRUE)

# block models 
tmpP5=grep("b.bump",theta.names,perl=TRUE)
tmpP6=grep("step",theta.names,perl=TRUE)
tmpP7=grep("trialUnlearn",theta.names,perl=TRUE)

# step models
tmpP8.1= grep("when",theta.names,perl=TRUE)
tmpP8.2= grep("initial",theta.names,perl=TRUE)



if (exists("blocks")) {
  # for each block, create a new variable tmpP1.8, tmpP1,9, etc based on the value of the block for v and a
  names.a = NULL
  names.v = NULL
  for (block in blocks) {
    name.a = paste0("tmpP1.", 7+block)
    name.v = paste0("tmpP3.", 7+block)
    names.a[block] = name.a
    names.v[block] = name.v
    assign(name.a, grep(paste0("a.", block), theta.names, perl=TRUE))
    assign(name.v, grep(paste0("v.", block), theta.names, perl=TRUE))
  }

}



# Start points (of MCMC) ------------------------------------------------------------
# simple model 
start.points=rep(NA,n.pars)
start.points[tmpP1]=1
start.points[tmpP2]=0.3
start.points[tmpP3]=3
start.points[tmpP4]=0.5

# full ddm 
start.points[tmpP3.1]=2
start.points[tmpP3.2]=0.3


# linear model
start.points[tmpP1.2]=1
start.points[tmpP1.3]=.2
start.points[tmpP3.2]=3
start.points[tmpP3.3]=.5


# power + exponential models
start.points[tmpP1.4]=1
start.points[tmpP1.5]=1
start.points[tmpP1.6]=.4
start.points[tmpP3.4]=3
start.points[tmpP3.5]=3
start.points[tmpP3.6]=.5

# transition/delay models
start.points[tmpP1.7]=20
start.points[tmpP3.7]=20

# block models 
start.points[tmpP5]=1
start.points[tmpP6]=0.05
start.points[tmpP7]=1

# step models 
start.points[tmpP8.1]=30
start.points[tmpP8.2]=0.05

# for block in blocks, create a new starting point corresponding to the v and a block variables (tmpP1.8, tmpP1.9, etc)
if (exists("blocks")){
for(block in blocks){
  start.points[names.a[block]]=1
  start.points[names.v[block]]=3
}
}

# Start point sd ----------------------------------------------------------

start.points.sd=rep(NA,n.pars)
start.points.sd[tmpP1]=0.5
start.points.sd[tmpP2]=0.1
start.points.sd[tmpP3]=1
start.points.sd[tmpP4]=0.15

# full ddm
start.points.sd[tmpP3.1]=0.5
start.points.sd[tmpP4.1]=0.15


# linear model
start.points[tmpP1.2]=1
start.points[tmpP1.3]=.2
start.points.sd[tmpP3.2]=1
start.points.sd[tmpP3.3]=1

# power + exponential models
start.points.sd[tmpP1.4]=0.5
start.points.sd[tmpP1.5]=0.5
start.points.sd[tmpP1.6]=0.5
start.points.sd[tmpP3.4]=1
start.points.sd[tmpP3.5]=1
start.points.sd[tmpP3.6]=1

# transition/delay models
start.points.sd[tmpP1.7]=10
start.points.sd[tmpP3.7]=10

# block models 
start.points.sd[tmpP5]=0.5
start.points.sd[tmpP6]=0.5
start.points.sd[tmpP7]=0.5

# step models
start.points.sd[tmpP8.1]=3
start.points.sd[tmpP8.2]=0.5

# for block in blocks, create a new starting point.sd corresponding to the v and a block variables (tmpP1.8, tmpP1.9, etc)
if (exists("blocks")){
for(block in blocks){
  start.points.sd[names.a[block]]=0.5
  start.points.sd[names.v[block]]=1
}
}

# Lower bounds ------------------------------------------------------------

# simple model
lower.bounds=rep(NA,n.pars)
lower.bounds[tmpP1]=0
lower.bounds[tmpP2]=0
lower.bounds[tmpP3]=-Inf
lower.bounds[tmpP4]=0

# full ddm
lower.bounds[tmpP3.1]=0
lower.bounds[tmpP4.1]=0

# linear model
lower.bounds[tmpP1.2]=0
lower.bounds[tmpP1.3]=0
lower.bounds[tmpP3.2]=-Inf
lower.bounds[tmpP3.3]=-Inf

# power + exponential models
lower.bounds[tmpP1.4]=0
lower.bounds[tmpP1.5]=0
lower.bounds[tmpP1.6]=0
lower.bounds[tmpP3.4]=0
lower.bounds[tmpP3.5]=0
lower.bounds[tmpP3.6]=0

# transition/delay models
lower.bounds[tmpP1.7]=0
lower.bounds[tmpP3.7]= 0


# block models 
lower.bounds[tmpP5]=0
lower.bounds[tmpP6]=0
lower.bounds[tmpP7]=0

# step models 
lower.bounds[tmpP8.1]=0
lower.bounds[tmpP8.2]=0

if (exists("blocks")){
for(block in blocks){
  lower.bounds[names.a[block]]=0
  lower.bounds[names.v[block]]=0
}
}

# Upper bounds ------------------------------------------------------------

# simple model
upper.bounds=rep(NA,n.pars)
upper.bounds[tmpP1]=Inf
upper.bounds[tmpP2]=Inf
upper.bounds[tmpP3]=Inf
upper.bounds[tmpP4]=1

# full ddm
upper.bounds[tmpP3.1]=Inf
upper.bounds[tmpP4.1]=Inf

# linear model
upper.bounds[tmpP1.2]=Inf
upper.bounds[tmpP1.3]=Inf
upper.bounds[tmpP3.2]=Inf
upper.bounds[tmpP3.3]=Inf

# power + exponential models
upper.bounds[tmpP1.4]=Inf
upper.bounds[tmpP1.5]=Inf
upper.bounds[tmpP1.6]=Inf
upper.bounds[tmpP3.4]=Inf
upper.bounds[tmpP3.5]=Inf
upper.bounds[tmpP3.6]=Inf

# transition/delay models
upper.bounds[tmpP1.7]=Inf
upper.bounds[tmpP3.7]=Inf

# block models 
upper.bounds[tmpP5]=Inf
upper.bounds[tmpP6]=Inf
upper.bounds[tmpP7]=Inf

# step models 
upper.bounds[tmpP8.1]=Inf
upper.bounds[tmpP8.2]=Inf

if (exists("blocks")){
for(block in blocks){
  upper.bounds[names.a[block]]=Inf
  upper.bounds[names.v[block]]=Inf
}
}

colnames(theta) = 
  names(start.points) = 
  names(start.points.sd) = 
  names(lower.bounds) = 
  names(upper.bounds) = 
  theta.names



# Define priors -----------------------------------------------------------


prior=list()


# Simple Model

tmp = grep("a", theta.names, value = TRUE)
# only add to prior if parameter is in model
if (length(tmp) > 0) {
  for (n in 1:length(tmp)) {
    tmp2 = tmp[n]
    prior[[tmp2]] = c(2, 1) # this is the prior mean and standard deviation
  }
}


tmp = grep("v", theta.names, value = TRUE)
if (length(tmp) > 0) {
  for (n in 1:length(tmp)) {
    tmp2 = tmp[n]
    prior[[tmp2]] = c(3, 1)
  }
}

tmp = grep("t0", theta.names, value = TRUE)
if (length(tmp) > 0) {
  for (n in 1:length(tmp)) {
    tmp2 = tmp[n]
    prior[[tmp2]] = c(0.3, 0.1)
  }
}

tmp = grep("z", theta.names, value = TRUE)
if (length(tmp) > 0) {
  for (n in 1:length(tmp)) {
    tmp2 = tmp[n]
    prior[[tmp2]] = c(0.5, 0.1)
  }
}

# full ddm
tmp = grep("sv", theta.names, value = TRUE)
if (length(tmp) > 0) {
  for (n in 1:length(tmp)) {
    tmp2 = tmp[n]
    prior[[tmp2]] = c(1.5, 0.1)
  }
}

tmp = grep("sz", theta.names, value = TRUE)
if (length(tmp) > 0) {
  for (n in 1:length(tmp)) {
    tmp2 = tmp[n]
    prior[[tmp2]] = c(0.2, 0.1)
  }
}

# linear model
tmp = grep("a.b", theta.names, value = TRUE)
if (length(tmp) > 0) {
  for (n in 1:length(tmp)) {
    tmp2 = tmp[n]
    prior[[tmp2]] = c(2, 1)
  }
}

tmp = grep("a.c", theta.names, value = TRUE)
if (length(tmp) > 0) {
  for (n in 1:length(tmp)) {
    tmp2 = tmp[n]
    prior[[tmp2]] = c(2, 1)
  }
}

tmp = grep("v.c", theta.names, value = TRUE)
if (length(tmp) > 0) {
  for (n in 1:length(tmp)) {
    tmp2 = tmp[n]
    prior[[tmp2]] = c(2, 2)
  }
}

tmp = grep("v.b", theta.names, value = TRUE)
if (length(tmp) > 0) {
  for (n in 1:length(tmp)) {
    tmp2 = tmp[n]
    prior[[tmp2]] = c(.5, .5)
  }
}

# power + exponential models
tmp = grep("a.asym", theta.names, value = TRUE)
if (length(tmp) > 0) {
  for (n in 1:length(tmp)) {
    tmp2 = tmp[n]
    prior[[tmp2]] = c(2, 1)
  }
}

tmp = grep("a.start", theta.names, value = TRUE)
if (length(tmp) > 0) {
  for (n in 1:length(tmp)) {
    tmp2 = tmp[n]
    prior[[tmp2]] = c(2, 1)
  }
}

tmp = grep("a.rate", theta.names, value = TRUE)
if (length(tmp) > 0) {
  for (n in 1:length(tmp)) {
    tmp2 = tmp[n]
    prior[[tmp2]] = c(2, 1)
  }
}


tmp = grep("v.asym", theta.names, value = TRUE)
if (length(tmp) > 0) {
  for (n in 1:length(tmp)) {
    tmp2 = tmp[n]
    prior[[tmp2]] = c(2, 2)
  }
}

tmp = grep("v.start", theta.names, value = TRUE)
if (length(tmp) > 0) {
  for (n in 1:length(tmp)) {
    tmp2 = tmp[n]
    prior[[tmp2]] = c(2, 2)
  }
}

tmp = grep("v.rate", theta.names, value = TRUE)
if (length(tmp) > 0) {
  for (n in 1:length(tmp)) {
    tmp2 = tmp[n]
    prior[[tmp2]] = c(0.5, 0.5)
  }
}

# transition/delay models
tmp = grep("a.delay", theta.names, value = TRUE)
if (length(tmp) > 0) {
  for (n in 1:length(tmp)) {
    tmp2 = tmp[n]
    prior[[tmp2]] = c(20, 10)
  }
}

tmp = grep("v.delay", theta.names, value = TRUE)
if (length(tmp) > 0) {
  for (n in 1:length(tmp)) {
    tmp2 = tmp[n]
    prior[[tmp2]] = c(20, 10)
  }
}

# block models
tmp = grep("b.bump", theta.names, value = TRUE)
if (length(tmp) > 0) {
  for (n in 1:length(tmp)) {
    tmp2 = tmp[n]
    prior[[tmp2]] = c(2, 2)
  }
}


tmp = grep("step", theta.names, value = TRUE)
if (length(tmp) > 0) {
  for (n in 1:length(tmp)) {
    tmp2 = tmp[n]
    prior[[tmp2]] = c(0.05, 0.5)
  }
}

tmp = grep("trialUnlearn", theta.names, value = TRUE)
if(length(tmp) > 0){
  for (n in 1:length(tmp)) {
    tmp2 = tmp[n]
    prior[[tmp2]] = c(2, 2)
  }
}

tmp = grep("initial", theta.names, value = TRUE)
if (length(tmp) > 0) {
  for (n in 1:length(tmp)) {
    tmp2 = tmp[n]
    prior[[tmp2]] = c(0.05, 0.5)
  }
}

tmp = grep("when", theta.names, value = TRUE)
if (length(tmp) > 0) {
  for (n in 1:length(tmp)) {
    tmp2 = tmp[n]
    prior[[tmp2]] = c(3, 1)
  }
}

if (exists("blocks")){
for (block in blocks){
  tmp = grep(paste0("a.",block), theta.names, value = TRUE)
  if (length(tmp) > 0) {
    for (n in 1:length(tmp)) {
      tmp2 = tmp[n]
      prior[[tmp2]] = c(2, 1)
    }
  }
  # now the same for v
  tmp = grep(paste0("v.",block), theta.names, value = TRUE)
  if (length(tmp) > 0) {
    for (n in 1:length(tmp)) {
      tmp2 = tmp[n]
      prior[[tmp2]] = c(3, 1)
    }
  }
}
}

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

# Start points ------------------------------------------------------------
# simple model 
start.points=rep(NA,n.pars)
start.points[tmpP1]=1
start.points[tmpP2]=0.3
start.points[tmpP3]=3
start.points[tmpP4]=0.5

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
start.points[tmpP1.7]=.4
start.points[tmpP3.7]=.5

# block models 
start.points[tmpP5]=1

# Start point sd ----------------------------------------------------------

start.points.sd=rep(NA,n.pars)
start.points.sd[tmpP1]=0.5
start.points.sd[tmpP2]=0.1
start.points.sd[tmpP3]=1
start.points.sd[tmpP4]=0.15
start.points.sd[tmpP5]=0.5



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
start.points.sd[tmpP1.7]=0.5
start.points.sd[tmpP3.7]=1

# block models 
start.points.sd[tmpP5]=0.5

# Lower bounds ------------------------------------------------------------

# simple model
lower.bounds=rep(NA,n.pars)
lower.bounds[tmpP1]=0
lower.bounds[tmpP2]=0
lower.bounds[tmpP3]=-Inf
lower.bounds[tmpP4]=0

# linear model
lower.bounds[tmpP1.2]=0
lower.bounds[tmpP1.3]=0
lower.bounds[tmpP3.2]=-Inf
lower.bounds[tmpP3.3]=-Inf

# power + exponential models
lower.bounds[tmpP1.4]=0
lower.bounds[tmpP1.5]=0
lower.bounds[tmpP1.6]=0
lower.bounds[tmpP3.4]=-Inf
lower.bounds[tmpP3.5]=-Inf
lower.bounds[tmpP3.6]=-Inf

# transition/delay models
lower.bounds[tmpP1.7]=0
lower.bounds[tmpP3.7]= 0


# block models 
lower.bounds[tmpP5]=0

# Upper bounds ------------------------------------------------------------

# simple model
upper.bounds=rep(NA,n.pars)
upper.bounds[tmpP1]=Inf
upper.bounds[tmpP2]=Inf
upper.bounds[tmpP3]=Inf
upper.bounds[tmpP4]=1

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
if (length(tmp) > 0) {
  # only add to prior if parameter is in model
  for (n in 1:length(tmp)) {
    tmp2 = tmp[n]
    prior[[tmp2]] = c(2, 1)
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
    prior[[tmp2]] = c(0.5, 0.5)
  }
}

tmp = grep("v.delay", theta.names, value = TRUE)
if (length(tmp) > 0) {
  for (n in 1:length(tmp)) {
    tmp2 = tmp[n]
    prior[[tmp2]] = c(0.5, 0.5)
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

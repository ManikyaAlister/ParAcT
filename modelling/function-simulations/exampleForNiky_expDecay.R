#data = data.frame(Trial = 1:100)
#data$Trial=c(0.1,4,2,0)
#names(x) = c("a.rate","a.asym","a.start","a.delay")
data = rnorm(100,1,0)
x = 1:100

r=0.1
a=4
b=2

z = 1:100

x = c(apply(theta,2,mean),0)
names(x) = c(theta.names,"a.delay")

plot(data$Trial,x["a.asym"]+x["a.start"]*exp(-x["a.rate"]*data$Trial),type="l",main="Classic exponential Decay")

plot(data$Trial,x["a.asym"]+x["a.start"]*exp(x["a.rate"]*data$Trial),type="l",main="Your exponential Increase")

plot(data$Trial,x["a.asym"]-x["a.start"]*exp(-x["a.rate"]*data$Trial),type="l",main="Mirrored exponential Decay")

plot(data$Trial,x["a.asym"]-x["a.start"]*exp(x["a.rate"]*data$Trial),type="l",main="Inverted (?) Mirrored exponential Decay")## <-- what I think threshold would look like 

plot(data,1/(1+(exp(1)^-data)),"l")



t=0

x = c(4,2,0.1,0)
names(x) = c("a.asym","a.start","a.rate","a.delay")

plot(1:length(data$Trial),x["a.rate"]+x["a.start"]*((x["a.delay"]+1)/(x["a.delay"]+exp(x["a.rate"]*data$Trial))),type="l",main="Classic Transition Exponential Decay")

plot(1:length(data$Trial),x["a.rate"]-x["a.start"]*((x["a.delay"]+1)/(x["a.delay"]+exp(x["a.rate"]*data$Trial))),type="l",main="Mirrored Transition Exponential Decay")

plot(1:length(data$Trial),x["a.rate"]+x["a.start"]*((x["a.delay"]+1)/(x["a.delay"]+exp(-x["a.rate"]*data$Trial))),type="l",main="Classic Transition Exponential Increase")

plot(1:length(data$Trial),x["a.rate"]-x["a.start"]*((x["a.delay"]+1)/(x["a.delay"]+exp(-x["a.rate"]*data$Trial))),type="l",main="Mirrored Transition Exponential Increase")




# plot(data$Trial,a+b*((t+1)/(t*+exp(r*z))),type="l",main="Classic Transition exponential Decay")
# 
# plot(data$Trial,x["a.asym"]+x["a.start"]*((x["a.delay"]+1)/(x["a.delay"]+exp(x["a.delay"]*data$Trial))),type="l",main="Classic Transition Exponential Decay")
# 
# plot(data$Trial,x["a.asym"]-x["a.start"]*((x["a.delay"]*+1)/(x["a.delay"]*+exp(x["a.rate"]*data$Trial))),type="l",main="Mirrored Transition exponential Decay")
# 
# plot(data$Trial,x["a.asym"]+x["a.start"]*((x["a.delay"]*+1)/(x["a.delay"]*+exp(-x["a.rate"]*data$Trial))),type="l",main="Classic Transition exponential Increase")
# 
# plot(data$Trial,x["a.asym"]-x["a.start"]*((x["a.delay"]*+1)/(x["a.delay"]*+exp(-x["a.rate"]*data$Trial))),type="l",main="Mirrored Transition exponential Increase")
# 


data = data.frame(Trial = 1:100)
data$Trial=c(0.1,4,2,0)
#names(data$Trial) = c("v.rate","v.asym","v.start","v.delay")

r=0.1
a=4
b=2

z = 1:100
x = c(4,2,0.1,0)
names(x) = c("v.asym","v.start","v.rate","v.delay")

plot(data$Trial,x["v.asym"]+x["v.start"]*exp(-x["v.rate"]*data$Trial),type="l",main="Classic exponential Decay")

plot(data$Trial,x["v.asym"]+x["v.start"]*exp(x["v.rate"]*data$Trial),type="l",main="Your exponential Increase")

plot(data$Trial,x["v.asym"]-x["v.start"]*exp(-x["v.rate"]*data$Trial),type="l",main="Mirrored exponential Decay")

plot(data$Trial,x["v.asym"]-x["v.start"]*exp(x["v.rate"]*data$Trial),type="l",main="Inverted (?) Mirrored exponential Decay")## <-- what I think threshold would look like 


t=0



plot(1:length(data$Trial),x["v.rate"]+x["v.start"]*((x["v.delay"]+1)/(x["v.delay"]+exp(x["v.rate"]*data$Trial))),type="l",main="Classic Transition Exponential Decay")

plot(1:length(data$Trial),x["v.rate"]-x["v.start"]*((x["v.delay"]+1)/(x["v.delay"]+exp(x["v.rate"]*data$Trial))),type="l",main="Mirrored Transition Exponential Decay")

plot(1:length(data$Trial),x["v.rate"]+x["v.start"]*((x["v.delay"]+1)/(x["v.delay"]+exp(-x["v.rate"]*data$Trial))),type="l",main="Classic Transition Exponential Increase")

plot(1:length(data$Trial),x["v.rate"]-x["v.start"]*((x["v.delay"]+1)/(x["v.delay"]+exp(-x["v.rate"]*data$Trial))),type="l",main="Mirrored Transition Exponential Increase")90
subj = 5
load(here(paste0("Recovery/a-delayed-exp/Fits_recovery/P",subj,"_a-delayed-exp.Rdata")))
x = as.vector(genParams[,1])
names(x) = rownames(genParams)
a=x["a.asym"]+(x["a.start"]*((x["a.delay"]+1)/(x["a.delay"]+exp(x["a.rate"]*data$Trial))))
plot(data$Trial, a, "l", main = paste0("Delay: ", round(x["a.delay"], 3), ", Rate: ", round(x["a.rate"],3), " Subj: ", subj))

# plot(data$Trial,a+b*((t+1)/(t*+exp(r*z))),type="l",main="Classic Transition exponential Decay")
# 
# plot(data$Trial,x["v.asym"]+x["v.start"]*((x["v.delay"]+1)/(x["v.delay"]+exp(x["v.delay"]*data$Trial))),type="l",main="Classic Transition Exponential Decay")
# 
# plot(data$Trial,x["v.asym"]-x["v.start"]*((x["v.delay"]*+1)/(x["v.delay"]*+exp(x["v.rate"]*data$Trial))),type="l",main="Mirrored Transition exponential Decay")
# 
# plot(data$Trial,x["v.asym"]+x["v.start"]*((x["v.delay"]*+1)/(x["v.delay"]*+exp(-x["v.rate"]*data$Trial))),type="l",main="Classic Transition exponential Increase")
# 
# plot(data$Trial,x["v.asym"]-x["v.start"]*((x["v.delay"]*+1)/(x["v.delay"]*+exp(-x["v.rate"]*data$Trial))),type="l",main="Mirrored Transition exponential Increase")
# 


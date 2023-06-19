
trial=1:100

r=0.1
a=4
b=2

beta = 0.01
c = 1

# linear 
plot(trial, c+beta*trial, "l", ylab = "v")
plot(trial, c+-beta*trial, "l", ylab = "a")


# Power
plot(trial, a-b*trial^-r, "l", ylab = "v")
plot(trial, a+b*trial^-r, "l", ylab = "a")


# Exp
plot(trial,a+b*exp(-r*trial),type="l",main="Classic Exponential Decay", ylab = "a")

plot(trial,a+b*exp(r*trial),type="l",main="Your Exponential Increase")

plot(trial,a-b*exp(-r*trial),type="l",main="Mirrored Exponential Decay", ylab = "v")

t=1000

plot(trial,a+b*((t+1)/(t+exp(r*trial))),type="l",main="Classic Transition Exponential Decay", ylab = "a")

plot(trial,a-b*((t+1)/(t+exp(r*trial))),type="l",main="Mirrored Transition Exponential Decay", ylab = "v")


plot(trial,a+b*((t+1)/(t+exp(-r*trial))),type="l",main="Classic Transition Exponential Increase")

plot(trial,a-b*((t+1)/(t+exp(-r*trial))),type="l",main="Mirrored Transition Exponential Increase")



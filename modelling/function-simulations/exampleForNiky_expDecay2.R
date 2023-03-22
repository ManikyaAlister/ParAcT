
trial=1:100

r=0.1
a=4
b=2

plot(trial,a+b*exp(-r*trial),type="l",main="Classic Exponential Decay")

plot(trial,a+b*exp(r*trial),type="l",main="Your Exponential Increase")

plot(trial,a-b*exp(-r*trial),type="l",main="Mirrored Exponential Decay")

t=1000

plot(trial,a+b*((t+1)/(t+exp(r*trial))),type="l",main="Classic Transition Exponential Decay")

plot(trial,a-b*((t+1)/(t+exp(r*trial))),type="l",main="Mirrored Transition Exponential Decay")


plot(trial,a+b*((t+1)/(t+exp(-r*trial))),type="l",main="Classic Transition Exponential Increase")

plot(trial,a-b*((t+1)/(t+exp(-r*trial))),type="l",main="Mirrored Transition Exponential Increase")



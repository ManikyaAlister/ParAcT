
x=1:100

r=0.1
a=4
b=2

plot(x,a+b*exp(-r*x),type="l",main="Classic Exponential Decay")

plot(x,a+b*exp(r*x),type="l",main="Your Exponential Increase")

plot(x,a-b*exp(-r*x),type="l",main="Mirrored Exponential Decay")


t=0

plot(x,a+b*((t+1)/(t+exp(r*x))),type="l",main="Classic Transition Exponential Decay")

plot(x,a-b*((t+1)/(t+exp(r*x))),type="l",main="Mirrored Transition Exponential Decay")


plot(x,a+b*((t+1)/(t+exp(-r*x))),type="l",main="Classic Transition Exponential Increase")

plot(x,a-b*((t+1)/(t+exp(-r*x))),type="l",main="Mirrored Transition Exponential Increase")



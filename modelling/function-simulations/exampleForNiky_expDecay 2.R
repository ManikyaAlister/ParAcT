
x=1:1000

r=0.001
a=3
b=2

plot(x,a+b*exp(-r*x),type="l",main="Classic Exponential Decay")

plot(x,a+b*exp(r*x),type="l",main="Your Exponential Increase")

plot(x,a-b*exp(-r*x),type="l",main="Mirrored Exponential Decay")


t=20

plot(x,a+b*((t+1)/(t+exp(r*x))),type="l",main="Classic Transition Exponential Decay")

plot(x,a-b*((t+1)/(t+exp(r*x))),type="l",main="Mirrored Transition Exponential Decay")


plot(x,a+b*((t+1)/(t+exp(-r*x))),type="l",main="Classic Transition Exponential Increase")

plot(x,a-b*((t+1)/(t+exp(-r*x))),type="l",main="Mirrored Transition Exponential Increase")



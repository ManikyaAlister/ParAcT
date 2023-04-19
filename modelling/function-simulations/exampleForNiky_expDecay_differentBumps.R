
block = 10

x=1:100

r=0.1
a=4
b=2

x1 = 1:100
x2=1:100
trialUnlearing=6
for (i in 1:block) {
  x2[(block*(i-1)+1):(block*(i-1)+block)]=x2[(block*(i-1)+1):(block*(i-1)+block)]-(trialUnlearing*(i-1))
}

b.bump=1.4
b2=rep(b,100)
for (i in 1:block) {
  b2[(block*(i-1)+1):(block*(i-1)+block)]=b2[(block*(i-1)+1):(block*(i-1)+block)]+(b.bump*(i-1))
}

# is the idea that we estimate a different value of b each time? 

plot(x,a+b*exp(-r*x),type="l",main="Classic Exp Decay")

plot(x,a+b*exp(-r*x2),type="l",main="Classic Exp Dec ay with trial unlearning", ylab = "a") # positive b because caution should increase after the block finishes
abline(v = x[seq(10, 100, 10)], lty = 2)


plot(x,a+b2*exp(-r*x1),type="l",main="Classic Exp Decay with start bump", ylab = "a")
abline(v = x[seq(10, 100, 10)], lty = 2)


plot(x,a-b*exp(-r*x),type="l",main="Mirrored Exp Decay")
abline(v = x[seq(10, 100, 10)], lty = 2)


plot(x,a-b*exp(-r*x2),type="l",main="Mirrored Exp Decay with trial unlearning", ylab = "v")
abline(v = x[seq(10, 100, 10)], lty = 2)


plot(x,a-b2*exp(-r*x),type="l",main="Mirrored Exp Decay with start bump", ylab = "v")
abline(v = x[seq(10, 100, 10)], lty = 2)
# Step model
block = 1:24
step = 0.066
d = -step*(block-1)
a = 2.69
a.step = a-d
a.step
plot(1:(24*40),sort(rep(a.step,40),decreasing = TRUE),"l", ylab = "a", xlab = "trials")
plot(1:(24*40),sort(rep(a.step,40),decreasing = FALSE),"l", ylab = "v", xlab = "trials")


# unlearning models 
blocks = 1:24
x <- c(a.asym = 1, a.rate = 0.001, a.start = 3, trialUnlearn = 0.02)
u = x["trialUnlearn"]*(blocks-1)
a=(x["a.asym"]+x["a.start"])-x["a.start"]*exp(x["a.rate"]*data$Trial-u) 

plot(data$Trial, a,"l")
x


block = 10

x=1:100

r=0.1
a=2
b=.2

x1 = 1:100
x2=1:100
trialUnlearing=6
for (i in 1:block) {
  x2[(block*(i-1)+1):(block*(i-1)+block)]=x2[(block*(i-1)+1):(block*(i-1)+block)]-(trialUnlearing*(i-1))
}

b.bump=4
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
blocks = rep(1:10, each = 10)
step = 0.066
a = sort(blocks - step, decreasing = TRUE)
plot(1:100,a, "l", ylim = c(0,10))
x_points <- seq(1, 100, 10)
segments(x_points, par("usr")[3], x_points, par("usr")[4], col = "black", lty = "dashed")
trials = 1:100
# blocked delay model
r=0.6
a=5
b=2
t = 1000
plot(trials,a+b*((t+1)/(t+exp(r*blocks))),type="l",main="Classic Transition Exponential Decay", ylab = "a")
segments(x_points, par("usr")[3], x_points, par("usr")[4], col = "black", lty = "dashed")



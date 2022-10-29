
x=1:100

r=0.1
a=4
b=2

x1 = 1:100
x2=1:100
trialUnlearing=6
for (i in 1:10) {
  x2[(10*(i-1)+1):(10*(i-1)+10)]=x2[(10*(i-1)+1):(10*(i-1)+10)]-(trialUnlearing*(i-1))
}


b.bump=1.4
b2=rep(b,100)
for (i in 1:10) {
  b2[(10*(i-1)+1):(10*(i-1)+10)]=b2[(10*(i-1)+1):(10*(i-1)+10)]+(b.bump*(i-1))
}


plot(x,a+b*exp(-r*x),type="l",main="Classic Exp Decay")

plot(x,a+b*exp(-r*x2),type="l",main="Classic Exp Decay with trial unlearning")

plot(x,a+b2*exp(-r*x1),type="l",main="Classic Exp Decay with start bump")


plot(x,a-b*exp(-r*x),type="l",main="Mirrored Exp Decay")

plot(x,a-b*exp(-r*x2),type="l",main="Mirrored Exp Decay with trial unlearning")

plot(x,a-b2*exp(-r*x),type="l",main="Mirrored Exp Decay with start bump")


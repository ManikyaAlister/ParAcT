blocks <- 1:4
trials <- 1:160

# load output
load("~/Documents/Projects/ParAcT/modelling/knowlesetal-19/round-1/06_output/P12_a-blocked-exp-sb.Rdata")

x <- apply(theta,2, mean)

b = x["b.bump"]*(data$Block-1)

a=x["a.asym"]+(b+x["a.start"])*exp(-x["a.rate"]*data$Trial)


plot(1:160, a, "l")

data$Block

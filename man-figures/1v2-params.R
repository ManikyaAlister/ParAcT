library(here)
library(modelProb)
setwd(here())
pdf("man-figures/1v2_params.pdf",width=3*4+1,height=12)
#create matrix "m" which is the size of how many plots I'm going to have. 0 is where there's no plots,
m=matrix(1:12,nrow=6,byrow=T)
m=rbind(0,cbind(0,m,0),0)
m=cbind(m[,1:2],0,m[,3:4])
m=rbind(m[1:2,],0,m[3,],0,m[4,])#,0,m[6,],0,m[7:8,])

datasets <- c("Data Set 1", "Data Set 2", "Data Set 3")

layout(mat=m,
       widths=c(0,1,0,1,0),
       heights=c(0,1,0,1,0,1,0,1,0))
#par(mar=rep(0,4))
par(mar=c(4,6,4,8))

S = 10 #n participants

load(here("data/evansetal-17/derived/optim/n_param_comparison_AIC.Rdata"))
load(here("data/evansetal-17/derived/optim/n_param_comparison_BIC.Rdata"))

plotWeightedICs(n_param_comparison_weights_BIC, colours = c("red","blue", "seagreen"),inset = -1, xlab = "", ylab = "", main = "")
mtext(side=1,line=0.8,"Participant",cex=1.3, font = 1)
mtext(side=2,line=4.2,datasets[1],cex=1, font = 2)
mtext(side=3,line=0.8,"BIC",cex=1, font = 2)
mtext(side=2,line=2.5,"Probability",cex=1.3, font = 1)


par(xpd=TRUE)
legend("topleft", 
       legend = c( "Standard", "1 Varying", "2 Varying"), 
       col =  c("red","blue", "seagreen"),
       pch = 15,
       horiz = F, # Whether the legend is horizontal or not 
       cex = 1.4, # legend size
       border = "white",
       inset=c(1, 0))# How far below the plot the legend appears

plotWeightedICs(n_param_comparison_weights_AIC, colours = c("red","blue", "seagreen"), inset = -1, xlab = "", ylab = "", main = "")
mtext(side=3,line=0.8,"AIC",cex=1, font = 2)
mtext(side=1,line=0.8,"Participant",cex=1.3, font = 1)
mtext(side=2,line=2.5,"Probability",cex=1.3, font = 1)



#text.width = 7)  

# axis(side=2, at=seq(0,1,0.5), labels=seq(0,1,0.5),cex.axis=1.5)
# axis(side=1, at=seq(0,S,S), labels=seq(0,S,S), cex.axis=1.5)



load(here("data/evansetal-17/derived/normal/n_param_comparison_AIC.Rdata"))
load(here("data/evansetal-17/derived/normal/n_param_comparison_BIC.Rdata"))
plotWeightedICs(n_param_comparison_weights_BIC, colours = c("red","blue", "seagreen"),inset = -1, xlab = "", ylab = "", main = "")
mtext(side=2,line=4.2,datasets[2],cex=1, font = 2)
mtext(side=1,line=0.8,"Participant",cex=1.3, font = 1)
mtext(side=2,line=2.5,"Probability",cex=1.3, font = 1)


plotWeightedICs(n_param_comparison_weights_AIC, colours = c("red","blue", "seagreen"),inset = -1, xlab = "", ylab = "", main = "")
mtext(side=1,line=0.8,"Participant",cex=1.3, font = 1)
mtext(side=2,line=2.5,"Probability",cex=1.3, font = 1)


load(here("data/knowlesetal-19/derived/n_param_comparison_AIC.Rdata"))
load(here("data/knowlesetal-19/derived/n_param_comparison_BIC.Rdata"))

plotWeightedICs(n_param_comparison_weights_BIC, colours = c("red","blue", "seagreen"),inset = -1, xlab = "", ylab = "", main = "")
mtext(side=1,line=0.8,"Participant",cex=1.3, font = 1)
mtext(side=2,line=2.5,"Probability",cex=1.3, font = 1)
mtext(side=2,line=4.2,datasets[3],cex=1, font = 2)

plotWeightedICs(n_param_comparison_weights_AIC, colours = c("red","blue", "seagreen"),inset = -1, xlab = "", ylab = "", main = "")
mtext(side=1,line=0.8,"Participant",cex=1.3, font = 1)
mtext(side=2,line=2.5,"Probability",cex=1.3, font = 1)


dev.off()

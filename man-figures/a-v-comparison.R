rm(list = ls())
library(here)
library(modelProb)
pdf("man-figures/a-vs-v-comparison.pdf",width=3*4+1,height=12)

par(mfrow = c(4, 2), mar = c(2, 3, 2, 8), oma = c(3, 4, 3, 1))  # Reduce margins
datasets <- c("Data Set 1", "Data Set 2", "Data Set 3", "Data Set 4")


colours = c("orange","blue", "darkgreen", "red")

S = 10 #n participants

load(here("data/evansetal-17/derived/optim/a_v_weights_AIC.Rdata"))
load(here("data/evansetal-17/derived/optim/a_v_weights_BIC.Rdata"))

plotWeightedICs(a_v_weighted_BIC, colours = colours,inset = -1, xlab = "", ylab = "", main = "")
mtext(side=1,line=0.8,"Participant",cex=1.3, font = 1)
mtext(side=2,line=4.2,datasets[1],cex=1, font = 2)
mtext(side=3,line=0.8,"BIC",cex=1, font = 2)
mtext(side=2,line=2.5,"Probability",cex=1.3, font = 1)


plotWeightedICs(a_v_weighted_AIC, colours = colours, inset = -1, xlab = "", ylab = "", main = "")
mtext(side=3,line=0.8,"AIC",cex=1, font = 2)
mtext(side=1,line=0.8,"Participant",cex=1.3, font = 1)
mtext(side=2,line=2.5,"Probability",cex=1.3, font = 1)

par(xpd=TRUE)
legend("topleft", 
       legend = c( "a Only", "v Only", "Both a + v", "Standard DDM"), 
       col =  colours,
       pch = 15,
       horiz = F, # Whether the legend is horizontal or not 
       cex = 1, # legend size
       border = "white",
       inset=c(1, 0))# How far below the plot the legend appears


load(here("data/evansetal-17/derived/normal/a_v_weights_AIC.Rdata"))
load(here("data/evansetal-17/derived/normal/a_v_weights_BIC.Rdata"))
plotWeightedICs(a_v_weighted_BIC, colours = colours,inset = -1, xlab = "", ylab = "", main = "")
mtext(side=2,line=4.2,datasets[2],cex=1, font = 2)
mtext(side=1,line=0.8,"Participant",cex=1.3, font = 1)
mtext(side=2,line=2.5,"Probability",cex=1.3, font = 1)


plotWeightedICs(a_v_weighted_AIC, colours = colours,inset = -1, xlab = "", ylab = "", main = "")
mtext(side=1,line=0.8,"Participant",cex=1.3, font = 1)
mtext(side=2,line=2.5,"Probability",cex=1.3, font = 1)


load(here("data/knowlesetal-19/derived/a_v_weights_AIC.Rdata"))
load(here("data/knowlesetal-19/derived/a_v_weights_BIC.Rdata"))

plotWeightedICs(a_v_weighted_BIC, colours = colours,inset = -1, xlab = "", ylab = "", main = "")
mtext(side=1,line=0.8,"Participant",cex=1.3, font = 1)
mtext(side=2,line=2.5,"Probability",cex=1.3, font = 1)
mtext(side=2,line=4.2,datasets[3],cex=1, font = 2)

plotWeightedICs(a_v_weighted_AIC, colours = colours,inset = -1, xlab = "", ylab = "", main = "")
mtext(side=1,line=0.8,"Participant",cex=1.3, font = 1)
mtext(side=2,line=2.5,"Probability",cex=1.3, font = 1)

load(here("data/dutilhetal-09/derived/a_v_weights_AIC.Rdata"))
load(here("data/dutilhetal-09/derived/a_v_weights_BIC.Rdata"))

plotWeightedICs(a_v_weighted_BIC, colours = colours,inset = -1, xlab = "", ylab = "", main = "")
mtext(side=1,line=0.8,"Participant",cex=1.3, font = 1)
mtext(side=2,line=2.5,"Probability",cex=1.3, font = 1)
mtext(side=2,line=4.2,datasets[4],cex=1, font = 2)

plotWeightedICs(a_v_weighted_AIC, colours = colours,inset = -1, xlab = "", ylab = "", main = "")
mtext(side=1,line=0.8,"Participant",cex=1.3, font = 1)
mtext(side=2,line=2.5,"Probability",cex=1.3, font = 1)


dev.off()

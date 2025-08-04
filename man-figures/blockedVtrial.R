library(here)
library(modelProb)
rm(list = ls())
pdf("man-figures/blockedVtrial.pdf",width=3*4+1,height=12)
par(mfrow = c(4, 2), mar = c(2, 3, 2, 8), oma = c(3, 4, 3, 1))  # Reduce margins
datasets <- c("Data Set 1", "Data Set 2", "Data Set 3", "Data Set 4")
colours = RColorBrewer::brewer.pal(4,"Set2")

plotQuantProbs = function(weights, starting_x = 0){
  # Compute aggregated weighted probabilities for the current dataset
  agg_prob <- colSums(weights)/sum(weights) 
  
  # get position that's about .75 of the full plot width
  full_text_width = nrow(weights)
  
  # Define label positions
  x_positions <- seq(starting_x, full_text_width, length.out = 4)  # Space out horizontally
  
  
  
  # Format labels with colors
  prob_labels <- paste0(c("Block: ", "Trial: ", "Both: ", "DDM: "), sprintf("%.2f", agg_prob))
  
  # Ensure text is drawn outside the plotting region
  par(xpd = TRUE) 
  
  # Add labels at spaced-out positions
  for (i in seq_along(prob_labels)) {
    if(i == 4) {
      x_position = x_positions[i] 
    } else{
      x_position = x_positions[i] 
    }
    text(x = x_position, y = 1.1, labels = prob_labels[i], col = colours[i], adj = 0, cex = 1.8, font = 2)
  }
  
  # Reset plotting behavior
  par(xpd = FALSE)
}


load(here("data/evansetal-17/derived/optim/blockVtrial_AIC.Rdata"))
load(here("data/evansetal-17/derived/optim/blockVtrial_BIC.Rdata"))

# order based on performance on most complex combination per BIC 
ordered <- order(weighted_blockVtrial_BIC[,"Best Mix"])

plotWeightedICs(weighted_blockVtrial_BIC[ordered,], colours =colours,inset = -1, xlab = "", ylab = "", main = "")
plotQuantProbs(weighted_blockVtrial_BIC)
mtext(side=1,line=0.8,"Participant",cex=1.3, font = 1)
mtext(side=2,line=4.2,datasets[1],cex=1, font = 2)
mtext(side=3,line=2,"BIC",cex=2, font = 2)
mtext(side=2,line=2.5,"Probability",cex=1.3, font = 1)

plotWeightedICs(weighted_blockVtrial_AIC[ordered,], colours =colours, inset = -1, xlab = "", ylab = "", main = "")
plotQuantProbs(weighted_blockVtrial_AIC)
mtext(side=3,line=2,"AIC",cex=2, font = 2)
mtext(side=1,line=0.8,"Participant",cex=1.3, font = 1)
mtext(side=2,line=2.5,"Probability",cex=1.3, font = 1)

par(xpd=TRUE)
legend("topleft", 
       legend = c( "Block", "Trial", "Both", "Standard"), 
       col = colours,
       pch = 15,
       horiz = F, # Whether the legend is horizontal or not 
       cex = 1.4, # legend size
       border = "white",
       inset=c(1, 0))# How far below the plot the legend appears

load(here("data/evansetal-17/derived/normal/blockVtrial_AIC.Rdata"))
load(here("data/evansetal-17/derived/normal/blockVtrial_BIC.Rdata"))

# order based on performance on most complex combination per BIC 
ordered <- order(weighted_blockVtrial_BIC[,"Best Mix"])

plotWeightedICs(weighted_blockVtrial_BIC[ordered,], colours =colours,inset = -1, xlab = "", ylab = "", main = "")
plotQuantProbs(weighted_blockVtrial_BIC)
mtext(side=2,line=4.2,datasets[2],cex=1, font = 2)
mtext(side=1,line=0.8,"Participant",cex=1.3, font = 1)
mtext(side=2,line=2.5,"Probability",cex=1.3, font = 1)


plotWeightedICs(weighted_blockVtrial_AIC[ordered,], colours =colours,inset = -1, xlab = "", ylab = "", main = "")
plotQuantProbs(weighted_blockVtrial_AIC)
mtext(side=1,line=0.8,"Participant",cex=1.3, font = 1)
mtext(side=2,line=2.5,"Probability",cex=1.3, font = 1)


load(here("data/knowlesetal-19/derived/blockVtrial_BIC.Rdata"))
load(here("data/knowlesetal-19/derived/blockVtrial_AIC.Rdata"))

# order based on performance on most complex combination per BIC 
ordered <- order(weighted_blockVtrial_BIC[,"Best Mix"])

plotWeightedICs(weighted_blockVtrial_BIC[ordered,], colours =colours,inset = -1, xlab = "", ylab = "", main = "")
plotQuantProbs(weighted_blockVtrial_BIC)
mtext(side=1,line=0.8,"Participant",cex=1.3, font = 1)
mtext(side=2,line=2.5,"Probability",cex=1.3, font = 1)
mtext(side=2,line=4.2,datasets[3],cex=1, font = 2)

plotWeightedICs(weighted_blockVtrial_AIC[ordered,], colours =colours,inset = -1, xlab = "", ylab = "", main = "")
plotQuantProbs(weighted_blockVtrial_AIC)
mtext(side=1,line=0.8,"Participant",cex=1.3, font = 1)
mtext(side=2,line=2.5,"Probability",cex=1.3, font = 1)

load(here("data/dutilhetal-09/derived/blockVtrial_BIC.Rdata"))
load(here("data/dutilhetal-09/derived/blockVtrial_AIC.Rdata"))

# order based on performance on most complex combination per BIC 
ordered <- order(weighted_blockVtrial_BIC[,"Best Mix"])

plotWeightedICs(weighted_blockVtrial_BIC[ordered,], colours =colours,inset = -1, xlab = "", ylab = "", main = "")
plotQuantProbs(weighted_blockVtrial_BIC)
mtext(side=1,line=0.8,"Participant",cex=1.3, font = 1)
mtext(side=2,line=2.5,"Probability",cex=1.3, font = 1)
mtext(side=2,line=4.2,datasets[4],cex=1, font = 2)

plotWeightedICs(weighted_blockVtrial_AIC[ordered,], colours =colours,inset = -1, xlab = "", ylab = "", main = "")
plotQuantProbs(weighted_blockVtrial_AIC)
mtext(side=1,line=0.8,"Participant",cex=1.3, font = 1)
mtext(side=2,line=2.5,"Probability",cex=1.3, font = 1)

dev.off()

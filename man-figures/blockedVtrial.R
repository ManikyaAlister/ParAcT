library(here)
library(modelProb)
rm(list = ls())
pdf("man-figures/blockedVtrial.pdf",width=11,height=10)
par(mfrow = c(4, 2), mar = c(2, 3, 2, 4), oma = c(3, 4, 3, 1))  # Reduce margins
datasets <- c("Data Set 1", "Data Set 2", "Data Set 3", "Data Set 4")
colours = RColorBrewer::brewer.pal(4,"Set2")
plotQuantProbs = function(weights, starting_x = 0){
  # Compute aggregated weighted probabilities
  agg_prob <- colSums(weights)/sum(weights) 
  
  # Adjust text position to avoid crowding
  full_text_width = nrow(weights) * 0.9
  
  # Define x positions (spread across width)
  x_positions <- seq(starting_x, full_text_width, length.out = length(agg_prob))
  
  # Format numbers without leading zero
  prob_values <- sub("^0+", "", sprintf("%.2f", agg_prob))  # e.g., "0.31" -> ".31"
  
  # Add labels
  prob_labels <- paste0(c("DDM: ", "Block: ", "Trial: ", "Both: "), prob_values)
  
  # Allow drawing outside plot area
  par(xpd = TRUE) 
  
  # Draw each label
  for (i in seq_along(prob_labels)) {
    x_position <- x_positions[i]
    text(x = x_position, y = 1.1, labels = prob_labels[i], col = colours[i], adj = 0, cex = 2, font = 2)
  }
  
  # Reset drawing limits
  par(xpd = FALSE)
}

column_order <- c("Simple","Best block change", "Best trial change", "Best Mix") 

load(here("data/evansetal-17/derived/optim/blockVtrial_AIC.Rdata"))
load(here("data/evansetal-17/derived/optim/blockVtrial_BIC.Rdata"))

# re order columns by complexity of model groupings
weighted_blockVtrial_AIC <- weighted_blockVtrial_AIC[,column_order]
weighted_blockVtrial_BIC <- weighted_blockVtrial_BIC[,column_order]


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



load(here("data/evansetal-17/derived/normal/blockVtrial_AIC.Rdata"))
load(here("data/evansetal-17/derived/normal/blockVtrial_BIC.Rdata"))

# re order columns by complexity of model groupings
weighted_blockVtrial_AIC <- weighted_blockVtrial_AIC[,column_order]
weighted_blockVtrial_BIC <- weighted_blockVtrial_BIC[,column_order]

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

# re order columns by complexity of model groupings
weighted_blockVtrial_AIC <- weighted_blockVtrial_AIC[,column_order]
weighted_blockVtrial_BIC <- weighted_blockVtrial_BIC[,column_order]

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

# re order columns by complexity of model groupings
weighted_blockVtrial_AIC <- weighted_blockVtrial_AIC[,column_order]
weighted_blockVtrial_BIC <- weighted_blockVtrial_BIC[,column_order]

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

par(xpd=TRUE)
legend("topright", 
       legend = c("Standard DDM", "Block", "Trial", "Both"), 
       fill =  colours,
       #col = colours,
       #pch = 15,
       horiz = F, # Whether the legend is horizontal or not 
       cex = 1.7, # legend size
       bty = "n",
       #bg = rgb(1, 1, 1, alpha = 0.8),
       # legend background 
       border = "black")#,

dev.off()

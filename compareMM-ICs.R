# Multiple Model Comparison Plots 
library(here)
library(roxygen2)

### 
# Take the best 6 single parameter models and the best 6 two parameter models and 
# see the combined probabilities

MMComparisonPlot =  function(ICweights,#' @param ICweights a numeric data frame/matrix/array that contains the model comparison information criterion (e.g., AIC, BIC, LOOIC) of each participant (rows) and each model (column)
                             models1, #' @param models1,models2 vector of column names or column numbers indicating a group of models to be compared together. Note that models1 and models2 must have the same length.
                             models2,
                             main = "Multiple Models Comparison", #'  @param main title of plot 
                             ylab = "Probability", #'  @param xlab x axis label 
                             xlab = "Participant", #'  @param ylab y axis label
                             groupNames = c("models1","models2"), #' @param groupNames character vector of length 2 describing the names of the two groups of models (to be shown on the plot legend)
                             inset= c(-0.5,-.25), #' @param inset determines the position of the plot legend
                             colours = c("darkgreen","lightblue")){ #' @param colours character vector of length 2 containing valid r graphics colours. 
  
  
  nModels1 = length(models1) # length of first group of models 
  nModels2 = length(models2) # length of second group of models
  
  if (nModels1 != nModels2){warning("models1 and models2 are not the same length. Be careful with your interpretations")} # check to make sure there are the same amount of models in each group
  
  nModels = nModels1+nModels2
  nSubj = length(ICweights[, 1])
  
  # draw blank plot area 
  
  plot( 
    x = 100,
    y = 100,
    xlim = c(0, nSubj+0.5),
    ylim = c(0, 1),
    xlab = "",
    ylab = "",
    main = "",
    xaxt = "n",
    yaxt = "n"  
    )
  
  # fill with bar plot 
  
  for (i in 1:nSubj) {
    use.i=i
    sumThing=0
    col=colours[1]
    currWeight=sum(ICweights[use.i,models1])
    rect(i-0.5,0,i+0.5,currWeight, border = col, col = col)
    sumThing=sumThing+currWeight
    col=colours[2]
    currWeight=sum(ICweights[use.i,models2])
    rect(i-0.5,sumThing,i+0.5,sumThing+currWeight, border = col, col = col)
    sumThing=sumThing+currWeight
  }
  
  title(main = main, xlab = xlab, ylab = ylab, line = 0.2)
  
  axis(side=2, at=seq(0,1,0.5), labels=seq(0,1,0.5),cex.axis=1.5)
  axis(side=1, at=seq(0,nSubj,nSubj), labels=seq(0,nSubj,nSubj), cex.axis=1.5)
  
  par(xpd=TRUE) # lets plot legend be drawn outside of plot area
  
  legend("bottom", 
         legend = groupNames, 
         col = colours,
         pch = 15,
         #horiz = T,
         cex = .7,
         inset = inset,
         ncol = 2) #' @returns a stacked bar plot
    
}


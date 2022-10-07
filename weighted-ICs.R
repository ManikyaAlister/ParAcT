# *** weighted model probabilities ***
library(here)
library(roxygen2)



# Input: 
# 1. a matrix with all of the ICs for each model and each participant

# load all AICs and BICs

#' Get the weighted probabilities of different models for each participant
#' 
#' 'weightedICs' takes the raw model comparison information criterion values (e.g., AIC, BIC, LOOIC)
#' for each model (columns) and participant (rows) and then converts them into a relitive probability
#' of that model being the best performing model according to that model comparison criterion.

weightedICs = function(ICs){ #' @param ICs a numeric matrix/array/data frame containing the raw model comparison criterion values (e.g., AIC, BIC, LOOIC) for each model (columns) and participant (rows).
  
  getWeights=function(x) {
    useX = x*(-0.5) # transform model criterion to a chi square distribution
    if (mean(is.na(useX)) == 1) {
      return(NA)
    }
    maxLogDens=max(useX) # calculate the maximum log density 
    if (maxLogDens > 700) {
      densTransform=maxLogDens-700
      useX=useX-densTransform
    } else if (maxLogDens < -710) {
      densTransform=maxLogDens-700
      useX=useX-densTransform
    } else {
      densTransform=0
    }
    exp(useX)/sum(exp(useX)) # convert to relative probabilities 
  }
  
  nSubj = length(ICs[,1])
  nModels = length(ICs[1,])
  ICweights=array(NA,c(nSubj,nModels))
  for (s in 1:nSubj) {
    ICweights[s,]= getWeights(as.matrix(ICs[s,]))
  }
  colnames(ICweights) = colnames(ICs)
  return(ICweights) #' @returns a matrix of probabilities
}


### Plot
#' Plot weighted model comparisons
#' 
#' 'plotWeightedICs' takes a matrix/array/data frame that contains a model comparison criterion (e.g., AIC, BIC, LOOIC) value 
#' for each model (columns) and participant (rows) (which can be created made using the "weightedICs" function) and turns it into a stacked bar 
#' plot. 

plotWeightedICs = function(ICweights, #' @param ICweights a numeric data frame/matrix/array that contains the model comparison information criterion (e.g., AIC, BIC, LOOIC) of each participant (rows) and each model (column)
                           main = "Weighted Model Comparison", #'  @param main title of plot 
                           ylab = "Probability", #'  @param xlab x axis label 
                           xlab = "Participant", #'  @param ylab y axis label
                           colours = "random", #' @param colours,seed character vector of base r colours. Default generates colours for each variable randomly,  You can also try different numeric values for "seed" to try out different random colour combinations. 
                           seed = FALSE,
                           inset = c(-0.5,-.34)) { #' @param inset determines the position of the plot legend
  nModels = length(ICweights[1,])
  nSubj = length(ICweights[, 1])
  
  if (is.numeric(seed)) {
    set.seed(seed) # keep any random colours that are generated constant
  } 
  
  set.seed(seed)
  
  # choose colours for each model in the barplot
  if (colours == "random") {
    colours = sample(colors(), size = nModels, replace = FALSE) # select random colours if user does not want any particular colours
  } else {
    colours = colours # must be a vector of valid base r colours of length nModels
    if (length(colours) != length(nModels)) {
      stop("Colours must be a vector of the same length as the number of models")
    } else if (is.vector(colours) == FALSE) {
      stop("Colours must be a vector")
    }
  }
  
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
  
  for (i in 1:nSubj) {
    use.i = i
    sumThing = 0
    for (j in 1:nModels) {
      col = colours[j] # Purple
      rect(i-0.5,sumThing,i+0.5,sumThing+ICweights[use.i,j], border = col, col = col)
      sumThing = sumThing + ICweights[use.i, j]
    }
  } 
  
  title(main = main, xlab = xlab, ylab = ylab, line = 0.2)
  
  axis(side=2, at=seq(0,1,0.5), labels=seq(0,1,0.5),cex.axis=1.5)
  axis(side=1, at=seq(0,nSubj,nSubj), labels=seq(0,nSubj,nSubj), cex.axis=1.5)
  
  par(xpd=TRUE) # lets plot legend be drawn outside of plot area
  
  legend("bottom", 
         legend = colnames(ICweights), 
         col = colours,
         pch = 15,
         #horiz = T,
         cex = .5,
         inset=inset,
         ncol = 4) #' @returns a stacked bar plot
  
}


# Other things for  this package: 
# 1. number of participants best fit by each model
# 2. overall percent probability of each model collapsed across participants + plot for that? 


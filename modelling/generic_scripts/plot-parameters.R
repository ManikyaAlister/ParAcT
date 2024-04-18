#' Function to plot time-varying parameters for each individual
#'
#' @param parameter Standard diffusion model parameter to be plotted (v, a , z, t0).
#' @param functions List of time-varying functions for each parameter that define how they change across time.
#' @param data Data that contains the trial/block.
#' @param theta Array of parameters outputted by the model fits.
#' @param plot_path File path that points to the folder the plots will be saved to. 
#' @param subject The subject being plotted.
#'
#' @return Png line graph plotting the parameter across time. 
#' @export
#'
#' @examples
plotParamsIndividual = function(parameter, functions, data, theta, plot_path, subject, blocked_likelihood){
  params = apply(theta,2,mean)
  paractFunction <- functions[[parameter]]
  # check if the function requires a "block" (b) argument and if so, different plotting procedure. 
  if (blocked_likelihood & ("b" %in% names(formals(paractFunction)))){
    blocks <- unique(data$Block)
    paract <- c()
    for (block in blocks){
      paract_iteration <- paractFunction(params, data = data[data$Block == block,], b = block)
      if (length(paract_iteration) == 1){
        paract_iteration <- rep(paract_iteration, length(data[data$Block == block,1]))
      }
      paract <- c(paract, paract_iteration)
    }
  } else {
    paract <- paractFunction(params, data = data)
  }
  if (length(paract) == 1){
    paract <- rep(paract, length(nrow(data)))
  }
  png(filename = paste0(plot_path(), "P", subject, "-",model,"-",parameter,"-parameter-plot.png"))
  plot(1:length(paract), paract, "l", ylab = paste0(parameter, " (",model," model)"), xlab = "Time", ylim = c(0,7))
  dev.off()
  print(paste0("Plot saved for ",parameter))
}

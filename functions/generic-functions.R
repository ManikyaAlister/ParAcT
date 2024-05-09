# function to rank models based on their wegighted probability
rank_models <- function(scores_array) {
  # apply the ranking function to each row of the array
  ranked_array <- t(apply(scores_array, 1, rank))
  
  # create a new array to store the model names
  model_names <-
    array(dim = dim(ranked_array),
          dimnames = dimnames(ranked_array))
  
  # loop over each row of the ranked array
  for (i in 1:nrow(ranked_array)) {
    # sort the row by the rank and get the corresponding column names
    model_names[i,] <-
      colnames(ranked_array)[order(ranked_array[i,])]
  }
  
  colnames(model_names) <- 1:length(scores_array[1,])
  return(model_names)
}
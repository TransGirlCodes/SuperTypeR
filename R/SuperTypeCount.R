#' @name countSuperTypes
#' @title Count the number of each supertype in each population.
#' @export
countSuperTypes <- function(samples){
  countMatrix <- matrix(0,
                        nrow = length(unique(samples$Supertype)),
                        ncol = length(unique(samples$Site)))
  colnames(countMatrix) <- as.character(unique(samples$Site))
  rownames(countMatrix) <- as.character(unique(samples$Supertype))
  updateCountMatrix <- function(sampleRow){
    countMatrix[as.character(sampleRow[5]), as.character(sampleRow[2])] <<-
      countMatrix[as.character(sampleRow[5]), as.character(sampleRow[2])] + 1
  }
  apply(samples, 1, function(x){updateCountMatrix(x)})
  return(countMatrix)
}

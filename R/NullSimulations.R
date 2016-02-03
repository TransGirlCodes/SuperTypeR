
randomiseSupertypes <- function(desigs){
  # Extract data from the origional dataframe.
  alleles <- desigs[, 2:3]
  superType <- desigs$Supertype

  # Shuffle the alleles randomly by sampling without replacement.

  # Generate a random order of alleles in the table.
  rand_idx <- sample(1:nrow(alleles), size = length(superType), replace = FALSE)

  # Create a new table from the old table, pulling out alleles according to the random order.
  rand_alleles <- alleles[rand_idx, ]

  # Create new table of random allele designations.
  nullTable <- cbind(superType, rand_alleles)

  return(nullTable)
}


#' @name nullSimulations
#' @title Generate Null Simulations using your data.
#' @export
nullSimulations <- function(obs, desigs, iter = 1000, boot = NULL){
  return(lapply(1:iter, function(i){
    apply(oneSimIter(obs, desigs, boot), 2, mean)
    }))
}


oneSimIter <- function(obs, desigs, boot = NULL){
  # Randomise the supertype definitions
  rand_desigs <- randomiseSupertypes(desigs)

  # Reassign supertypes to the samples
  newSTs <- assignSuperTypes(obs, rand_desigs)
  obs$Supertype <- newSTs

  # Count the number of supertypes per population.
  superTypeCounts <- countSuperTypesPerPop(obs)

  # Calculate Pairwise Jost D.
  return(pairwiseD22(superTypeCounts, boot = boot))
}


#' @name assignSuperTypes
#' @title Assign a supertype for each allele of each individual.
#' @export
assignSuperTypes <- function(obs, desigs){
  return(apply(obs, 1, function(x) desigs$superType[which(x[4] == desigs$Allele)]))
}






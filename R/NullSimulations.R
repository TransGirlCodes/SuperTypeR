











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


nullSimulations <- function(obs, desigs){





}


oneSimIter <- function(obs, desigs){
  # Randomise the supertype definitions
  rand_desigs <- randomiseSupertypes(desigs)

  # Reassign supertypes to the samples
  newSTs <- assignSuperTypes(obs, rand_desigs)
  obs$Supertype <- newSTs



}

#' @name assignSuperTypes
#' @title Assign a supertype for each allele of each individual.
#' @export
assignSuperTypes <- function(obs, desigs){
  idx <- apply(obs, 1, function(x) which(x[4] == desigs$Allele))
  supertypes <- desigs$superType[idx]
  return(supertypes)
}






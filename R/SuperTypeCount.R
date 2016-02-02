# Counting of Alleles and Supertypes.


#' @name countSuperTypes
#' @title Count the number of each supertype in each population.
#' @export
countSuperTypesPerPop <- function(samples){
  samples[, 5] <- as.character(samples[, 5])
  samples[, 2] <- as.character(samples[, 2])
  countMatrix <- matrix(0,
                        nrow = length(unique(samples$Supertype)),
                        ncol = length(unique(samples$Site)))
  colnames(countMatrix) <- as.character(unique(samples$Site))
  rownames(countMatrix) <- as.character(unique(samples$Supertype))
  for(i in 1:nrow(samples)){
    countMatrix[samples[i, 5], samples[i, 2]] <- countMatrix[samples[i, 5], samples[i, 2]] + 1
  }
  return(countMatrix)
}


#' @name countAlleles
#' @title Count the number of distinct alleles each individual has.
#' @export
countAlleles <- function(obs) {
  alleleCounts <- group_by(obs, Sample) %>%
    summarise(rep = n(), nAllele = n_distinct(Genotype))

  repeated <- rep.int(alleleCounts$nAllele, alleleCounts$rep)

  return(repeated)
}


#' @name countSuperTypes
#' @title Count the number of distinct supertypes each individual has.
#' @export
countSuperTypes <- function(obs){
  superTypes <- group_by(obs, Sample) %>%
    summarise(nSuper = n_distinct(Supertype), rep = n())

  repeated <- rep.int(superTypes$nSuper, superTypes$rep)

  return(repeated)
}

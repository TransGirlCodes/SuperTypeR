# Counting of Alleles and Supertypes.


#' @name countSuperTypes
#' @title Count the number of each supertype in each population.
#' @export
countSuperTypesPerPop <- function(samples){
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

setwd("~/Desktop/Academic/Supertypes")
samples <- read.csv("Samples.csv")
STdesig <- read.csv("SuperType_Designations.csv")

convertSamplesData <- function(table){
  do.call(rbind, apply(table, 1, function(x){

    geno <- x[4:12]
    geno <- geno[which((geno != "") & (!is.na(geno)))]

    st <- x[14:22]
    st <- st[which(!is.na(st))]

    if(length(geno) == length(st)){
      return(
        data.frame(Sample = rep(x[1], length(geno)),
                   Site = rep(x[2], length(geno)),
                   Drainage = rep(x[3], length(geno)),
                   Genotype = geno,
                   Supertype = st,
                   AlleleNum = rep(x[24], length(geno)),
                   SuperTypeNum = rep(x[25], length(geno))
        )
      )
    } else {
      errmsg <- paste0("ERROR: The number of genotypes is ",
                       length(geno),
                       ". \nHowever the number of supertypes assigned is ",
                       length(st), "!")
      stop(errmsg)
    }
  }))
}

reshapedSamples <- convertSamplesData(samples)

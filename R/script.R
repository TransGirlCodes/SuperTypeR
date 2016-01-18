#!/usr/bin/Rscript

data <- read.table("random STs 1 in.txt")

allRes <- main(data)

## pairwise estimates matrix

nCommunities <- ncol(data)
D22_PW <- matrix(0, nCommunities, nCommunities)

for(i in 1:(nCommunities - 1)){
  for(j in (i + 1):nCommunities){
    D22_PW[i, j] <- D22_PW[j, i] <- main(data[, c(i, j)], boot = 1000)[1]
  }
}

write.csv(D22_PW, file = "pairwise_D22.csv")

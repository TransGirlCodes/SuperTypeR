

main <- function(x, boot = 1000L){
  method <- "MVUE"
  nCommunities <- ncol(x)
  nPerCommunity <- colSums(x)

  p_hat <- sapply(1:nCommunities, function(j){ x[, j] / nPerCommunity[j] })

  D2n <- D2n_equ(x, method = "MVUE")

  if(D2n < 0){
    method <- "MLE"
  } else {
    if(D2n > 0){
      method <- "MVUE"
    } else {
      stop("ERROR: D2n is neighter higher or lower than zero!")
    }
  }

  bootD2n <- rep(0, boot)
  D2n <- D2n_equ(x, method = method)

  for(rep in 1:boot){
    bootSample <- sapply(1:nCommunities, function(j){
      rmultinom(1, nPerCommunity[j], p_hat[, j])
    })
    bootD2n[rep] <-D2n_equ(bootSample, method = method)
  }

  D2n_se <- sd(bootD2n)
  result <- c(Mean = D2n, SE = D2n_se)
  return(result)
}

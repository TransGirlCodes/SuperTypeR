#' @import dplyr
NULL


#' @name MVUE
#' @title MVUE method.
MVUE <- function(xk, n){
  return(sum(xk * (xk - 1) / n / (n - 1)))
}

#' @name MLE
#' @title MLE method.
MLE <- function(xk, n){
  return(sum(xk * xk / n / n))
}

#' @name MVUE_MLE
#' @title Calculate either MVUE or MLE
MVUE_MLE <- function(x, nInCom, method){

  out <- sapply(1:ncol(x), function(k){
    xk <- x[, k]
    n <- nInCom[k]

    method(xk, n)
  })

  res <- sum(out)
  return(res)

}


#' @name D2n_equ
#' @title Calculate Jost's D
#' @export
D2n_equ <- function(x, method = "MVUE") {

  nCommunities <- ncol(x)
  nInCommunity <- colSums(x)
  temp1 <- as.matrix(x) %*% matrix(1 / nInCommunity, nCommunities, 1)
  temp2 <- sum(sapply(1:nCommunities, function(k) {
    sum((x[, k]/nInCommunity[k])^2)
  }))

  useMVUE <- "MVUE" %in% method
  useMLE <- "MLE" %in% method

  if(useMVUE && !useMLE) {
    c2n_denominator <- MVUE_MLE(x, nInCommunity, MVUE)
  } else {
    if(!useMVUE && useMLE) {
      c2n_denominator <- MVUE_MLE(x, nInCommunity, MLE)
    } else {
      stop("ERROR: Invalid selection for parameter 'method'. Must be 'MVUE' or 'MLE'.")
    }
  }

  c2n_numerator <- (sum(temp1 ^ 2) - temp2) / (nCommunities - 1)
  c2n <- c2n_numerator / c2n_denominator
  D2n <- 1 - c2n
  return(D2n)

}


#' @name bootstrapJostD
#' @title Calculate Jost's D, Mean and SE by bootstrap
#' @export
bootstrapJostD <- function(x, boot = 1000L){
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


#' @name pairwiseD22
#' @title Calculate Pairwise Jost's D.
#' @description Calculate Pairwise Jost's D, optionally bootstrap to calculate mean and SD.
#' @export
pairwiseD22 <- function(data, boot = NULL){

  nCommunities <- ncol(data)
  mat <- matrix(0, nCommunities, nCommunities)
  coms <- colnames(mat) <- rownames(mat) <- colnames(data)

  # Use a Lexical Scope trick!
  # Don't do this sort of thing unless you understand
  # how R searches for things, and what lexical scope is,
  # and how R's 'superassignment operator': '<<-' works,
  # compared to the regular assignment operator: '<-'

  # BEGIN LEXICAL SCOPE TRICK
  if(!is.null(boot)){

    # If you want to estimate a bootstrap in pairwise calcs,
    # set up output object, and create function which does stuff.

    out <- list(Mean = mat, SE = mat)

    doCalc <- function() {
      ans <- bootstrapJostD(data[, c(coms[i], coms[j])], boot = boot)
      out$Mean[coms[i], coms[j]] <<- out$Mean[coms[j], coms[i]] <<- ans[1]
      out$SE[coms[i], coms[j]] <<- out$SE[coms[j], coms[i]] <<- ans[2]
    }

  } else {

    # If you DON'T want to estimate a bootstrap in pairwise calcs,
    # set up output object, and create function which does stuff.

    out <- mat

    doCalc <- function() {
      ans <- D2n_equ(data[, c(coms[i], coms[j])])
      out[coms[i], coms[j]] <<- out[coms[j], coms[i]] <<- ans
    }

  }
  # END LEXICAL SCOPE TRICK

  for(i in 1:(nCommunities - 1)){
    for(j in (i + 1):nCommunities){
      doCalc()
    }
  }

  return(out)

}

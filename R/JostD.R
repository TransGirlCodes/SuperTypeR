



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



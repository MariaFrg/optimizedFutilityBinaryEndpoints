source("calculate_twostage_OCs.R")

#' Determines the minimum sample size for a one-sided, one-sample exact
#' binomial test with specified alpha (type I) and beta (type II) errors
#' (needed for the function 'search_opt_fut_boundary')
#' 
#' @param p0 Null hypothesis response probability
#' @param p1 Alternative hypothesis response probability
#' @param alpha Type I error rate
#' @param beta Type II error rate
#' @param n.min Minimum sample size considered
#'
#' @return
#' \code{bin1samp} returns a vector giving the minimum sample size
#' (\code{n}), the critical value \code{r} (reject if outcome is more extreme
#' than \code{r}), the null and alternative response probabilities (\code{p0}
#' and \code{pa}), and the type I and type II errors (\code{size} and
#' \code{type2}).)
#' modified from https://github.com/raredd/desmon/blob/master/R/bin1samp.R
bin1samp <- function(p0, p1, alpha = 0.1, beta = 0.1, n.min = 5) {
  if (p0 >= p1 )
    stop('p0 and p1 are identical or p0 is greater than p1')
  b <- 1
  x <- round(p0 * n.min)
  n <- n.min - 1
  if (p1 > p0) {
    while (b > beta) {
      n <- n + 1
      # determine cutoff: reject if X>x
      l <- x:n
      s <- 1 - pbinom(l, n, p0)
      sub <- s <= alpha
      x <- l[sub][1]
      a <- s[sub][1]
      b <- pbinom(x, n, p1)
    }
  } 
  
  structure(
    c(n = n,  r = x, p0 = p0, p1 = p1,  type1 = a, type2 = b),
    class = 'bin1samp'
  )
}

#test one sample
#bin1samp(0.5,0.65,0.05,0.2)

#' searches for fixed r and n 'optimal' r1 and n1 (optimizes regarding en_p0)
#' n is selected as the minimum sample size for a one-sided, one-sample exact binomial test fulfilling the alpha and beta constraints (without interim analysis)
#' and r is selected as the corresponding number of responses
#' 
#' @param p0 Null hypothesis response probability
#' @param p1 Alternative hypothesis response probability
#' @param alpha Type I error rate
#' @param beta Type II error rate
#' @param pi_wrong limit for the probability to wrongly stop for futility when in fact the underlying standardized treatment effect is given by the relevant effect delta
#' @param power_loss denotes the admissible over-all power loss caused by applying a binding futility boundary
#' @param min1stage minimal fraction of n that should be included in the interim analysis
#' @param max1stage maximal fraction of n that should be included in the interim analysis
#' @param return_all indicates if all admissible r1 and n1 should be returned (TRUE) or only the optimal r1 and n1 (FALSE)
search_opt_fut_boundary <- function(p0, p1, pi_wrong, power_loss, alpha=0.05, beta = 0.2, min1stage = 1/3, max1stage = 2/3, return_all = FALSE){
  # calculates the minimum sample size for a one-sided, one-sample exact binomial test fulfilling the alpha and beta constraints (without interim analysis) 
  onesamp <- bin1samp(p0, p1, alpha, beta)
  
  # initializes a new matrix z to store all admissible results and to later search the best design
  # first row is the design without interim analysis 
  z <- matrix(c(0,0,onesamp[2], onesamp[1], onesamp[5], onesamp[6], 0,0,0,onesamp[1], onesamp[1]), nrow = 1)
  colnames(z) <- c("r1","n1","r","n","alpha","beta","PET(p1)","pow_loss","PET(p0)","en0", "en1")
  
  # search through all admissible n1
  for(n1_temp in (ceiling(z[1,4]*min1stage):floor(z[1,4]*max1stage))){
    # search through all possible r1 for n1_temp
    for (r1_temp in 0:n1_temp) {
      pet_p0 <- pbinom(r1_temp,n1_temp,p0)  
      pet_p1 <- pbinom(r1_temp,n1_temp,p1)
      
      #z[1,3] = r and z[1,4] = n from onesamp
      i <- (r1_temp+1):min(n1_temp, z[1,3])
      # type 1 error
      err1 <- 1 - (pet_p0 + sum(dbinom(i, n1_temp, p0)*pbinom(z[1,3]-i, z[1,4]-n1_temp, p0)))
      # type 2 error
      err2 <- pet_p1 + sum(dbinom(i, n1_temp, p1)*pbinom(z[1,3]-i, z[1,4]-n1_temp, p1))
      
      # check if all conditions are met for the parameters n1_j and r1_temp
      if(err2 <= beta + power_loss && err1 <= alpha && pet_p1 <= pi_wrong){
        # expected sample size under p0
        en_p0 <- n1_temp + (z[1,4]-n1_temp)*(1-pet_p0)
        # expected sample size under p1 
        en_p1 <- n1_temp + (z[1,4]-n1_temp)*(1-pet_p1)
        #add n1_temp with its corresponding r1_temp to the table for later use, if all the operational characteristics fit under the criteria
        z <- rbind(z, c(r1_temp, n1_temp, z[1,3], z[1,4], err1, err2, pet_p1, err2-beta, pet_p0, en_p0, en_p1))
      }
    } # end r1 loop 
  } # end n1 loop
  
  #next step filter out the row with the highest PET0/least EN0. 
  if (dim(z)[1] == 1 )
    stop('no solution found')
  
  z <- z[-1,] #remove the row without interim analysis
  z <- z[order(-z[,9],z[,10],decreasing = FALSE),] # 9 is pet_p0 (pi_correct) - could also search for 10 en0 sample size first before 9.
  
  if(return_all == TRUE){
    return(z)
  }
  else{
    return(z[1,])
  }
}

# combine function with calculation of OCs
calculate_optBoundaryOCs <- function(p0, p1, pi_wrong, power_loss, alpha, beta, min1stage = 1/3, max1stage = 2/3, return_all = FALSE){
  optBound <- search_opt_fut_boundary(p0, p1, pi_wrong, power_loss, alpha, beta, min1stage, max1stage, return_all)
  calculate_twostage_OCs(p0, p1, optBound[1], optBound[2], optBound[3], optBound[4])
}


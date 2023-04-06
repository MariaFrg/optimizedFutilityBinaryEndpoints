# packages
library(clinfun)
library(tidyverse)
library(writexl)


#' calculate OC's for a two stage design with option to stop for futility
#' 
#' @param p0 Null hypothesis response probability
#' @param pa Alternative hypothesis response probability
#' @param n1 number of patients in the first stage
#' @param n total sample size (first and second stage together)
#' @param r1 maximal number responses in first stage where drug would still be declared ineffective
#' @param r maximal number of total responses for drug to be declared ineffective
#' 
#' @return vector with the following elements "p0", "pa", "r1", "n1", "r", "n", "n1/n", "futility boundary", "EN(p0)", "EN(pa)", "PET(p0)", "PET(pa)", "P(reject H0 | p0)","P(reject H0 | pa)", "power loss", "alpha (no interim)"
calculate_twostage_OCs <- function(p0, pa, r1, n1, r, n){
  # corresponding futility boundary (if one would use the p-value of the exact binomial test instead of a threshold for the number of responses)
  # binom.test corresponds to 1- pbinom(r1-1, n1, p0)
  # stop if p-value at interim exact binomial test is greater than fut_bound
  fut_bound <- binom.test(r1, n1, p0, "greater")$p.value # p value of the exact binomial test of the true response rate being greater than p0
  # correctly stopped (under p0)
  pet_p0 <- pbinom(r1, n1, p0) # pbinom is the cumulative distribution function cdf
  # wrongly stopped (under pa) - very bad! and power is decreased
  pet_pa <- pbinom(r1, n1, pa)
  # type 1 error (overprotected when stopping for futility)
  i <- (r1+1):min(n1, r)
  err1 <- 1 - (pet_p0 + sum(dbinom(i, n1, p0)*pbinom(r-i, n-n1, p0)))
  # power (maybe compare with power without interim -> power loss), power is set for p=pa
  pow <- 1 - (pet_pa + sum(dbinom(i, n1, pa)*pbinom(r-i, n-n1, pa)))
  # what alpha would we have if we didn't do an interim analysis
  err1_noInterim <- 1 - pbinom(r, n, p0)
  # what power would we have if we didn't do an interim analysis
  pow_noInterim <- 1- pbinom(r, n, pa)
  # expected sample size under p0
  en_p0 <- n1 + (n-n1)*(1-pet_p0)
  # expected sample size unter pa (Mander 2012 - H0-optimal design rarely good when allowing stopping for efficacy)
  en_pa <- n1 + (n-n1)*(1-pet_pa)
  
  out <- c(p0, pa, r1, n1, r, n, n1/n, fut_bound, en_p0, en_pa, pet_p0, pet_pa, err1, pow, pow_noInterim-pow, err1_noInterim)
  names(out) <- c("p0", "pa", "r1", "n1", "r", "n", "n1/n", "futility boundary", "EN(p0)", "EN(pa)", "PET(p0)", "PET(pa)", "P(reject H0 | p0)","P(reject H0 | pa)", "power loss (wrt no interim)", "alpha (no interim)")
  out
}

#test
#calculate_twostage_OCs(0.5, 0.65, 19, 40, 41, 72)
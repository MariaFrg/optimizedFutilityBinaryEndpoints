source("calculate_twostage_OCs.R")

############### FUNCTIONS ##########################################

#' calculates Simon's designs for given p0, pa, alpha and beta with two options (minimax or optimal)
#' 
#' @param p0 Null hypothesis response probability
#' @param pa Alternative hypothesis response probability
#' @param alpha Type I error rate
#' @param beta Type II error rate
#' @param minimax indicates if parameters for simon's minimax design are calculated (TRUE) or for simon's optimal design (FALSE)
#' 
#' @return vector with the following elements "p0", "p1", "r1", "n1", "r", "n"
get_simon_design <- function(p0, pa, alpha, beta, minimax, nmax=200){
  if (alpha > 0.5 | alpha <= 0 | 1 - beta <= alpha | beta <= 0 | p0 <= 0 | p0 >= pa | pa >= 1)
    stop('invalid arguments')
  
  simon <- ph2simon(p0, pa, alpha, beta, nmax=200)
  x <- as.matrix(simon$out)
  
  # Simon minimax
  if(minimax == TRUE){
    x <- x[order(x[,4],decreasing = FALSE),]
  }
  
  # Simon optimal
  else{
    x <- x[order(x[,5],decreasing = FALSE),]
  }
  
  # output
  r1 <- x[1,1]
  n1 <- x[1,2]
  r <-  x[1,3]
  n <-  x[1,4]
  en0 <- x[1,5]
  pet0 <- x[1,6]
  out <- c(p0, pa, r1, n1, r, n)
  names(out) <- c("p0", "pa", "r1", "n1", "r", "n")
  out
}

#test
#get_simon_design(0.5, 0.65, 0.1, 0.1, TRUE)


#' determines the parameters r1,n1,r,n of Simon's designs for given values of p0,pa,alpha,beta and calculates the OC's of these designs
#' 
#' @param p0 Null hypothesis response probability
#' @param pa Alternative hypothesis response probability
#' @param alpha Type I error rate
#' @param beta Type II error rate
#' @param minimax indicates if parameters for simon's minimax design are calculates (TRUE) or for simon's optimal design (FALSE) 
#' 
#' @return vector with the following elements "p0", "pa", "r1", "n1", "r", "n", "n1/n", "futility boundary", "EN(p0)", "EN(pa)", "PET(p0)", "PET(pa)", "P(reject H0 | p0)","P(reject H0 | pa)", "power loss", "alpha (no interim)"
calculate_simonAndOCs <- function(p0, pa, alpha, beta, minimax = FALSE){
  simon <- get_simon_design(p0, pa, alpha=alpha, beta=beta, minimax = minimax)
  calculate_twostage_OCs(p0, pa, simon[3], simon[4], simon[5], simon[6])
}

#test
#calculate_simonAndOCs(0.5, 0.65, 0.1, 0.1, TRUE)


############### CALCULATIONS ##########################################

############### ORIGINAL SIMON OPTIMAL AND MINIMAX VALUES AND OCS ############################################################################
## delta 0.2 (difference between p0 and pa is always 0.2)
p0_vec <- seq(0.05, 0.75, by=0.05)
p1_vec <- seq(0.25, 0.95, by=0.05)

## OPTIMAL alpha 0.05, beta 0.2
list_opt_d2_a005_b02 <- mapply(function(x,y)calculate_simonAndOCs(x, y, alpha=0.05, beta=0.2, minimax = FALSE), p0_vec, p1_vec)
as.data.frame(t(list_opt_d2_a005_b02)) %>% write_xlsx("opt_d2_a005_b02.xlsx")
## MINIMAX
list_minimax_d2_a005_b02 <- mapply(function(x,y)calculate_simonAndOCs(x, y, alpha=0.05, beta=0.2, minimax = TRUE), p0_vec, p1_vec)
as.data.frame(t(list_minimax_d2_a005_b02)) %>% write_xlsx("minimax_d2_a005_b02.xlsx")

## OPTIMAL alpha 0.1, beta 0.1
list_opt_d2_a01_b01 <- mapply(function(x,y)calculate_simonAndOCs(x, y, alpha=0.1, beta=0.1, minimax = FALSE), p0_vec, p1_vec)
as.data.frame(t(list_opt_d2_a01_b01)) %>% write_xlsx("opt_d2_a01_b01.xlsx")
## MINIMAX
list_minimax_d2_a01_b01 <- mapply(function(x,y)calculate_simonAndOCs(x, y, alpha=0.1, beta=0.1, minimax = TRUE), p0_vec, p1_vec)
as.data.frame(t(list_minimax_d2_a01_b01)) %>% write_xlsx("minimax_d2_a01_b01.xlsx")

## OPTIMAL alpha 0.05, beta 0.1
list_opt_d2_a005_b01 <- mapply(function(x,y)calculate_simonAndOCs(x, y, alpha=0.05, beta=0.1, minimax = FALSE), p0_vec, p1_vec)
as.data.frame(t(list_opt_d2_a005_b01)) %>% write_xlsx("opt_d2_a005_b01.xlsx")
## MINIMAX
list_minimax_d2_a005_b01 <- mapply(function(x,y)calculate_simonAndOCs(x, y, alpha=0.05, beta=0.1, minimax = TRUE), p0_vec, p1_vec)
as.data.frame(t(list_minimax_d2_a005_b01)) %>% write_xlsx("minimax_d2_a005_b01.xlsx")


## delta 0.15
p0_vec <- seq(0.05, 0.8, by=0.05)
p1_vec <- seq(0.2, 0.95, by=0.05)

## OPTIMAL alpha 0.05, beta 0.2
list_opt_d15_a005_b02 <- mapply(function(x,y)calculate_simonAndOCs(x, y, alpha=0.05, beta=0.2, minimax = FALSE), p0_vec, p1_vec)
as.data.frame(t(list_opt_d15_a005_b02)) %>% write_xlsx("opt_d15_a005_b02.xlsx")
## MINIMAX
list_minimax_d15_a005_b02 <- mapply(function(x,y)calculate_simonAndOCs(x, y, alpha=0.05, beta=0.2, minimax = TRUE), p0_vec, p1_vec)
as.data.frame(t(list_minimax_d15_a005_b02)) %>% write_xlsx("minimax_d15_a005_b02.xlsx")

## OPTIMAL alpha 0.1, beta 0.1
list_opt_d15_a01_b01 <- mapply(function(x,y)calculate_simonAndOCs(x, y, alpha=0.1, beta=0.1, minimax = FALSE), p0_vec, p1_vec)
as.data.frame(t(list_opt_d15_a01_b01)) %>% write_xlsx("opt_d15_a01_b01.xlsx")
## MINIMAX
list_minimax_d15_a01_b01 <- mapply(function(x,y)calculate_simonAndOCs(x, y, alpha=0.1, beta=0.1, minimax = TRUE), p0_vec, p1_vec)
as.data.frame(t(list_minimax_d15_a01_b01)) %>% write_xlsx("minimax_d15_a01_b01.xlsx")

## OPTIMAL alpha 0.05, beta 0.1
list_opt_d15_a005_b01 <- mapply(function(x,y)calculate_simonAndOCs(x, y, alpha=0.05, beta=0.1, minimax = FALSE), p0_vec, p1_vec)
as.data.frame(t(list_opt_d15_a005_b01)) %>% write_xlsx("opt_d15_a005_b01.xlsx")
## MINIMAX
list_minimax_d15_a005_b01 <- mapply(function(x,y)calculate_simonAndOCs(x, y, alpha=0.05, beta=0.1, minimax = TRUE), p0_vec, p1_vec)
as.data.frame(t(list_minimax_d15_a005_b01)) %>% write_xlsx("minimax_d15_a005_b01.xlsx")
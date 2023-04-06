source("calculate_twostage_OCs.R")

############### MODIFIED SIMON OPTIMAL AND MINIMAX VALUES AND OCS ############################################################################
# all design specifications imported from Kim et al. (2020)

## delta 0.2
# import designs
# Kim's modified optimal designs for alpha = 0.1, beta = 0.1 and delta = 0.2
values_modopt_d2_a01_b01 <- matrix(c(0.05,0.25,0,9,2,24, 0.1,0.3,1,12,5,35, 0.15,0.35,3,19,7,33, 0.2,0.4,3,17,10,37, 0.25,0.45,3,15,13,40, 
                                     0.3,0.5,7,22,17,46, 0.35,0.55,7,20,20,47, 0.4,0.6,7,18,22,46, 0.45,0.65,9,20,24,45, 0.5,0.7,11,21,26,45, 
                                     0.55,0.75,10,18,26,41, 0.6,0.8,9,15,28,41, 0.65,0.85,10,15,25,34, 0.7,0.9,11,15,22,28, 0.75,0.95,6,8,16,19), ncol = 6, byrow = TRUE)

# Kim's modified minimax designs for alpha = 0.1, beta = 0.1 and delta = 0.2
values_modminimax_d2_a01_b01 <- matrix(c(0.05,0.25,0,13,2,20, 0.1,0.3,1,16,4,25, 0.15,0.35,2,17,7,32, 0.2,0.4,3,19,10,36, 0.25,0.45,5,23,13,39, 
                                         0.3,0.5,6,26,15,39, 0.35,0.55,7,21,19,44, 0.4,0.6,7,21,20,41, 0.45,0.65,9,21,22,41, 0.5,0.7,11,23,23,39, 
                                         0.55,0.75,13,23,25,39, 0.6,0.8,9,16,25,36, 0.65,0.85,8,13,23,31, 0.7,0.9,11,16,20,25, 0.75,0.95,6,8,16,19), ncol = 6, byrow = TRUE)

# Kim's modified optimal designs for alpha = 0.05, beta = 0.2 and delta = 0.2
values_modopt_d2_a005_b02 <- matrix(c(0.05,0.25,0,9,2,17, 0.1,0.3,1,12,5,26, 0.15,0.35,2,15,7,28, 0.2,0.4,4,18,10,33, 0.25,0.45,4,16,14,39, 
                                      0.3,0.5,4,14,17,42, 0.35,0.55,8,21,18,39, 0.4,0.6,7,17,21,41, 0.45,0.65,8,17,23,41, 0.5,0.7,8,16,24,39, 
                                      0.55,0.75,7,13,26,39, 0.6,0.8,8,13,25,35, 0.65,0.85,7,11,24,31, 0.7,0.9,8,11,23,28, 0.75,0.95,8,10,20,23), ncol = 6, byrow = TRUE)

# Kim's modified optimal designs for alpha = 0.05, beta = 0.2 and delta = 0.2
values_modminimax_d2_a005_b02 <- matrix(c(0.05,0.25,0,9,2,17, 0.1,0.3,1,15,5,25, 0.15,0.35,2,15,7,28, 0.2,0.4,4,18,10,33, 0.25,0.45,4,17,13,36,
                                          0.3,0.5,6,19,16,39, 0.35,0.55,8,21,18,39, 0.4,0.6,7,17,21,41, 0.45,0.65,8,17,23,41, 0.5,0.7,12,23,23,37, 
                                          0.55,0.75,11,20,25,37, 0.6,0.8,8,13,25,35, 0.65,0.85,7,11,24,31, 0.7,0.9,8,11,23,28, 0.75,0.95,8,10,20,23), ncol = 6, byrow = TRUE)

# Kim's modified optimal designs for alpha = 0.05, beta = 0.1 and delta = 0.2
values_modopt_d2_a005_b01 <- matrix(c(0.05,0.25,0,10,3,28, 0.1,0.3,2,18,6,35, 0.15,0.35,3,19,10,44, 0.2,0.4,4,19,15,54, 0.25,0.45,6,22,19,57, 
                                      0.3,0.5,8,24,24,63, 0.35,0.55,7,20,26,59, 0.4,0.6,11,25,32,66, 0.45,0.65,11,23,33,61, 0.5,0.7,13,24,36,61,
                                      0.55,0.75,10,18,35,54, 0.6,0.8,12,19,37,53, 0.65,0.85,10,15,33,44, 0.7,0.9,11,15,29,36, 0.75,0.95,12,15,24,28), ncol = 6, byrow = TRUE)

# modified optimal designs for alpha = 0.05, beta = 0.1 and delta = 0.2
values_modminimax_d2_a005_b01 <- matrix(c(0.05,0.25,0,15,3,25, 0.1,0.3,2,22,6,33, 0.15,0.35,3,23,9,38, 0.2,0.4,5,24,13,45, 0.25,0.45,6,26,17,49, 
                                          0.3,0.5,7,24,21,53, 0.35,0.55,10,34,24,53, 0.4,0.6,12,29,27,54, 0.45,0.65,14,31,30,54, 0.5,0.7,14,27,32,53,
                                          0.55,0.75,15,28,33,50, 0.6,0.8,15,26,32,45, 0.65,0.85,12,18,31,41, 0.7,0.9,13,18,26,32, 0.75,0.95,12,15,24,28), ncol = 6, byrow = TRUE)

# calculate OCs
## OPTIMAL alpha 0.1, beta 0.1 
list_modopt_d2_a01_b01 <- mapply(function(a,b,c,d,e,f) calculate_twostage_OCs(a,b,c,d,e,f), values_modopt_d2_a01_b01[,1], values_modopt_d2_a01_b01[,2], values_modopt_d2_a01_b01[,3], values_modopt_d2_a01_b01[,4], values_modopt_d2_a01_b01[,5], values_modopt_d2_a01_b01[,6])
as.data.frame(t(list_modopt_d2_a01_b01)) %>% write_xlsx("modopt_d2_a01_b01.xlsx")
## MINIMAX 
list_modminimax_d2_a01_b01 <- mapply(function(a,b,c,d,e,f) calculate_twostage_OCs(a,b,c,d,e,f), values_modminimax_d2_a01_b01[,1], values_modminimax_d2_a01_b01[,2], values_modminimax_d2_a01_b01[,3], values_modminimax_d2_a01_b01[,4], values_modminimax_d2_a01_b01[,5], values_modminimax_d2_a01_b01[,6])
as.data.frame(t(list_modminimax_d2_a01_b01)) %>% write_xlsx("modminimax_d2_a01_b01.xlsx")

## OPTIMAL alpha 0.05, beta 0.2 
list_modopt_d2_a005_b02 <- mapply(function(a,b,c,d,e,f) calculate_twostage_OCs(a,b,c,d,e,f), values_modopt_d2_a005_b02[,1], values_modopt_d2_a005_b02[,2], values_modopt_d2_a005_b02[,3], values_modopt_d2_a005_b02[,4], values_modopt_d2_a005_b02[,5], values_modopt_d2_a005_b02[,6])
as.data.frame(t(list_modopt_d2_a005_b02)) %>% write_xlsx("modopt_d2_a005_b02.xlsx")
## MINIMAX 
list_modminimax_d2_a005_b02 <- mapply(function(a,b,c,d,e,f) calculate_twostage_OCs(a,b,c,d,e,f), values_modminimax_d2_a005_b02[,1], values_modminimax_d2_a005_b02[,2], values_modminimax_d2_a005_b02[,3], values_modminimax_d2_a005_b02[,4], values_modminimax_d2_a005_b02[,5], values_modminimax_d2_a005_b02[,6])
as.data.frame(t(list_modminimax_d2_a005_b02)) %>% write_xlsx("modminimax_d2_a005_b02.xlsx")

## OPTIMAL alpha 0.05, beta 0.1 
list_modopt_d2_a005_b01 <- mapply(function(a,b,c,d,e,f) calculate_twostage_OCs(a,b,c,d,e,f), values_modopt_d2_a005_b01[,1], values_modopt_d2_a005_b01[,2], values_modopt_d2_a005_b01[,3], values_modopt_d2_a005_b01[,4], values_modopt_d2_a005_b01[,5], values_modopt_d2_a005_b01[,6])
as.data.frame(t(list_modopt_d2_a005_b01)) %>% write_xlsx("modopt_d2_a005_b01.xlsx")
## MINIMAX 
list_modminimax_d2_a005_b01 <- mapply(function(a,b,c,d,e,f) calculate_twostage_OCs(a,b,c,d,e,f), values_modminimax_d2_a005_b01[,1], values_modminimax_d2_a005_b01[,2], values_modminimax_d2_a005_b01[,3], values_modminimax_d2_a005_b01[,4], values_modminimax_d2_a005_b01[,5], values_modminimax_d2_a005_b01[,6])
as.data.frame(t(list_modminimax_d2_a005_b01)) %>% write_xlsx("modminimax_d2_a005_b01.xlsx")

## delta 0.15
# import designs
# modified optimal designs for alpha = 0.1, beta = 0.1 and delta = 0.15
values_modopt_d15_a01_b01 <- matrix(c(0.05,0.2,1,19,3,38, 0.1,0.25,2,21,7,50, 0.15,0.3,3,23,11,55, 0.2,0.35,5,27,16,63, 0.25,0.4,7,29,22,72, 
                                      0.3,0.45,9,30,29,82, 0.35,0.5,12,34,33,81, 0.4,0.55,16,38,40,88, 0.45,0.6,14,32,40,78, 0.5,0.65,18,35,47,84,
                                      0.55,0.7,19,34,46,75, 0.6,0.75,21,34,47,71, 0.65,0.8,20,30,45,63, 0.7,0.85,14,20,45,59, 0.75,0.9,12,16,39,48, 
                                      0.8,0.95,13,16,27,31), ncol = 6, byrow = TRUE)

# modified minimax designs for alpha = 0.1, beta = 0.1 and delta = 0.15
values_modminimax_d15_a01_b01 <- matrix(c(0.05,0.2,0,18,3,32, 0.1,0.25,1,23,6,40, 0.15,0.3,5,34,11,53, 0.2,0.35,6,33,15,58, 0.25,0.4,9,39,20,64, 
                                          0.3,0.45,13,45,26,71, 0.35,0.5,14,43,30,72, 0.4,0.55,18,45,34,73, 0.45,0.6,22,50,39,75, 0.5,0.65,19,40,41,72, 
                                          0.55,0.7,16,31,44,71, 0.6,0.75,22,39,43,64, 0.65,0.8,22,33,43,60, 0.7,0.85,15,22,40,52, 0.75,0.9,19,26,33,40, 
                                          0.8,0.95,13,16,27,31), ncol = 6, byrow = TRUE)

# modified optimal designs for alpha = 0.05, beta = 0.2 and delta = 0.15
values_modopt_d15_a005_b02 <- matrix(c(0.05,0.2,0,11,3,28, 0.1,0.25,1,15,7,41, 0.15,0.3,3,21,11,49, 0.2,0.35,4,21,17,61, 0.25,0.4,6,24,22,67, 
                                       0.3,0.45,7,24,26,68, 0.35,0.5,10,28,32,74, 0.4,0.55,12,29,38,79, 0.45,0.6,14,30,43,80, 0.5,0.65,15,29,44,75, 
                                       0.55,0.7,14,25,47,74, 0.6,0.75,14,23,46,67, 0.65,0.8,16,24,45,61, 0.7,0.85,16,22,41,52, 0.75,0.9,14,18,37,44, 
                                       0.8,0.95,13,16,27,30), ncol = 6, byrow = TRUE)

# modified minimax designs for alpha = 0.05, beta = 0.2 and delta = 0.15
values_modminimax_d15_a005_b02 <- matrix(c(0.05,0.2,0,13,3,27, 0.1,0.25,2,22,7,40, 0.15,0.3,3,23,11,48, 0.2,0.35,6,31,15,53, 0.25,0.4,10,40,21,62, 
                                           0.3,0.45,9,31,26,67, 0.35,0.5,14,42,30,68, 0.4,0.55,15,40,35,71, 0.45,0.6,19,42,38,70, 0.5,0.65,20,41,41,69, 
                                           0.55,0.7,20,35,43,67, 0.6,0.75,18,30,43,62, 0.65,0.8,20,31,41,55, 0.7,0.85,16,23,39,49, 0.75,0.9,17,22,33,39, 
                                           0.8,0.95,13,16,27,30), ncol = 6, byrow = TRUE)

# modified optimal designs for alpha = 0.05, beta = 0.1 and delta = 0.15
values_modopt_d15_a005_b01 <- matrix(c(0.05,0.2,1,21,4,41, 0.1,0.25,3,28,9,57, 0.15,0.3,5,30,17,82, 0.2,0.35,8,37,22,83, 0.25,0.4,10,37,31,99, 
                                       0.3,0.45,13,40,40,110, 0.35,0.5,16,43,44,105, 0.4,0.55,19,45,49,104, 0.45,0.6,19,40,60,116, 0.5,0.65,22,42,60,105, 
                                       0.55,0.7,22,38,68,110, 0.6,0.75,21,34,64,95, 0.65,0.8,21,31,67,93, 0.7,0.85,23,31,63,82, 0.75,0.9,18,23,52,63, 
                                       0.8,0.95,16,19,37,42), ncol = 6, byrow = TRUE)

# modified minimax designs for alpha = 0.05, beta = 0.1 and delta = 0.15
values_modminimax_d15_a005_b01 <- matrix(c(0.05,0.2,0,23,4,38, 0.1,0.25,3,31,9,55, 0.15,0.3,6,42,14,64, 0.2,0.35,8,42,21,77, 0.25,0.4,11,52,27,83, 
                                           0.3,0.45,14,46,34,91, 0.35,0.5,16,46,40,94, 0.4,0.55,24,62,45,94, 0.45,0.6,26,58,52,98, 0.5,0.65,28,57,54,93, 
                                           0.55,0.7,26,47,58,92, 0.6,0.75,29,48,58,85, 0.65,0.8,29,46,55,75, 0.7,0.85,33,44,53,68, 0.75,0.9,19,25,45,54, 
                                           0.8,0.95,16,19,37,42), ncol = 6, byrow = TRUE)

# calculate OCs
## OPTIMAL alpha 0.1, beta 0.1 
list_modopt_d15_a01_b01 <- mapply(function(a,b,c,d,e,f) calculate_twostage_OCs(a,b,c,d,e,f), values_modopt_d15_a01_b01[,1], values_modopt_d15_a01_b01[,2], values_modopt_d15_a01_b01[,3], values_modopt_d15_a01_b01[,4], values_modopt_d15_a01_b01[,5], values_modopt_d15_a01_b01[,6])
as.data.frame(t(list_modopt_d15_a01_b01)) %>% write_xlsx("modopt_d15_a01_b01.xlsx")
## MINIMAX 
list_modminimax_d15_a01_b01 <- mapply(function(a,b,c,d,e,f) calculate_twostage_OCs(a,b,c,d,e,f), values_modminimax_d15_a01_b01[,1], values_modminimax_d15_a01_b01[,2], values_modminimax_d15_a01_b01[,3], values_modminimax_d15_a01_b01[,4], values_modminimax_d15_a01_b01[,5], values_modminimax_d15_a01_b01[,6])
as.data.frame(t(list_modminimax_d15_a01_b01)) %>% write_xlsx("modminimax_d15_a01_b01.xlsx")

## OPTIMAL alpha 0.05, beta 0.2 
list_modopt_d15_a005_b02 <- mapply(function(a,b,c,d,e,f) calculate_twostage_OCs(a,b,c,d,e,f), values_modopt_d15_a005_b02[,1], values_modopt_d15_a005_b02[,2], values_modopt_d15_a005_b02[,3], values_modopt_d15_a005_b02[,4], values_modopt_d15_a005_b02[,5], values_modopt_d15_a005_b02[,6])
as.data.frame(t(list_modopt_d15_a005_b02)) %>% write_xlsx("modopt_d15_a005_b02.xlsx")
## MINIMAX 
list_modminimax_d15_a005_b02 <- mapply(function(a,b,c,d,e,f) calculate_twostage_OCs(a,b,c,d,e,f), values_modminimax_d15_a005_b02[,1], values_modminimax_d15_a005_b02[,2], values_modminimax_d15_a005_b02[,3], values_modminimax_d15_a005_b02[,4], values_modminimax_d15_a005_b02[,5], values_modminimax_d15_a005_b02[,6])
as.data.frame(t(list_modminimax_d15_a005_b02)) %>% write_xlsx("modminimax_d15_a005_b02.xlsx")

## OPTIMAL alpha 0.05, beta 0.1 
list_modopt_d15_a005_b01 <- mapply(function(a,b,c,d,e,f) calculate_twostage_OCs(a,b,c,d,e,f), values_modopt_d15_a005_b01[,1], values_modopt_d15_a005_b01[,2], values_modopt_d15_a005_b01[,3], values_modopt_d15_a005_b01[,4], values_modopt_d15_a005_b01[,5], values_modopt_d15_a005_b01[,6])
as.data.frame(t(list_modopt_d15_a005_b01)) %>% write_xlsx("modopt_d15_a005_b01.xlsx")
## MINIMAX 
list_modminimax_d15_a005_b01 <- mapply(function(a,b,c,d,e,f) calculate_twostage_OCs(a,b,c,d,e,f), values_modminimax_d15_a005_b01[,1], values_modminimax_d15_a005_b01[,2], values_modminimax_d15_a005_b01[,3], values_modminimax_d15_a005_b01[,4], values_modminimax_d15_a005_b01[,5], values_modminimax_d15_a005_b01[,6])
as.data.frame(t(list_modminimax_d15_a005_b01)) %>% write_xlsx("modminimax_d15_a005_b01.xlsx")


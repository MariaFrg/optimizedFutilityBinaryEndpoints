source("calculate_twostage_OCs.R")
source("search_opt_fut_boundary.R")


############### CALCULATIONS ##########################################

############### OPTIMIZED DESIGN VALUES AND OCS ############################################################################
## delta 0.2 (difference between p0 and pa is always 0.2)
p0_vec <- seq(0.1, 0.75, by=0.05)
p1_vec <- seq(0.3, 0.95, by=0.05)

## alpha 0.05, beta 0.2, power loss 0.05, pi_wrong 0.05
list_modopt_d2_a005_b02_pl005_pw005 <- mapply(function(x,y)calculate_optBoundaryOCs(x, y, pi_wrong=0.05, power_loss=0.05, alpha=0.05, beta=0.2), p0_vec, p1_vec)
as.data.frame(t(list_modopt_d2_a005_b02_pl005_pw005)) %>% write_xlsx("modopt_d2_a005_b02_pl005_pw005.xlsx")

## alpha 0.05, beta 0.2, power loss 0.05, pi_wrong 0.025
list_modopt_d2_a005_b02_pl005_pw0025 <- mapply(function(x,y)calculate_optBoundaryOCs(x, y, pi_wrong=0.025, power_loss=0.05, alpha=0.05, beta=0.2), p0_vec, p1_vec)
as.data.frame(t(list_modopt_d2_a005_b02_pl005_pw0025)) %>% write_xlsx("modopt_d2_a005_b02_pl005_pw0025.xlsx")

## alpha 0.05, beta 0.2, power loss 0.01, pi_wrong 0.01
list_modopt_d2_a005_b02_pl001_pw001 <- mapply(function(x,y)calculate_optBoundaryOCs(x, y, pi_wrong=0.01, power_loss=0.01, alpha=0.05, beta=0.2), p0_vec, p1_vec)
as.data.frame(t(list_modopt_d2_a005_b02_pl001_pw001)) %>% write_xlsx("modopt_d2_a005_b02_pl001_pw001.xlsx")


## alpha 0.1, beta 0.1, power loss 0.05, pi_wrong 0.05
list_modopt_d2_a01_b01_pl005_pw005 <- mapply(function(x,y)calculate_optBoundaryOCs(x, y, pi_wrong=0.05, power_loss=0.05, alpha=0.1, beta=0.1), p0_vec, p1_vec)
as.data.frame(t(list_modopt_d2_a01_b01_pl005_pw005)) %>% write_xlsx("modopt_d2_a01_b01_pl005_pw005.xlsx")

## alpha 0.1, beta 0.1, power loss 0.05, pi_wrong 0.025
list_modopt_d2_a01_b01_pl005_pw0025 <- mapply(function(x,y)calculate_optBoundaryOCs(x, y, pi_wrong=0.025, power_loss=0.05, alpha=0.1, beta=0.1), p0_vec, p1_vec)
as.data.frame(t(list_modopt_d2_a01_b01_pl005_pw0025)) %>% write_xlsx("modopt_d2_a01_b01_pl005_pw0025.xlsx")

## alpha 0.1, beta 0.1, power loss 0.01, pi_wrong 0.01
list_modopt_d2_a01_b01_pl001_pw001 <- mapply(function(x,y)calculate_optBoundaryOCs(x, y, pi_wrong=0.01, power_loss=0.01, alpha=0.1, beta=0.1), p0_vec, p1_vec)
as.data.frame(t(list_modopt_d2_a01_b01_pl001_pw001)) %>% write_xlsx("modopt_d2_a01_b01_pl001_pw001.xlsx")


## alpha 0.05, beta 0.1, power loss 0.05, pi_wrong 0.05
list_modopt_d2_a005_b01_pl005_pw005 <- mapply(function(x,y)calculate_optBoundaryOCs(x, y, pi_wrong=0.05, power_loss=0.05, alpha=0.05, beta=0.1), p0_vec, p1_vec)
as.data.frame(t(list_modopt_d2_a005_b01_pl005_pw005)) %>% write_xlsx("modopt_d2_a005_b01_pl005_pw005.xlsx")

## alpha 0.05, beta 0.1, power loss 0.05, pi_wrong 0.025
list_modopt_d2_a005_b01_pl005_pw0025 <- mapply(function(x,y)calculate_optBoundaryOCs(x, y, pi_wrong=0.025, power_loss=0.05, alpha=0.05, beta=0.1), p0_vec, p1_vec)
as.data.frame(t(list_modopt_d2_a005_b01_pl005_pw0025)) %>% write_xlsx("modopt_d2_a005_b01_pl005_pw0025.xlsx")

## alpha 0.05, beta 0.1, power loss 0.01, pi_wrong 0.01
list_modopt_d2_a005_b01_pl001_pw001 <- mapply(function(x,y)calculate_optBoundaryOCs(x, y, pi_wrong=0.01, power_loss=0.01, alpha=0.05, beta=0.1), p0_vec, p1_vec)
as.data.frame(t(list_modopt_d2_a005_b01_pl001_pw001)) %>% write_xlsx("modopt_d2_a005_b01_pl001_pw001.xlsx")


## delta 0.15
p0_vec <- seq(0.05, 0.8, by=0.05)
p1_vec <- seq(0.2, 0.95, by=0.05)

## alpha 0.05, beta 0.2, power loss 0.05, pi_wrong 0.05
list_modopt_d15_a005_b02_pl005_pw005 <- mapply(function(x,y)calculate_optBoundaryOCs(x, y, pi_wrong=0.05, power_loss=0.05, alpha=0.05, beta=0.2), p0_vec, p1_vec)
as.data.frame(t(list_modopt_d15_a005_b02_pl005_pw005)) %>% write_xlsx("modopt_d15_a005_b02_pl005_pw005.xlsx")

## alpha 0.05, beta 0.2, power loss 0.05, pi_wrong 0.025
list_modopt_d15_a005_b02_pl005_pw0025 <- mapply(function(x,y)calculate_optBoundaryOCs(x, y, pi_wrong=0.025, power_loss=0.05, alpha=0.05, beta=0.2), p0_vec, p1_vec)
as.data.frame(t(list_modopt_d15_a005_b02_pl005_pw0025)) %>% write_xlsx("modopt_d15_a005_b02_pl005_pw0025.xlsx")

## alpha 0.05, beta 0.2, power loss 0.01, pi_wrong 0.01
list_modopt_d15_a005_b02_pl001_pw001 <- mapply(function(x,y)calculate_optBoundaryOCs(x, y, pi_wrong=0.01, power_loss=0.01, alpha=0.05, beta=0.2), p0_vec, p1_vec)
as.data.frame(t(list_modopt_d15_a005_b02_pl001_pw001)) %>% write_xlsx("modopt_d15_a005_b02_pl001_pw001.xlsx")


## alpha 0.1, beta 0.1, power loss 0.05, pi_wrong 0.05
list_modopt_d15_a01_b01_pl005_pw005 <- mapply(function(x,y)calculate_optBoundaryOCs(x, y, pi_wrong=0.05, power_loss=0.05, alpha=0.1, beta=0.1), p0_vec, p1_vec)
as.data.frame(t(list_modopt_d15_a01_b01_pl005_pw005)) %>% write_xlsx("modopt_d15_a01_b01_pl005_pw005.xlsx")

## alpha 0.1, beta 0.1, power loss 0.05, pi_wrong 0.025
list_modopt_d15_a01_b01_pl005_pw0025 <- mapply(function(x,y)calculate_optBoundaryOCs(x, y, pi_wrong=0.025, power_loss=0.05, alpha=0.1, beta=0.1), p0_vec, p1_vec)
as.data.frame(t(list_modopt_d15_a01_b01_pl005_pw0025)) %>% write_xlsx("modopt_d15_a01_b01_pl005_pw0025.xlsx")

## alpha 0.1, beta 0.1, power loss 0.01, pi_wrong 0.01
list_modopt_d15_a01_b01_pl001_pw001 <- mapply(function(x,y)calculate_optBoundaryOCs(x, y, pi_wrong=0.01, power_loss=0.01, alpha=0.1, beta=0.1), p0_vec, p1_vec)
as.data.frame(t(list_modopt_d15_a01_b01_pl001_pw001)) %>% write_xlsx("modopt_d15_a01_b01_pl001_pw001.xlsx")


## alpha 0.05, beta 0.1, power loss 0.05, pi_wrong 0.05
list_modopt_d15_a005_b01_pl005_pw005 <- mapply(function(x,y)calculate_optBoundaryOCs(x, y, pi_wrong=0.05, power_loss=0.05, alpha=0.05, beta=0.1), p0_vec, p1_vec)
as.data.frame(t(list_modopt_d15_a005_b01_pl005_pw005)) %>% write_xlsx("modopt_d15_a005_b01_pl005_pw005.xlsx")

## alpha 0.05, beta 0.1, power loss 0.05, pi_wrong 0.025
list_modopt_d15_a005_b01_pl005_pw0025 <- mapply(function(x,y)calculate_optBoundaryOCs(x, y, pi_wrong=0.025, power_loss=0.05, alpha=0.05, beta=0.1), p0_vec, p1_vec)
as.data.frame(t(list_modopt_d15_a005_b01_pl005_pw0025)) %>% write_xlsx("modopt_d15_a005_b01_pl005_pw0025.xlsx")

## alpha 0.05, beta 0.1, power loss 0.01, pi_wrong 0.01
list_modopt_d15_a005_b01_pl001_pw001 <- mapply(function(x,y)calculate_optBoundaryOCs(x, y, pi_wrong=0.01, power_loss=0.01, alpha=0.05, beta=0.1), p0_vec, p1_vec)
as.data.frame(t(list_modopt_d15_a005_b01_pl001_pw001)) %>% write_xlsx("modopt_d15_a005_b01_pl001_pw001.xlsx")
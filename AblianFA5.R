#6
#a
P_error_given_S1 <- 0.01
P_error_given_S2 <- 0.02
P_error_given_S3 <- 0.015
P_S1 <- 0.4
P_S2 <- 0.25
P_S3 <- 0.35

P_error <- P_error_given_S1 * P_S1 + P_error_given_S2 * P_S2 + P_error_given_S3 * P_S3
P_error
#b
P_no_error <- 1 - P_error
P_no_error

#c
P_no_error_given_S1 <- 1 - P_error_given_S1
P_S1_given_no_error <- (P_no_error_given_S1 * P_S1) / P_no_error
P_S1_given_no_error
#9
#a
P_not_upgrade <- 0.20
P_not_upgrade
#b
P_buy_given_upgrade <- 0.40
P_buy_given_upgrade
#c
# P(G), P(B|G), P(B|G), P(C|G), P(C|G)
P_G <- 1 - P_buy_given_upgrade
P_B_given_G <- P_buy_given_upgrade
P_B_given_not_G <- 0.70
P_C_given_G <- 0.20
P_C_given_not_G <- 1 - P_not_upgrade - P_buy_given_upgrade

P_G
P_B_given_G
P_B_given_not_G
P_C_given_G
P_C_given_not_G

#13
#a
P_internet <- 0.70
P_email <- 0.30
P_infect_given_internet <- 0.60
P_infect_given_email <- 0.80

P_infect <- P_internet * P_infect_given_internet + P_email * P_infect_given_email
P_infect
#b
P_internet_given_detected <- (P_infect_given_internet * P_internet) / P_infect
P_internet_given_detected



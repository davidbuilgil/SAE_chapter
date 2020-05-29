########################################################
#                                                      #
# Small Area Estimation for Crime Analysis             #
#                                                      #
# Imputing Covariates' Missing Data                    #
#                                                      #
# David Buil-Gil                                       #
#                                                      #
# 29 May 2020                                          #
#                                                      #
########################################################

rm(list = ls())

# Load packages required.
packages <- c("dplyr", "RCurl", "Hmisc")

lapply(packages, require, character.only = TRUE)

# Load all possible covariates.
url_covs <- getURL("https://raw.githubusercontent.com/davidbuilgil/SAE_chapter/master/data/covs.csv")

covs <- read.csv(text = url_covs)

# Check number of missing values per variable.
covs %>%
  summarise_all(funs(sum(is.na(.))))

# Fit multiple imputation model via Bootstrapping and Predictive Mean Matching.
fun_imput <- aregImpute(~ fem_p_16   + popdens_16  + netmig_r_16 + gdp_eurhab_16 + 
                          pps_hab_16 + loun_ths_16 + unra1524_16 + unraall_16    +
                          hcide_r_10 + robb_r_10   + burg_r_10   + vthft_r_10    +
                          he_p_16    + medage_16, 
                        data = covs, n.impute = 10)

# Check R Squared with which each missing variable could be predicted from the others. 
fun_imput$rsq
# Not bad at all!

# Replace missing data with the mean of the multiple imputations for each case.
imputed <- as.data.frame(impute.transcan(fun_imput,       imputation = 1, 
                                         rhsImp = "mean", data = covs, 
                                         list.out = T))

# Replace imputed values in main dataset.
covs <- covs %>%
  dplyr::select(domain) %>%
  cbind(imputed)

# Select covariates for SAE.
covs_short <- covs %>%
  dplyr::select(domain, fem_p_16, gdp_eurhab_16, robb_r_10, burg_r_10, he_p_16, medage_16)

# Save all imputed covariates.
write.csv(covs, "data/covs_imp.csv")

# Save selected imputed covariates.
write.csv(covs_short, "data/covs_short_imp.csv")


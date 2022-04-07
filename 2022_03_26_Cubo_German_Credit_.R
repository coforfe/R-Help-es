

#-------------------- Library Loading --------------
library(sampling)    # To use samplecube function - slow way
library(scorecard)   # To get germancredit data.
library(dplyr)       # Data wrangling.
library(janitor)     # To clean names.
library(recipes)     # To apply some transformations.

#------ Some checks.
#-- Target Distribution
table(germancredit$creditability)

# bad good 
# 300  700 

#------ Let's sample 300 rows from the "good" class.
#-- For demonstration purposes we will use just a subset of columns.
#-- Some numeric columns and some of the character columns.

german_good <- germancredit %>%
                clean_names() %>%
                filter(creditability == "good") %>%
                select( duration_in_month, credit_amount, age_in_years, 
                        status_of_existing_checking_account, credit_history, 
                        savings_account_and_bonds, foreign_worker, creditability) 

german_good_tr <- recipe( creditability ~ . , data = german_good) %>%
                          step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
                          prep() %>% bake( new_data = NULL) %>%
                          select(-creditability) %>%
                          clean_names()

#---- Preparation for samplecube
target_df <- as.data.frame(table(germancredit$creditability))
perc_minority <- min(target_df$Freq) / max(target_df$Freq)
prob_minority <- rep(perc_minority, nrow(german_good_tr)) 

german_good_matrix  <- as.matrix(german_good_tr)

#------------------------ SLOW APPROACH --------------
#---- Cube sampling gets back row indexes to sample.
s_slow <- samplecube( X = german_good_matrix, pik = prob_minority, order = 2, comment = FALSE, method = 1)

#---- Select subsample
german_slow <- german_good[ s_slow, ]


#--- Final sampled data.frame 
german_sampled_slow <- rbind(
                        german_slow,
                        germancredit %>% filter(creditability == "bad") %>%
                           clean_names() %>%
                           select( duration_in_month, credit_amount, age_in_years, 
                                   status_of_existing_checking_account, credit_history, 
                                   savings_account_and_bonds, foreign_worker, creditability) 
)




#--------------------- FAST APPROACH -------------
#--------------------- By using fast "BalancedSampling".
library(BalancedSampling)

#--- "cube" function gets back row indexes to sample.
s_fast <- cube( p = prob_minority, X = german_good_matrix)

#---- Select subsample
german_fast <- german_good[ s_fast, ]

#--- Final sampled data.frame 
german_sampled_fast <- rbind(
  german_fast,
  germancredit %>% filter(creditability == "bad") %>%
    clean_names() %>%
    select( duration_in_month, credit_amount, age_in_years, 
            status_of_existing_checking_account, credit_history, 
            savings_account_and_bonds, foreign_worker, creditability) 
)


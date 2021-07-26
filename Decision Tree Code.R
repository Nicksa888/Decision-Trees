#########################
#########################
#### Clear Workspace ####
#########################
#########################

rm(list = ls()) 
# clear global environment to remove all loaded data sets, functions and so on.

###################
###################
#### Libraries ####
###################
###################

library(easypackages) # enables the libraries function
suppressPackageStartupMessages(
  libraries("tidyverse",
            "dplyr",
            "rpart",
            "rpart.plot",
            "caret" # for confusionMatrix()
            ))

########################
########################
# Working Directory ####
########################
########################

setwd("C:/R Portfolio/Decision Trees/Data")

##################
##################
# Obtain Data ####
##################
##################

permits <- read_csv("permits.csv", col_types = "ffffffnnnnfffff")

glimpse(permits)

permits <- permits %>%
  mutate_at(c('valuation', 'floorArea', 'numberUnits', 'stories'), ~ifelse(.< 1, NA, .))

# Fix the outliers issue.
permits <- permits %>%
  mutate(stories = ifelse(stories > 73, NA, stories))

summary(dplyr::select(permits, valuation, floorArea, numberUnits, stories))

# Select only the first four features to build the decision tree.
permits <- permits %>%
  dplyr::select(
    permitType,
    permitSubtype,
    initiatingOffice,
    permitCategory
  )

# Using the sample() function, let's create our training and test datasets with a 80% to 20% split.
# The set.seed() function is used to ensure that we can get the same result every time we run a random sampling process.
set.seed(1234)
sample_set <- sample(nrow(permits), round(nrow(permits)*.80), replace = FALSE)
permits_train <- permits[sample_set, ]
permits_test <- permits[-sample_set, ]

# Check the proportions for the class between all 3 sets.
round(prop.table(table(dplyr::select(permits, permitCategory))),2)
round(prop.table(table(dplyr::select(permits_train, permitCategory))),2)
round(prop.table(table(dplyr::select(permits_test, permitCategory))),2)

# Build the classification tree with permitcategory as the outcome variable and all other variables as predictors.
permits_mod <-
  rpart(
    permitCategory ~ .,
    method = "class",
    data = permits_train
  )

rpart.plot(permits_mod)
rpart.rules(permits_mod) # indicates the rules and percentage probability of each rule
rpart

####################
# Plot Description #
####################

# In the plot, the coloured boxes indicate the nodes, while the black lines indicate the branches.
# The plot consists of nodes and branches that one can logically take in a journey of classifying previously unclassified data.
# The root node is at the very top. It indicates that 33% of the data at ths partition will be flagged for further review. As this probability is less than 50%, then the node is labeled as 'No Plan Check'. The 100% informs us that this partition contains 100% of the data.
# If the left hand route is followed, the outcome is that where the permit type is categorised as either plumbing, electrical, HVAC or Pressure Vessel, then probability of further review being flagged for a new application is just 10%, and this outcome comprises 57% of the data.

##############
# Prediction #
##############

#### Compute evaluation metrics ####

compute.eval.metrics <- function(cmatrix) {
  TP <- cmatrix[1, 1] # true positive
  TN <- cmatrix[2, 2] # true negative
  FP <- cmatrix[2, 1] # false positive
  FN <- cmatrix[1, 2] # false negative
  acc = sum(diag(cmatrix)) / sum(cmatrix)
  misclass = 1 - sum(diag(cmatrix)) / sum(cmatrix)
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  specificity <- TN / (TN + FP)
  F1 <- 2 * precision * recall / (precision + recall)
  c(Accuracy = acc, 
    Misclassification = misclass, 
    Precision = precision,  
    Recall = recall, 
    Specificity = specificity, 
    F1 = F1)
}  

permits_pred <- predict(permits_mod, permits_test, type = "class")

tab1 <- table(Predicted = permits_pred, Actual = permits_test$permitCategory)
tab1

round(compute.eval.metrics(tab1) * 100, 2)

####################
####################
# Regression Trees #
####################
####################

# Regression Trees can be used to solve regression problems (problems with numeric outcomes)
# In such trees, values of each leaf node are based on the average of the output values

###############
# Import Data #
###############

# This data 

income <- read_csv("income.csv", col_types = "nffnfffffnff")

####################################
# Data Exploration and Preparation #
####################################

glimpse(income)
summary(income)

set.seed(123)

# sample data in a 75%/25% split to create training and test data sets
sample_set <- sample(nrow(income), round(nrow(income) * .75), replace = F)

# decision trees are not adversely affected by missing data, so we dont need to to deal with them. We also need not be concerned with outliers, noise or normalization.

income_train <- income[sample_set, ]
income_test <- income[-sample_set, ]

# Determine the class distribution #
# Check the proportions for the class between all 3 datasets #
round(prop.table(table(dplyr::select(income, income), exclude = NULL)), 4) * 100
round(prop.table(table(dplyr::select(income_train, income), exclude = NULL)), 4) * 100
round(prop.table(table(dplyr::select(income_test, income), exclude = NULL)), 4) * 100

#######################
# Construct the Model #
#######################

# Build the classification tree with income as the outcome variable and all other variables as predictors.

income_mod <-
  rpart(
    income ~ .,
    method = "class",
    data = income_train
  )
rpart.plot(income_mod)
rpart.rules(income_mod)

##############
# Prediction #
##############

#### Compute evaluation metrics ####

compute.eval.metrics <- function(cmatrix) {
  TP <- cmatrix[1, 1] # true positive
  TN <- cmatrix[2, 2] # true negative
  FP <- cmatrix[2, 1] # false positive
  FN <- cmatrix[1, 2] # false negative
  acc = sum(diag(cmatrix)) / sum(cmatrix)
  misclass = 1 - sum(diag(cmatrix)) / sum(cmatrix)
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  specificity <- TN / (TN + FP)
  F1 <- 2 * precision * recall / (precision + recall)
  c(Accuracy = acc, 
    Misclassification = misclass, 
    Precision = precision,  
    Recall = recall, 
    Specificity = specificity, 
    F1 = F1)
}  

income_pred <- predict(income_mod, income_test, type = "class")

tab1 <- table(Predicted = income_pred, Actual = income_test$income)
tab1

round(compute.eval.metrics(tab1) * 100, 2)
confusionMatrix(Predicted = income_pred, Actual = income_test$income)

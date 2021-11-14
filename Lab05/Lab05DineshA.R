# Aadhithya Dinesh
# MIS 545 Section 02
# Lab05DineshA.R
# Import a dataset of spending by groups of people visiting a zoo and generate 
# a multiple linear regression model by assigning data types, building a model
# and testing for model fit.

# install.packages("tidyverse")
# install.packages("dummies")
# install.packages("corrplot")
# install.packages("olsrr")

library("tidyverse")
library("corrplot")
library("olsrr")

# set the working directory
setwd("~/MIS/Classes/MIS545/Assignments/Lab05")

# read the csv file with column types specified
zooSpending <- read_csv(file = "ZooVisitSpending.csv",
                       col_types = "niil",
                       col_names = TRUE)

# print the zooSpending tibble
print(zooSpending)

#print the structure of zooSpending
str(zooSpending)

#print the summary of zooSpending
print(summary(zooSpending))

# define the function to display all histograms
displayAllHistograms <- function(tibbleDataset) {
  tibbleDataset %>%
    keep(is.numeric) %>%
    gather() %>%
    ggplot() + geom_histogram(mapping = aes(x=value, fill=key),
                              color = "black") + 
    facet_wrap (~ key, scales = "free") + 
    theme_minimal()
}

# call the function
displayAllHistograms(zooSpending)
# rounding the correlation to 2 decimal places
print(round(cor(zooSpending  %>% keep(is.numeric)),2))

# displaying the correlation plot using number method
corrplot(cor(zooSpending), 
         method = "number",
         type = "lower")

# generating the linear regression model by using all the independent variables
zooSpendingModel <- lm(data = zooSpending,
                       formula = VisitSpending ~ .)

# displaying the beta coefficients for the model
print(zooSpendingModel)

# displaying the linear regression model results using the summary function
print(summary(zooSpendingModel))

# testing for multicollinearity
print(ols_vif_tol(zooSpendingModel))

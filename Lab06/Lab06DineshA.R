# Aadhithya Dinesh
# MIS 545 Section 02
# Lab05DineshA.R
# Import a dataset of mobile phone plan subscribers and generate a multiple 
# logistic regression model that will predict if a customer will 
# cancel their contract based on a number of different factors. 

# install.packages("tidyverse")
#install.packages("e1071")
# install.packages("dummies")
# install.packages("corrplot")
# install.packages("olsrr")
# install.packages("smotefamily")


library("tidyverse")
library("corrplot")
library("olsrr")
library("smotefamily")

# set the working directory
setwd("~/MIS/Classes/MIS545/Assignments/Lab06")

mobilePhone <- read_csv(file = "MobilePhoneSubscribers.csv",
                        col_types = "lillnininn",
                        col_names = TRUE)


# print the mobilePhone tibble
print(mobilePhone)

#print the structure of mobilePhone
str(mobilePhone)

#print the summary of mobilePhone
print(summary(mobilePhone))

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
displayAllHistograms(mobilePhone)
# rounding the correlation to 2 decimal places
print(round(cor(mobilePhone  %>% keep(is.numeric)),2))

# displaying the correlation plot using number method
corrplot(cor(mobilePhone), 
         method = "number",
         type = "lower")

mobilePhone <- select(mobilePhone, -c(DataPlan, DataUsage))

set.seed(203)
sampleSet <- sample(nrow(mobilePhone),
                    round(nrow(mobilePhone)*0.75),
                    replace = FALSE)
mobilePhoneTraining <- mobilePhone[sampleSet, ]
mobilePhoneTesting <- mobilePhone[-sampleSet, ]

classImbalanceMagnitude  <- 299/76

mobilePhoneTrainingSmoted <-
  tibble(SMOTE(X = data.frame(mobilePhoneTraining),
               target = mobilePhoneTraining$CancelledService,
               dup_size = 3)$data)

print(summary(mobilePhoneTrainingSmoted))
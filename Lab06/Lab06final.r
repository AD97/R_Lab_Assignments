# Aadhithya Dinesh
# MIS 545 Section 02
# Lab05DineshA.R
# Import a dataset of mobile phone plan subscribers and generate a multiple 
# logistic regression model that will predict if a customer will 
# cancel their contract based on a number of different factors. 

#install.packages ("tidyverse")
#library (tidyverse)
#install.packages ('corrplot')
#library (corrplot)
#install.packages("olsrr")
#library (olsrr)
#install.packages("smotefamily")
#library (smotefamily)

# Set the working directory 
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

# random dataset with 203 as seed
set.seed(203)

# creating a vector of 75% ramdomly sampled rows
sampleSet<- sample (nrow(mobilePhone),
                    round(nrow(mobilePhone)*0.75),
                    replace= FALSE)

# assign 75% to mobilePhoneTraining
mobilePhoneTraining<- mobilePhone[sampleSet, ]

# assign 25% to mobilePhoneTesting
mobilePhoneTesting<- mobilePhone[-sampleSet, ]

# checking imbalance
print(summary(mobilePhoneTraining$CancelledService))

# storing the magnitude of imbalance
classImbalanceMagnitude<- 1256/357

# dealing with class imbalance using SMOTE technique
mobilePhoneTrainingSmoted<- 
  tibble(SMOTE(X=mobilePhoneTraining,
               target=mobilePhoneTraining$CancelledService,
               dup_size = 3)$ data)

print(summary(mobilePhoneTrainingSmoted))

# converting CancelledService and RecentRenewal back into logical types
mobilePhoneTrainingSmoted<- mobilePhoneTrainingSmoted %>%
  mutate(CancelledService= as.logical(CancelledService),
         RecentRenewal= as.logical(RecentRenewal))

# deleting "class" column
mobilePhoneTrainingSmoted<- mobilePhoneTrainingSmoted %>%
  select(-class)

summary(mobilePhoneTrainingSmoted)

# generating the logistic regression model 
mobilePhoneModel<- glm(data= mobilePhoneTrainingSmoted,
                       family=binomial,
                       formula= CancelledService ~ . )

# displaying the logistic regression model summary
summary(mobilePhoneModel)

# odds ratios for the 7 independent variable coefficients
exp(coef(mobilePhoneModel)["AccountWeeks"])
exp(coef(mobilePhoneModel)["RecentRenewalTRUE"])
exp(coef(mobilePhoneModel)["CustServCalls"])
exp(coef(mobilePhoneModel)["AvgCallMinsPerMonth"])
exp(coef(mobilePhoneModel)["AvgCallsPerMonth"])
exp(coef(mobilePhoneModel)["MonthlyBill"])
exp(coef(mobilePhoneModel)["OverageFee"])

# predicted outcomes
mobilePhonePrediction<- predict(mobilePhoneModel,
                                mobilePhoneTesting,
                                type="response")
# display mobilePhonePrediction
print(mobilePhonePrediction)

# treating anything below or equal to 0.5 as a 0, anything above 0.5 as a 1
mobilePhonePrediction<- 
  ifelse(mobilePhonePrediction >= 0.5, 1,0)

# displaying mobilePhonePrediction
print(mobilePhonePrediction)

# creating confusion matrix
mobilePhoneConfusionMatrix<- table(mobilePhoneTesting$CancelledService,
                                   mobilePhonePrediction)

# displaying mobilePhoneConfusionMatrix
print(mobilePhoneConfusionMatrix)

# calculating false positive
mobilePhoneConfusionMatrix[1,2]/
  (mobilePhoneConfusionMatrix[1,2]+
     mobilePhoneConfusionMatrix[1,1])

# calculating false negative
mobilePhoneConfusionMatrix[2,1]/
  (mobilePhoneConfusionMatrix[2,1]+
     mobilePhoneConfusionMatrix[2,2])

# Calculating model prediction accuracy
sum(diag(mobilePhoneConfusionMatrix))/ nrow(mobilePhoneTesting)
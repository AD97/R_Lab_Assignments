# Aadhithya Dinesh
# MIS 545 Section 02
# Lab12DineshA.R
# To import a dataset of people and generate a neural network model that will 
# predict if a fisher used a chartered boat service based on their 
# fishing catch rate and their annual income
# We will be importing csv files, assigning data types, 
# building a supervised neural network model, and testing for model fit.

# install.packages("tidyverse")
# install.packages("neuralnet")

library(tidyverse)
library(neuralnet)
library(factoextra)
library(cluster)
library(gridExtra)

# set the working directory
setwd("~/MIS/Classes/MIS545/Assignments/Lab12")

fishingCharter <- read_csv(file = "FishingCharter.csv",
                      col_types = "lnn",
                      col_names = TRUE)

# print the fishingCharter tibble
print(fishingCharter)

# print the structure of fishingCharter
print(str(fishingCharter))

# print the summary of fishingCharter
print(summary(fishingCharter))

# scaling the annual income to a value between 0 and 1
fishingCharter <- fishingCharter %>%
  mutate(AnnualIncomeScaled = (AnnualIncome - min(AnnualIncome))/
           (max(AnnualIncome) - min(AnnualIncome)))

# scaling the catch rate to a value between 0 and 1
fishingCharter <- fishingCharter %>%
  mutate(CatchRateScaled = (CatchRate - min(CatchRate))/
           (max(CatchRate) - min(CatchRate)))

# set the seed to 591
set.seed(591)
sampleSet <- sample(nrow(fishingCharter),
                    round(nrow(fishingCharter)*0.75),
                    replace = FALSE)

# splitting into 75% training dataset
fishingCharterTraining <- fishingCharter[sampleSet, ]

# loading the remaining 25% of the dataset for testing
fishingCharterTesting <- fishingCharter[-sampleSet, ]

# generating the neural network
fishingCharterNeuralNet <- neuralnet(
  formula = CharteredBoat ~ CatchRateScaled + AnnualIncomeScaled,
  data = fishingCharterTraining,
  hidden = 3,
  act.fct = "logistic",
  linear.output = FALSE)

# displaying the neural network results
print(fishingCharterNeuralNet$result.matrix)

# using fishingCharterProbability to generate probablities on the testing dataset
fishingCharterProbability <- compute(fishingCharterNeuralNet, 
                                     fishingCharterTesting)

# visualizing the neural network
plot(fishingCharterNeuralNet)

# displaying the results from the testing dataset on the console
print(fishingCharterProbability$net.result)

# converting probability predictions into 0 or 1 predictions
fishingCharterPrediction <- 
  ifelse(fishingCharterProbability$net.result > 0.5, 1, 0)

# displaying the predictions on the console
print(fishingCharterPrediction)

# evaluating the model by forming a confusion matrix
fishingCharterConfusionMatrix <- table(fishingCharterTesting$CharteredBoat,
                                       fishingCharterPrediction)

# displaying confusion matrix on the console
print(fishingCharterConfusionMatrix)

# calculating model predictive accuracy
predictiveAccuracy <- sum(diag(fishingCharterConfusionMatrix)) /
  nrow(fishingCharterTesting)

# displaying the predictive accuracy
print(predictiveAccuracy)





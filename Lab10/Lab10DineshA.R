# Aadhithya Dinesh
# MIS 545 Section 02
# Lab10DineshA.R
# to import a dataset of Instacart grocery transactions and generate 
# association rules among items in a transaction. We will be importing csv files, 
# assigning data types, generating strong association rules, 
# and interpreting association rules.

# install.packages("tidyverse")
# install.packages("arules")

library(tidyverse)
library(arules)

# set the working directory
setwd("~/MIS/Classes/MIS545/Assignments/Lab10")

instacartTransactions <- read.transactions(file = "InstacartTransactions.csv",
                                  format = "single",
                                  header = TRUE,
                                  sep = ",",
                                  cols = c("OrderID", "ItemID")
                                  )

# print the riceFarms tibble
print(instacartTransactions)

# print the structure of riceFarms
print(str(instacartTransactions))

# print the summary of riceFarms
print(summary(instacartTransactions))

# print the first three transactions
inspect(instacartTransactions[1:3])

# print the frequency of 24852 (bananas)
itemFrequency(instacartTransactions[, "24852"])

# convert the frequency values into a tibble
instacartTransactionsFrequency <- 
  tibble(Items = names(itemFrequency(instacartTransactions)),
         Frequency = itemFrequency(instacartTransactions))

# display the item frequencies in the console
print(instacartTransactionsFrequency)

# display the ten most frequently occurring items
instacartTransactionsFrequency %>%
  arrange(desc(Frequency)) %>%
  slice(1:10)

# generating the association rules for instacart online transactions
instacartTransactionRules <-
  apriori(instacartTransactions,
          parameter = list(
            support = 0.005,
            confidence = 0.2,
            minlen = 2))

# display the summary of online transaction rules
print(summary(instacartTransactionRules))

# display the first 10 association rules
inspect(instacartTransactionRules[1:10])

# sort the association rules by lift and view the top 10
instacartTransactionRules %>%
  sort(by = "lift") %>%
  head(n = 10) %>%
  inspect()

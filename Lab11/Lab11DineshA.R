# Aadhithya Dinesh
# MIS 545 Section 02
# Lab11DineshA.R
# To import a dataset of country-level data and generate clusters using the 
# k-means clustering method.
# We will be importing csv files, assigning data types, generating clusters, 
# and interpreting clusters.

# install.packages("tidyverse")
# install.packages("factoextra")

library(tidyverse)
library(stats)
library(factoextra)
library(cluster)
library(gridExtra)

# set the working directory
setwd("~/MIS/Classes/MIS545/Assignments/Lab11")

countries <- read_csv(file = "CountryData.csv",
                                  col_types = "cnnnnini",
                                  col_names = TRUE)

# print the countries tibble
print(countries)

# print the structure of countries
print(str(countries))

# print the summary of countries
print(summary(countries))

# Converting the column containing the country name to the row title of the tibble 
# (this is a requirement for later visualizing the clusters)
countries <- countries %>% column_to_rownames(var = "Country")

# removing countries with missing data in any feature
countries <- countries %>% drop_na()

# print the summary of countries again to ensure no NA values are present
print(summary(countries))

# scaling both features in the tibble so they have equal impact
countriesScaled <- countries %>%
  select(CorruptionIndex, DaysToOpenBusiness) %>% scale()

# set the seed to 679
set.seed(679)

# generating the k-means cluster
countries4Clusters <- kmeans(x = countriesScaled,
                             centers = 4,
                             nstart = 25)

# display cluster sizes
print(countries4Clusters$size)

# display cluster centers (z-scores)
print(countries4Clusters$centers)

# visualize the clusters
fviz_cluster(object = countries4Clusters,
             data = countriesScaled,
             repel = FALSE)

# optimizing the value for k using the methods below

# elbow method
fviz_nbclust(x = countriesScaled,
             FUNcluster = kmeans,
             method = "wss")

# average silhouette method
fviz_nbclust(x = countriesScaled,
             FUNcluster = kmeans,
             method = "silhouette")

# gap statistic method
fviz_nbclust(x = countriesScaled,
             FUNcluster = kmeans,
             method = "gap_stat")

# regenerating the analysis using 3 as the optimal number of clusters
countries3Clusters <- kmeans(x = countriesScaled,
                             centers = 3,
                             nstart = 25)

# display cluster sizes
print(countries3Clusters$size)

# display cluster centers (z-scores)
print(countries3Clusters$centers)

# visualize the clusters
fviz_cluster(object = countries3Clusters,
             data = countriesScaled,
             repel = FALSE)

# determining similarities and differences among all the features
countries %>%
  mutate(cluster = countries3Clusters$cluster) %>%
  select(cluster,
         CorruptionIndex, 
         CompulsoryEducationYears,
         GiniCoefficient,
         GDPPerCapita,
         EduPercGovSpend,
         EduPercGDP,
         CompulsoryEducationYears,
         DaysToOpenBusiness) %>%
  group_by(cluster) %>%
  summarise_all("mean")
  

# packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(readr)
library(forcats)
library(purrr)
library(tibble)
library(corrplot)


# Read data
train <- read_csv('data/train.csv')

# Count missing values
missing.count <- colSums(sapply(train, is.na))
missing.count <- missing.count[missing.count > 0]
missing.count <- as_tibble(data.frame(column = names(missing.count),
                                      count = missing.count,
                                      row.names = NULL))

missing.count <-
  missing.count %>%
  mutate(percent_missing = round(count / nrow(missing.count))) %>%
  arrange(desc(count))

missing.count
create_none <- function(colName) {
  train[colName] <<- ifelse(is.na(train[colName]),
                            "None",
                            train[colName])
} 

# Remove id, garage year built, set masvnrarea to zero where null and remove observation with electrical as NA
train <- 
  train %>%
  select(-c(GarageYrBlt, Id)) %>%
  mutate(MasVnrArea = ifelse(is.na(MasVnrArea),
                             0,
                             MasVnrArea)) %>%
  filter(!is.na(Electrical)) 

train %>%
  select(c(LotArea, LotFrontage)) %>%
  na.omit %>%
  cor

train %>%
  select(c(LotArea, LotFrontage)) %>%
  na.omit %>%
  mutate(LotArea = sqrt(LotArea)) %>%
  cor

train <-
  train %>%
  mutate(LotFrontage = ifelse(is.na(LotFrontage),
                              sqrt(LotArea),
                              LotFrontage))

train[is.na(train)] <- "None"


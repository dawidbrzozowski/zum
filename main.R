# Title     : TODO
# Objective : TODO
# Created by: dawid
# Created on: 1/18/2022

# install.packages("groupdata2")
# install.packages("dplyr")
# install.packages("plyr")

library(plyr)
library(groupdata2)
library(dplyr)


datadf = read.csv("data/train.csv")

encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x <- x - 1
  x
}

preprocess_data <- function(data) {
  data <- subset(data, select=-c(X, id))
  data[["Gender"]] <- encode_ordinal(data[["Gender"]])
  data[["Customer.Type"]] <- encode_ordinal(data[["Customer.Type"]])
  data[["Type.of.Travel"]] <- encode_ordinal(data[["Type.of.Travel"]])
  data[["Class"]] <- encode_ordinal(data[["Class"]], order = c("Eco", "Eco Plus", "Business"))
  data[["satisfaction"]] <- encode_ordinal(data[["satisfaction"]], order = c("neutral or dissatisfied", "satisfied"))
  
  return(data)
}

create_anomaly_set <- function(data, percent) {

  data_not_pos_cnt <- nrow(subset(data, satisfaction == "neutral or dissatisfied"))
  anomaly_cnt <- data_not_pos_cnt * percent / (1 - percent)

  data_pos_cnt <- nrow(data) - data_not_pos_cnt
  anomaly_to_remove <- data_pos_cnt - anomaly_cnt
  anomaly_to_remove <- as.integer(anomaly_to_remove)

  print(data_pos_cnt)
  data <- data[-sample(which(data$satisfaction=="satisfied"), anomaly_to_remove),]
  return(data)
}

data_df <- create_anomaly_set(datadf, 0.05)

datadf <- preprocess_data(datadf)

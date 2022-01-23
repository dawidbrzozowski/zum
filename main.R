# Title     : TODO
# Objective : TODO
# Created by: dawid
# Created on: 1/18/2022

library(plyr)
library(groupdata2)
library(dplyr)
library(randomForest)

train_datadf = read.csv("data/train.csv")
test_datadf = read.csv("data/test.csv")

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
  data$Arrival.Delay.in.Minutes[is.na(data$Arrival.Delay.in.Minutes)] <- 0
  
  
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

# train_datadf <- create_anomaly_set(train_datadf, 0.05)

train_datadf <- preprocess_data(train_datadf)
test_datadf <- preprocess_data(test_datadf)

train_datadf$satisfaction <- as.factor(train_datadf$satisfaction)
test_datadf$satisfaction <- as.factor(test_datadf$satisfaction)

train_datadf_y <- train_datadf$satisfaction
test_datadf_y <- test_datadf$satisfaction

train_datadf_x <- subset(train_datadf, select=-c(satisfaction))
test_datadf_x <- subset(test_datadf, select=-c(satisfaction))

model <- randomForest(train_datadf_x, train_datadf_y, xtest=test_datadf_x, ytest=test_datadf_y)
tuneRF(model)
print("Finished")

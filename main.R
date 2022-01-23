# Title     : TODO
# Objective : TODO
# Created by: dawid
# Created on: 1/18/2022

library(plyr)
library(groupdata2)
library(dplyr)
library(randomForest)
library(ggplot2)

train_datadf = read.csv("/Users/robertostoja-lniski/Projekty/ZUM/zum/data/train.csv")
test_datadf = read.csv("/Users/robertostoja-lniski/Projekty/ZUM/zum/data/test.csv")

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

train_datadf <- create_anomaly_set(train_datadf, 0.05)

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


# isolation forest
iforest <- isolationForest$new()
iforest$fit(dataset = train_datadf_x)

# predict outliers within dataset
pred <- iforest$predict(test_datadf_x)
min_anomaly_score <- min(pred$anomaly_score)
max_anomaly_score <- max(pred$anomaly_score)
print(min_anomaly_score)
print(max_anomaly_score)
print(pred)

anomaly_scores <- pred$anomaly_score
comparison <- pred$anomaly_score
comparison_df <- ldply(comparison, data.frame)
colnames(comparison_df)[1] <- "anomaly_score"
print(comparison_df)

comparison_df$actual <- test_datadf_y
print(comparison_df)

print(typeof(anomaly_scores))
scores$actual <- test_datadf_y

hist <- comparison_df %>%
  ggplot( aes(x=anomaly_scores, fill=actual)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  labs(fill="")

hist

library(readxl)
library(dplyr) 
library(neuralnet)
library(Metrics)


uow_consumption <- read_excel("/Users/thushanivasanthan/Downloads/Machine CW/uow_consumption.xlsx")
View(uow_consumption)

colnames(uow_consumption) <- c("Date", "Six", "Seven", "Eight")
View(uow_consumption)

twentyColumn <- c(uow_consumption$`Eight`)

# These are the time delayed lag using t+1(as the predicted), 

# t     as the current (today)    - column 1
# t-1   as previous               - column 2
# t-2   as two previous           - column 3
# t-3   as three previous         - column 4
# t-4   as four previous          - column 5
# t-7   as the one week previous  - column 8

# the one to predict is the whole of the 20th column 
# Due to t being the first; it will be column 1 

twentyColumnlagged <- bind_cols(t_7 = lag(twentyColumn,8),
                                t_4 = lag(twentyColumn,5),
                                t_3 = lag(twentyColumn,4),
                                t_2 = lag(twentyColumn,3),
                                t_1 = lag(twentyColumn,2),
                                t = lag(twentyColumn,1),
                                twentyHour = twentyColumn)          
twentyColumnlagged 

#To get rid of the N/A data
trainOrigin <- twentyColumnlagged[1:380,]
trainOrigin <- trainOrigin[complete.cases(trainOrigin),]
testOrigin <- twentyColumnlagged[381:470,]
View(testOrigin)

#Finding the min and max in order to de-normalise the data later on
twentyTrainOrigin_min <- min(trainOrigin)
twentyTrainOrigin_max <- max(trainOrigin)


#Normalization function
normalise <- function(x) 
{return((x - min(x)) / (max(x) - min(x)))}

testNormalise  <- function(x) 
{ return((x - twentyTrainOrigin_min) / (twentyTrainOrigin_max - twentyTrainOrigin_min))}

#Normalizing the data     - so it is in the same range for the neural network
#one value shouldn't dominate the other.
#Turned into data-frame
twentyTrain_Normalisation <- normalise(trainOrigin)
twentyTest_Normalisation  <- testNormalise(testOrigin)
View(twentyTest_Normalisation)
View(twentyTrain_Normalisation)

#nn1 and nn2
#Creating a neural network
set.seed(11111)
Model1 <- neuralnet(twentyHour ~ t + t_7 , hidden = 1, data = twentyTrain_Normalisation, act.fct = "tanh")
Model2 <- neuralnet(twentyHour ~ t + t_7 , hidden = c(1,2), data = twentyTrain_Normalisation, act.fct = "logistic")

#De-normalize function
unnormalise <- function(x, min, max) {return( (max - min)*x + min )}

#The model's result by testing it on test data 
#Test data is normalized
predicted_TwentyColumn1 <- predict(Model1, twentyTest_Normalisation [, c("t" , "t_7")])   
predicted_TwentyColumn2 <- predict(Model2, twentyTest_Normalisation [, c("t" , "t_7")]) 

#De-normalize the result prediction of the model
#Using the original not normalized data's min and max - to de-normalize
#Min and max is of the original training data-set that was created
model01 <- unnormalise(predicted_TwentyColumn1, twentyTrainOrigin_min, twentyTrainOrigin_max)
model02 <- unnormalise(predicted_TwentyColumn2, twentyTrainOrigin_min, twentyTrainOrigin_max)

#Calculating accuracy measures
#Comparing it to the prediction of the neural network
rmse(testOrigin$twentyHour, model01)
mae(testOrigin$twentyHour, model01)
mape(testOrigin$twentyHour, model01)*100
smape(testOrigin$twentyHour, model01)*100
plot(Model1)

rmse(testOrigin$twentyHour, model02)
mae(testOrigin$twentyHour, model02)
mape(testOrigin$twentyHour, model02)*100
smape(testOrigin$twentyHour, model02)*100
plot(Model2)


#nn3, nn4 and nn5
#Creating a neural network
set.seed(11111)
Model3 <- neuralnet(twentyHour ~ t + t_1 , hidden = 1, data = twentyTrain_Normalisation, linear.output=TRUE)
Model4 <- neuralnet(twentyHour ~ t + t_1 , hidden = 2, data = twentyTrain_Normalisation, act.fct = "tanh")
Model5 <- neuralnet(twentyHour ~ t + t_1 , hidden = c(1,2), data = twentyTrain_Normalisation, act.fct = "logistic")

#De-normalize function
unnormalise <- function(x, min, max) {return( (max - min)*x + min )}

#The model's result by testing it on test data 
#Test data is normalized
predicted_TwentyColumn3 <- predict(Model3, twentyTest_Normalisation [, c("t" ,"t_1")])   
predicted_TwentyColumn4 <- predict(Model4, twentyTest_Normalisation [, c("t" ,"t_1")]) 
predicted_TwentyColumn5 <- predict(Model5, twentyTest_Normalisation [, c("t" ,"t_1")]) 

#De-normalize the result prediction of the model
#Using the original not normalized data's min and max - to de-normalize
#Min and max is of the original training data-set that was created
model03 <- unnormalise(predicted_TwentyColumn3, twentyTrainOrigin_min, twentyTrainOrigin_max)
model04 <- unnormalise(predicted_TwentyColumn4, twentyTrainOrigin_min, twentyTrainOrigin_max)
model05 <- unnormalise(predicted_TwentyColumn5, twentyTrainOrigin_min, twentyTrainOrigin_max)

#Calculating accuracy measures
#Comparing it to the prediction of the neural network
rmse(testOrigin$twentyHour, model03)
mae(testOrigin$twentyHour, model03)
mape(testOrigin$twentyHour, model03)*100
smape(testOrigin$twentyHour, model03)*100
plot(Model3)

rmse(testOrigin$twentyHour, model04)
mae(testOrigin$twentyHour, model04)
mape(testOrigin$twentyHour, model04)*100
smape(testOrigin$twentyHour, model04)*100
plot(Model4)

rmse(testOrigin$twentyHour, model05)
mae(testOrigin$twentyHour, model05)
mape(testOrigin$twentyHour, model05)*100
smape(testOrigin$twentyHour, model05)*100
plot(Model5)


#nn6, nn7 and nn8
#Creating a neural network
set.seed(11111)
Model6 <- neuralnet(twentyHour ~ t + t_1 + t_2 , hidden = 1, data = twentyTrain_Normalisation, act.fct = "logistic")
Model7 <- neuralnet(twentyHour ~ t + t_1 + t_2 , hidden = 2, data = twentyTrain_Normalisation, act.fct = "tanh")
Model8 <- neuralnet(twentyHour ~ t + t_1 + t_2 , hidden = c(2,1), data = twentyTrain_Normalisation, linear.output=FALSE)

#De-normalize function
unnormalise <- function(x, min, max) {return( (max - min)*x + min )}

#The model's result by testing it on test data 
#Test data is normalized
predicted_TwentyColumn6 <- predict(Model6, twentyTest_Normalisation [, c("t" ,"t_1", "t_2")])   
predicted_TwentyColumn7 <- predict(Model7, twentyTest_Normalisation [, c("t" ,"t_1", "t_2")]) 
predicted_TwentyColumn8 <- predict(Model8, twentyTest_Normalisation [, c("t" ,"t_1", "t_2")]) 

#De-normalize the result prediction of the model
#Using the original not normalized data's min and max - to de-normalize
#Min and max is of the original training data-set that was created
model06 <- unnormalise(predicted_TwentyColumn6, twentyTrainOrigin_min, twentyTrainOrigin_max)
model07 <- unnormalise(predicted_TwentyColumn7, twentyTrainOrigin_min, twentyTrainOrigin_max)
model08 <- unnormalise(predicted_TwentyColumn8, twentyTrainOrigin_min, twentyTrainOrigin_max)

#Calculating accuracy measures
#Comparing it to the prediction of the neural network
rmse(testOrigin$twentyHour, model06)
mae(testOrigin$twentyHour, model06)
mape(testOrigin$twentyHour, model06)*100
smape(testOrigin$twentyHour, model06)*100
plot(Model6)

rmse(testOrigin$twentyHour, model07)
mae(testOrigin$twentyHour, model07)
mape(testOrigin$twentyHour, model07)*100
smape(testOrigin$twentyHour, model07)*100
plot(Model7)

rmse(testOrigin$twentyHour, model08)
mae(testOrigin$twentyHour, model08)
mape(testOrigin$twentyHour, model08)*100
smape(testOrigin$twentyHour, model08)*100
plot(Model8)


#nn9, nn10 and nn11
#Creating a neural network
set.seed(11111)
Model9 <- neuralnet(twentyHour ~ t + t_1 + t_2 + t_3 , hidden = 1, data = twentyTrain_Normalisation, linear.output=FALSE)
Model10 <- neuralnet(twentyHour ~ t + t_1 + t_2 + t_3 , hidden = 2, data = twentyTrain_Normalisation, linear.output=TRUE)
Model11 <- neuralnet(twentyHour ~ t + t_1 + t_2 + t_3 , hidden = c(2,2), data = twentyTrain_Normalisation, act.fct = "tanh")

#De-normalize function
unnormalise <- function(x, min, max) {return( (max - min)*x + min )}

#The model's result by testing it on test data 
#Test data is normalized
predicted_TwentyColumn9 <- predict(Model9, twentyTest_Normalisation [, c("t" ,"t_1", "t_2", "t_3")])   
predicted_TwentyColumn10 <- predict(Model10 , twentyTest_Normalisation [, c("t" ,"t_1", "t_2", "t_3")]) 
predicted_TwentyColumn11 <- predict(Model11 , twentyTest_Normalisation [, c("t" ,"t_1", "t_2", "t_3")]) 

#De-normalize the result prediction of the model
#Using the original not normalized data's min and max - to de-normalize
#Min and max is of the original training data-set that was created
model09 <- unnormalise(predicted_TwentyColumn9, twentyTrainOrigin_min, twentyTrainOrigin_max)
model010 <- unnormalise(predicted_TwentyColumn10, twentyTrainOrigin_min, twentyTrainOrigin_max)
model011 <- unnormalise(predicted_TwentyColumn11, twentyTrainOrigin_min, twentyTrainOrigin_max)

#Calculating accuracy measures
#Comparing it to the prediction of the neural network
rmse(testOrigin$twentyHour, model09)
mae(testOrigin$twentyHour, model09)
mape(testOrigin$twentyHour, model09)*100
smape(testOrigin$twentyHour, model09)*100
plot(Model9)

rmse(testOrigin$twentyHour, model010)
mae(testOrigin$twentyHour, model010)
mape(testOrigin$twentyHour, model010)*100
smape(testOrigin$twentyHour, model010)*100
plot(Model10)

rmse(testOrigin$twentyHour, model011)
mae(testOrigin$twentyHour, model011)
mape(testOrigin$twentyHour, model011)*100
smape(testOrigin$twentyHour, model011)*100
plot(Model11)


#nn12 and nn13
#Creating a neural network
set.seed(11111)
Model12 <- neuralnet(twentyHour ~ t + t_1 + t_2 + t_3 + t_4  , hidden = 1, data = twentyTrain_Normalisation, act.fct = "tanh")
Model13 <- neuralnet(twentyHour ~ t + t_1 + t_2 + t_3 + t_4  , hidden = 2, data = twentyTrain_Normalisation, linear.output=TRUE)

#De-normalize function
unnormalise <- function(x, min, max) {return( (max - min)*x + min )}

#The model's result by testing it on test data 
#Test data is normalized
predicted_TwentyColumn12 <- predict(Model12, twentyTest_Normalisation [, c("t","t_1","t_2", "t_3","t_4")])   
predicted_TwentyColumn13 <- predict(Model13 , twentyTest_Normalisation [, c("t","t_1","t_2", "t_3","t_4")]) 

#De-normalize the result prediction of the model
#Using the original not normalized data's min and max - to de-normalize
#Min and max is of the original training data-set that was created
model012 <- unnormalise(predicted_TwentyColumn12, twentyTrainOrigin_min, twentyTrainOrigin_max)
model013 <- unnormalise(predicted_TwentyColumn13, twentyTrainOrigin_min, twentyTrainOrigin_max)

#Calculating accuracy measures
#Comparing it to the prediction of the neural network
rmse(testOrigin$twentyHour, model012)
mae(testOrigin$twentyHour, model012)
mape(testOrigin$twentyHour, model012)*100
smape(testOrigin$twentyHour, model012)*100
plot(Model12)

rmse(testOrigin$twentyHour, model013)
mae(testOrigin$twentyHour, model013)
mape(testOrigin$twentyHour, model013)*100
smape(testOrigin$twentyHour, model013)*100
plot(Model13)


#nn14 and nn15
#Creating a neural network
set.seed(11111)
Model14 <- neuralnet(twentyHour ~ t + t_1 + t_2 + t_3 + t_4 + t_7 , hidden = c(1,1) , data = twentyTrain_Normalisation, act.fct = "logistic")
Model15 <- neuralnet(twentyHour ~ t + t_1 + t_2 + t_3 + t_4 + t_7  , hidden = c(2,2) , data = twentyTrain_Normalisation, linear.output = FALSE)

#De-normalize function
unnormalise <- function(x, min, max) {return( (max - min)*x + min )}

#The model's result by testing it on test data 
#Test data is normalized
predicted_TwentyColumn14 <- predict(Model14, twentyTest_Normalisation [, c("t","t_1","t_2", "t_3","t_4", "t_7")])   
predicted_TwentyColumn15  <- predict(Model15 , twentyTest_Normalisation [, c("t","t_1","t_2", "t_3","t_4", "t_7")]) 

#De-normalize the result prediction of the model
#Using the original not normalized data's min and max - to de-normalize
#Min and max is of the original training data-set that was created
model014 <- unnormalise(predicted_TwentyColumn14, twentyTrainOrigin_min, twentyTrainOrigin_max)
model015 <- unnormalise(predicted_TwentyColumn15, twentyTrainOrigin_min, twentyTrainOrigin_max)

#Calculating accuracy measures
#Comparing it to the prediction of the neural network
rmse(testOrigin$twentyHour, model014)
mae(testOrigin$twentyHour, model014)
mape(testOrigin$twentyHour, model014)*100
smape(testOrigin$twentyHour, model014)*100
plot(Model14)

rmse(testOrigin$twentyHour, model015)
mae(testOrigin$twentyHour, model015)
mape(testOrigin$twentyHour, model015)*100
smape(testOrigin$twentyHour, model015)*100
plot(Model15)


# NARX APPROACH

library(forecast)
library(nnfor)
library(useful)

output_column <- uow_consumption$Eight
time_delayed_data_narx <- bind_cols(t_7 = lag(output_column,8),
                                    t_4 = lag(output_column,5),
                                    t_3 = lag(output_column,4),
                                    t_2 = lag(output_column,3),
                                    t_1 = lag(output_column,2),
                                    output = output_column
)
time_delayed_data_narx <- cbind(uow_consumption[, 2:3], time_delayed_data_narx)
time_delayed_data_narx <- time_delayed_data_narx[complete.cases(time_delayed_data_narx),]
head(time_delayed_data_narx)

# Splitting the original data into train and test data
train_data_narx_original <- time_delayed_data_narx[1:380, ]
test_data_narx_original <- time_delayed_data_narx[381:nrow(time_delayed_data_narx), ]

# Minimum and Maximum Values of Data Before Normalisation
min_value_narx <- min(train_data_narx_original)
max_value_narx <- max(train_data_narx_original)

orginal_output_narx <- test_data_narx_original$output

normalise_narx = function(x) {return ((x - min(x)) /(max(x) - min(x)))}

denormalise_narx <- function(x, min, max) {return( (max - min)*x + min )}

time_delayed_narx_norm <- as.data.frame(lapply(time_delayed_data_narx[1:ncol(time_delayed_data_narx)], normalise_narx))

# Splitting the normalised data into train and test data
train_data_narx_norm <- time_delayed_narx_norm[1:380, ]
test_data_narx_norm <- time_delayed_narx_norm[381:nrow(time_delayed_narx_norm), ]

# Creating Testing Matrices for each time delay
t_1_narx_test <- test_data_narx_norm[, c("Six", "Seven", "t_1")]
t_2_narx_test <- test_data_narx_norm[, c("Six", "Seven", "t_1", "t_2")]
t_3_narx_test <- test_data_narx_norm[, c("Six", "Seven", "t_1", "t_2", "t_3")]
t_4_narx_test <- test_data_narx_norm[, c("Six", "Seven", "t_1", "t_2", "t_3", "t_4")]
t_7_narx_test <- test_data_narx_norm[, c("Six", "Seven", "t_1", "t_2", "t_3", "t_4", "t_7")]

# Function to train neural networks
train_neural_network_narx <- function(formula = formula, data = data, hidden = hidden, act.fct = 'logistic', linear.output = TRUE){
  
  neuralnet(
    formula,
    data = data,
    hidden = hidden,
    act.fct = act.fct,
    linear.output = linear.output
  )
}

model_evalutation_narx <- function(model, testing_data){
  results <- neuralnet::compute(model, testing_data)
  predicted <- results$net.result
  denormalised_predicted <- denormalise_narx(predicted, min_value_narx, max_value_narx)
  deviation = ((orginal_output_narx - denormalised_predicted)/orginal_output_narx)
  model_accuracy = 1 - abs(mean(deviation))
  accuracy = round(model_accuracy * 100 , digits = 3)
  
  mae = round(mae(orginal_output_narx, denormalised_predicted), digits = 3)
  rmse = round(rmse(orginal_output_narx, denormalised_predicted), digits = 3)
  mape = round(mape(orginal_output_narx, denormalised_predicted) * 100, digits = 3)
  smape = round(smape(orginal_output_narx, denormalised_predicted) * 100 , digits = 3)
  
  cat("Model Accuracy:", accuracy, "%\n")
  cat("RMSE:", rmse, "\n")
  cat("MAE:", mae, "\n")
  cat("MAPE:", mape, "%\n")
  cat("sMAPE:", smape, "%\n")
}

set.seed(23)

nn1_narx <- train_neural_network_narx(output ~ Six + Seven + t_1, train_data_narx_norm, 1)
nn2_narx <- train_neural_network_narx(output ~ Six + Seven + t_1 + t_2, train_data_narx_norm, 2)
nn3_narx <- train_neural_network_narx(output ~ Six + Seven + t_1 + t_2 + t_3, train_data_narx_norm, 1)
nn4_narx <- train_neural_network_narx(output ~ Six + Seven + t_1 + t_2 + t_3 + t_4, train_data_narx_norm, 2)
nn5_narx <- train_neural_network_narx(output ~ Six + Seven + t_1 + t_2 + t_3 + t_4 + t_7, train_data_narx_norm, 1)

nn1_narx_weights_h1 <- (3 + 1) * 1 + (1 + 1) * 1
cat("Number of weight parameters for nn1 with one hidden layer:", nn1_narx_weights_h1, "\n")
model_evalutation_narx(nn1_narx, t_1_narx_test)
plot(nn1_narx)

nn2_narx_weights_h1 <- (4 + 1) * 2 + (2 + 1) * 1
cat("Number of weight parameters for nn2 with one hidden layer:", nn2_narx_weights_h1, "\n")
model_evalutation_narx(nn2_narx, t_2_narx_test)
plot(nn2_narx)

nn3_narx_weights_h1 <- (5 + 1) * 1 + (1 + 1) * 1
cat("Number of weight parameters for nn3 with one hidden layer:", nn3_narx_weights_h1, "\n")
model_evalutation_narx(nn3_narx, t_3_narx_test)
plot(nn3_narx)

nn4_narx_weights_h1 <- (6 + 1) * 2 + (2 + 1) * 1
cat("Number of weight parameters for nn4 with one hidden layer:", nn4_narx_weights_h1, "\n")
model_evalutation_narx(nn4_narx, t_4_narx_test)
plot(nn4_narx)

nn5_narx_weights_h1 <- (7 + 1) * 1 + (1 + 1) * 1
cat("Number of weight parameters for nn5 with one hidden layer:", nn5_narx_weights_h1, "\n")
model_evalutation_narx(nn5_narx, t_7_narx_test)
plot(nn5_narx)


#simple line chart.
# Evaluate the model on the test set
nn4_narx_pred <- predict(nn4_narx, t_4_narx_test)

# Extract the target values from the test set
target <- t_4_narx_test$y

# Plot the predicted values and target values as a line chart
plot(target, type = "l", col = "blue", ylim = c(min(target, nn4_narx_pred), max(target, nn4_narx_pred)))
lines(nn4_narx_pred, col = "red")
legend("topright", legend = c("Target", "Predicted"), col = c("blue", "red"), lty = 1)

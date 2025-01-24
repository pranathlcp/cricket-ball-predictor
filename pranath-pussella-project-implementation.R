require(glmnet)
require(randomForest)

# A function for computing the the following metrics for classification problems
# 1) Classification Accuracy
# 2) Average Class Accuracy
# 3) F1 - Score (Macro Averaged)
compute_eval_metrics = function(predicted, actual) {
  # Input:
  #  predicted: A vector containing the predicted classes
  #  actual: A vector containing the truth values
  #
  # Output:
  #  A vector with "Classification Accuracy", "Average Class Accuracy", and
  #  "F1 - Score" respectively
  #
  predicted <- factor(as.character(predicted), levels=sort(unique(as.character(actual))))
  actual  <- as.factor(actual)
  
  confusion_matrix = as.matrix(table(actual, predicted))
  precision = diag(confusion_matrix) / colSums(confusion_matrix)
  recall = diag(confusion_matrix) / rowSums(confusion_matrix)
  
  ## Computing the Classification Accuracy
  accuracy = mean(predicted == actual)
  
  ## Computing the Average Class Accuracy (getting the arithmetic mean)
  average_class_accuracy = mean(recall)
  
  ## Computing the F1 - Score (Macro Average)
  f1 <-  ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))
  # Assuming that F1 is zero when it's not possible compute it
  f1[is.na(f1)] <- 0
  macro_f1 = mean(f1)
  
  metric_list = c(accuracy, average_class_accuracy, macro_f1)
  
  return(metric_list)
}


# The following columns are the actual columns used for modelling
# Other columns from the dataset are metadata relevant to each event
columns_for_modeling = c("outcome", "innings_score_pred", "innings_wickets_pred", 
                         "innings_legal_balls_pred", "innings_run_rate_pred", 
                         "striker_cum_avg_pred", "striker_cum_sr_pred", 
                         "striker_avg_bowler_style_pred", "striker_sr_bowler_style_pred", 
                         "striker_cum_four_rate_pred", "striker_cum_sixes_rate_pred", 
                         "last_ball_score", "striker_current_score_pred", 
                         "striker_current_sr_pred", "balls_without_boundary_pred", 
                         "rrr_pred", "bowler_avg_striker_style_pred", 
                         "bowler_sr_striker_style_pred", "bowler_cum_avg_pred", 
                         "bowler_cum_sr_pred")



### -------------------------------------- NOTE ------------------------------------- ###
##                                                                                     ##
## PLEASE SAVE THE FILES FROM "pranath-data-for-implementation.zip" INSIDE THE WORKING ## 
##                          DIRECTORY BEFORE PROCEEDING FURTHER                        ##
##                                                                                     ##
### -------------------------------------- NOTE ------------------------------------- ###


###############################################
### Preliminary Steps with Training Dataset ###
###############################################

# Loading Training Data and considering only the relevant columns for modelling
training_data = read.csv("data/cricket_training.csv")
training_data = training_data[, columns_for_modeling]
n_training = dim(training_data)[1]
table(training_data$outcome)*100/n_training

# Converting the outcome variable to factors
training_data$outcome = factor(training_data$outcome)

trainingX = model.matrix(outcome ~ ., data = training_data)
trainingY = training_data$outcome

# Computing Weights
training_weights = as.vector(1 / (table(trainingY)[trainingY] / length(trainingY)))



#################################################
### Preliminary Steps with Validation Dataset ###
#################################################

# Loading Validation Data and considering only the relevant columns for modelling
validation_data = read.csv("data/cricket_validation.csv")
validation_data = validation_data[, columns_for_modeling]
n_validation = dim(validation_data)[1]

# Converting the outcome variable to factors
validation_data$outcome = factor(validation_data$outcome)

validationX = model.matrix(outcome ~ ., data = validation_data)
validationY = validation_data$outcome


##############################################
### Preliminary Steps with Testing Dataset ###
##############################################

# Loading Testing Data and considering only the relevant columns for modelling
testing_data = read.csv("data/cricket_testing.csv")
testing_data = testing_data[, columns_for_modeling]
n_testing = dim(testing_data)[1]

# Converting the outcome variable to factors
testing_data$outcome = factor(testing_data$outcome)

testingX = model.matrix(outcome ~ ., data = testing_data)
testingY = testing_data$outcome


#########################################################
### Training and Validation Datasets will be combined ###
###    to create a single Training dataset Later      ###
#########################################################

final_training_data = rbind(training_data, validation_data)
final_trainingX = model.matrix(outcome ~ ., data = final_training_data)
final_trainingY = final_training_data$outcome

# Computing Weights
final_training_weights = as.vector(1 / (table(final_trainingY)[final_trainingY] / length(final_trainingY)))


# Note: Since the training/validation process takes a considerably long time,
#       the saved validation data for each method are available in the validation-results.zip
#       You may save them inside the working directory and load them directly, if you want to 
#       skip the training iterations.


###############################################
### Applying Logistic Regression with LASSO ###                       
###             (Non - Weighted)            ###
###############################################

# nw suffix stands for non_weighted

lambda_values_nw = seq(from = 0, to = 0.12, by = 0.001)
n_lambda_values_nw = length(lambda_values_nw)
lasso_eval_metrics_nw = setNames(data.frame(matrix(ncol = 4, nrow = n_lambda_values_nw)),
                                 c("lambda", "accuracy", "avg_class_accuracy", "f1"))
lasso_eval_metrics_nw[, "lambda"] = lambda_values_nw

for(i in 1:n_lambda_values_nw){
  lambda = lambda_values_nw[i]
  fit = glmnet(trainingX, trainingY, family = 'multinomial', lambda = lambda)
  validation_yHat = predict(fit, newx = validationX, type='class')
  validation_yHat = factor(validation_yHat, levels = levels(validationY))
  
  eval_metrics = compute_eval_metrics(validation_yHat, validationY)
  lasso_eval_metrics_nw[i, "accuracy"] = eval_metrics[1]
  lasso_eval_metrics_nw[i, "avg_class_accuracy"] = eval_metrics[2]
  lasso_eval_metrics_nw[i, "f1"] = eval_metrics[3]

  print(paste0("Iteration ", i, " out of ", n_lambda_values_nw))
}

# Save/Load results if needed
# write.csv(lasso_eval_metrics_nw, "validation_results/lasso_validation_results_nw.csv", row.names = FALSE)
# lasso_eval_metrics_nw = read.csv("validation_results/lasso_validation_results_nw.csv")

plot(lambda_values_nw, lasso_eval_metrics_nw[, "accuracy"], type = 'b', col = "black", 
     ylim = c(0, 0.70), xlab = "Lambda", ylab = "Evaluation Metric Value", 
     main = "Lambda vs Evaluation Metric (Non - Weighted Logistic Regression)",
     pch = 20, lwd = 0.5)
points(lambda_values_nw, lasso_eval_metrics_nw[, "avg_class_accuracy"], type = 'b', col = "red",
       pch = 20, lwd = 0.5)
points(lambda_values_nw, lasso_eval_metrics_nw[, "f1"], type = 'b', col = "blue",
       pch = 20, lwd = 0.5)
legend(x = "topright", legend=c("Classification Accuracy", "Average Class Accuracy", "F1 - Score"),
       col=c("black", "Red", "Blue"), lty=1, pch = c(16))


# max_val_accuracy_lasso_nw = 0.4394936
max_val_accuracy_lasso_nw = max(lasso_eval_metrics_nw[, "accuracy"])
max_val_accuracy_lasso_nw

# max_val_avg_class_accuracy_lasso_nw = 0.2014848
max_val_avg_class_accuracy_lasso_nw = max(lasso_eval_metrics_nw[, "avg_class_accuracy"])
max_val_avg_class_accuracy_lasso_nw

# Note: Two Optimal Lambda values will be chosen by considering the
#       Classification Accuracy and the Average Class Accuracy
#       Afterwards, two models will be created with the two lambda values

# Optimal Lambda when Classification Accuracy is used as the Evaluation Metric
# optimal_lambda_accuracy_lasso_nw = 0.071
optimal_lambda1_lasso_nw = lambda_values_nw[which.max(lasso_eval_metrics_nw[, "accuracy"])]
optimal_lambda1_lasso_nw

# Optimal Lambda when Average Class Accuracy is used as the Evaluation Metric
# optimal_lambda_avg_class_accuracy_lasso_nw = 0
optimal_lambda2_lasso_nw = lambda_values_nw[which.max(lasso_eval_metrics_nw[, "avg_class_accuracy"])]
optimal_lambda2_lasso_nw

# Applying the Optimal Lambda 1 to Test Dataset (Accuracy as Metric)
best_fit1_lasso_nw = glmnet(final_trainingX, final_trainingY, family = 'multinomial', lambda = optimal_lambda1_lasso_nw)
testing_yHat1_lasso_nw = predict(best_fit1_lasso_nw, newx = testingX, type = "class")
testing_yHat1_lasso_nw = factor(testing_yHat1_lasso_nw, levels = levels(testingY))

# Confusion Matrix (save the result if needed)
table(testingY, testing_yHat1_lasso_nw)
# write.csv(table(testingY, testing_yHat1_lasso_nw), "confusion_matrices/confusion_matrix_lasso_nw.csv")

# Results

# testing_accuracy1_lasso_nw = 0.4367724
testing_accuracy1_lasso_nw = compute_eval_metrics(testing_yHat1_lasso_nw, testingY)[1]
testing_accuracy1_lasso_nw

# testing_avg_class_accuracy1_lasso_nw = 0.1992909
testing_avg_class_accuracy1_lasso_nw = compute_eval_metrics(testing_yHat1_lasso_nw, testingY)[2]
testing_avg_class_accuracy1_lasso_nw

# testing_f1_score1_lasso_nw = 0.1661076
testing_f1_score1_lasso_nw = compute_eval_metrics(testing_yHat1_lasso_nw, testingY)[3]
testing_f1_score1_lasso_nw



## Applying the Optimal Lambda 2 to Test Dataset (Average Class Accuracy as Evaluation Metric)
best_fit2_lasso_nw = glmnet(final_trainingX, final_trainingY, family = 'multinomial', lambda = optimal_lambda2_lasso_nw)
testing_yHat2_lasso_nw = predict(best_fit2_lasso_nw, newx = testingX, type = "class")
testing_yHat2_lasso_nw = factor(testing_yHat2_lasso_nw, levels = levels(testingY))

# Confusion Matrix (save the result if needed)
table(testingY, testing_yHat2_lasso_nw)
# write.csv(table(testingY, testing_yHat2_lasso_nw), "confusion_matrices/confusion_matrix_lasso_nw2.csv")


# Results

# testing_accuracy2_lasso_nw = 0.4377499
testing_accuracy2_lasso_nw = compute_eval_metrics(testing_yHat2_lasso_nw, testingY)[1]
testing_accuracy2_lasso_nw

# testing_avg_class_accuracy2_lasso_nw = 0.200633
testing_avg_class_accuracy2_lasso_nw = compute_eval_metrics(testing_yHat2_lasso_nw, testingY)[2]
testing_avg_class_accuracy2_lasso_nw

# testing_f1_score2_lasso_nw = 0.1690901
testing_f1_score2_lasso_nw = compute_eval_metrics(testing_yHat2_lasso_nw, testingY)[3]
testing_f1_score2_lasso_nw


###############################################
### Applying Logistic Regression with LASSO ###                       
###                (Weighted)               ###
###############################################

# w suffix stands for weighted

lambda_values_w = seq(from = 0, to = 0.12, by = 0.001)
n_lambda_values_w = length(lambda_values_w)
lasso_eval_metrics_w = setNames(data.frame(matrix(ncol = 4, nrow = n_lambda_values_w)),
                                 c("lambda", "accuracy", "avg_class_accuracy", "f1"))
lasso_eval_metrics_w[, "lambda"] = lambda_values_w

for(i in 1:n_lambda_values_w){
  lambda = lambda_values_w[i]
  fit = glmnet(trainingX, trainingY, family = 'multinomial', lambda = lambda,
               weights = training_weights)
  validation_yHat = predict(fit, newx = validationX, type='class')
  validation_yHat = factor(validation_yHat, levels = levels(validationY))
  
  eval_metrics = compute_eval_metrics(validation_yHat, validationY)
  lasso_eval_metrics_w[i, "accuracy"] = eval_metrics[1]
  lasso_eval_metrics_w[i, "avg_class_accuracy"] = eval_metrics[2]
  lasso_eval_metrics_w[i, "f1"] = eval_metrics[3]
  
  print(paste0("Iteration ", i, " out of ", n_lambda_values_w))
}

# Save/Load results if needed
# write.csv(lasso_eval_metrics_w, "validation_results/lasso_validation_results_w.csv", row.names = FALSE)
# lasso_eval_metrics_w = read.csv("validation_results/lasso_validation_results_w.csv")

plot(lambda_values_w, lasso_eval_metrics_w[, "accuracy"], type = 'b', col = "black", 
     ylim = c(0, 0.40), xlab = "Lambda", ylab = "Evaluation Metric Value", 
     main = "Lambda vs Evaluation Metric (Non - Weighted Logistic Regression)",
     pch = 20, lwd = 0.5)
points(lambda_values_w, lasso_eval_metrics_w[, "avg_class_accuracy"], type = 'b', col = "red",
       pch = 20, lwd = 0.5)
points(lambda_values_w, lasso_eval_metrics_w[, "f1"], type = 'b', col = "blue",
       pch = 20, lwd = 0.5)
legend(x = "topright", legend=c("Classification Accuracy", "Average Class Accuracy", "F1 - Score"),
       col=c("black", "Red", "Blue"), lty=1, pch = c(16))

# max_val_accuracy_lasso_w = 0.3232785
max_val_accuracy_lasso_w = max(lasso_eval_metrics_w[, "accuracy"])
max_val_accuracy_lasso_w

# max_val_avg_class_accuracy_lasso_w = 0.2439373
max_val_avg_class_accuracy_lasso_w = max(lasso_eval_metrics_w[, "avg_class_accuracy"])
max_val_avg_class_accuracy_lasso_w

# Note: Two Optimal Lambda values will be chosen by considering the
#       Classification Accuracy and the Average Class Accuracy
#       Afterwards, two models will be created with the two lambda values

# Optimal Lambda when Classification Accuracy is used as the Evaluation Metric
# optimal_lambda_accuracy_lasso_w = 0.037
optimal_lambda1_lasso_w = lambda_values_w[which.max(lasso_eval_metrics_w[, "accuracy"])]
optimal_lambda1_lasso_w


# Optimal Lambda when Average Class Accuracy is used as the Evaluation Metric
# optimal_lambda_avg_class_accuracy_lasso_w = 0
optimal_lambda2_lasso_w = lambda_values_w[which.max(lasso_eval_metrics_w[, "avg_class_accuracy"])]
optimal_lambda2_lasso_w

# Applying the Optimal Lambda 1 to Test Dataset (Accuracy as Metric)
best_fit1_lasso_w = glmnet(final_trainingX, final_trainingY, family = 'multinomial', 
                           lambda = optimal_lambda1_lasso_w, weights = final_training_weights)
testing_yHat1_lasso_w = predict(best_fit1_lasso_w, newx = testingX, type = "class")
testing_yHat1_lasso_w = factor(testing_yHat1_lasso_w, levels = levels(testingY))

# Confusion Matrix (save the result if needed)
table(testingY, testing_yHat1_lasso_w)
# write.csv(table(testingY, testing_yHat1_lasso_w), "confusion_matrices/confusion_matrix_lasso_w1.csv")


# Results

# testing_accuracy1_lasso_w = 0.3268462
testing_accuracy1_lasso_w = compute_eval_metrics(testing_yHat1_lasso_w, testingY)[1]
testing_accuracy1_lasso_w

# testing_avg_class_accuracy1_lasso_w = 0.2194199
testing_avg_class_accuracy1_lasso_w = compute_eval_metrics(testing_yHat1_lasso_w, testingY)[2]
testing_avg_class_accuracy1_lasso_w

# testing_f1_score1_lasso_w = 0.1618038
testing_f1_score1_lasso_w = compute_eval_metrics(testing_yHat1_lasso_w, testingY)[3]
testing_f1_score1_lasso_w



## Applying the Optimal Lambda 2 to Test Dataset (Average Class Accuracy as Evaluation Metric)
best_fit2_lasso_w = glmnet(final_trainingX, final_trainingY, family = 'multinomial', 
                           lambda = optimal_lambda2_lasso_w, weights = final_training_weights)
testing_yHat2_lasso_w = predict(best_fit2_lasso_w, newx = testingX, type = "class")
testing_yHat2_lasso_w = factor(testing_yHat2_lasso_w, levels = levels(testingY))

# Confusion Matrix (save the result if needed)
table(testingY, testing_yHat2_lasso_w)
# write.csv(table(testingY, testing_yHat2_lasso_w), "confusion_matrices/confusion_matrix_lasso_w2.csv")


# Results

# testing_accuracy2_lasso_w = 0.2501999
testing_accuracy2_lasso_w = compute_eval_metrics(testing_yHat2_lasso_w, testingY)[1]
testing_accuracy2_lasso_w

# testing_avg_class_accuracy2_lasso_w = 0.238344
testing_avg_class_accuracy2_lasso_w = compute_eval_metrics(testing_yHat2_lasso_w, testingY)[2]
testing_avg_class_accuracy2_lasso_w

# testing_f1_score2_lasso_w = 0.1877949
testing_f1_score2_lasso_w = compute_eval_metrics(testing_yHat2_lasso_w, testingY)[3]
testing_f1_score2_lasso_w


##############################################

###############################
### Applying Random Forests ###                       
###    (Non - Weighted)     ###
###############################

# nw suffix stands for non_weighted

n_trees = 500

m_values_nw = seq(from = 2, to = 19, by = 1)
n_m_values_nw = length(m_values_nw)

rf_eval_metrics_nw = setNames(data.frame(matrix(ncol = 4, nrow = n_m_values_nw)),
                                 c("m", "accuracy", "avg_class_accuracy", "f1"))
rf_eval_metrics_nw[, "m"] = m_values_nw

# Note: It will take about 15 minutes per iteration
for(k in 1:n_m_values_nw){
  
  m = m_values_nw[k]
  
  forest = randomForest(outcome ~ ., data = training_data, 
                        ntree = n_trees, mtry = m, nodesize = 4)
  
  validation_yHat = predict(forest, validationX)
  
  eval_metrics = compute_eval_metrics(validation_yHat, validationY)
  rf_eval_metrics_nw[k, "accuracy"] = eval_metrics[1]
  rf_eval_metrics_nw[k, "avg_class_accuracy"] = eval_metrics[2]
  rf_eval_metrics_nw[k, "f1"] = eval_metrics[3]

  print(paste0("Iteration ", k, " out of ", n_m_values))

  # Save results of each iteration just-in-case R crashes
  # filename = paste0("rf_results/forest_results_nw_iteration_", m,"_", n_trees,"ntrees.csv")
  # write.csv(rf_eval_metrics_nw, filename, row.names = FALSE)
}

# Save/Load the results
# write.csv(rf_eval_metrics_nw, "validation_results/rf_validation_results.csv", row.names = FALSE)
# rf_eval_metrics_nw = read.csv("validation_results/forest_validation_results_nw.csv")

plot(m_values_nw, rf_eval_metrics_nw[, "accuracy"], type = 'b', col = "black", 
     ylim = c(0.15, 0.60), xlab = "m", ylab = "Evaluation Metric Value", 
     main = "m vs Evaluation Metric (Non - Weighted Random Forests)",
     pch = 20, lwd = 0.5)
points(m_values_nw, rf_eval_metrics_nw[, "avg_class_accuracy"], type = 'b', col = "red",
       pch = 20, lwd = 0.5)
points(m_values_nw, rf_eval_metrics_nw[, "f1"], type = 'b', col = "blue",
       pch = 20, lwd = 0.5)
legend(x = "topright", legend=c("Classification Accuracy", "Average Class Accuracy", "F1 - Score"),
       col=c("black", "Red", "Blue"), lty=1, pch = c(16))


# max_val_accuracy_rf_nw = 0.4167037
max_val_accuracy_rf_nw = max(rf_eval_metrics_nw[, "accuracy"])
max_val_accuracy_rf_nw

# max_val_avg_class_accuracy_rf_nw = 0.1969875
max_val_avg_class_accuracy_rf_nw = max(rf_eval_metrics_nw[, "avg_class_accuracy"])
max_val_avg_class_accuracy_rf_nw

# Note: Two Optimal m values will be chosen by considering the
#       Classification Accuracy and the Average Class Accuracy
#       Afterwards, two models will be created with the two m values

# Optimal m when Classification Accuracy is used as the Evaluation Metric
# optimal_m1_rf_nw = 2
optimal_m1_rf_nw = m_values_nw[which.max(rf_eval_metrics_nw[, "accuracy"])]
optimal_m1_rf_nw


# Optimal Lambda when Average Class Accuracy is used as the Evaluation Metric
# optimal_m2_rf_nw = 2
optimal_m2_rf_nw = m_values_nw[which.max(rf_eval_metrics_nw[, "avg_class_accuracy"])]
optimal_m2_rf_nw


# optimal_m1_rf_nw = optimal_m2_rf_nw = 2
# We have to create only one model with the optimal m = 2 value

# Applying the Optimal m = 2 to Test Dataset
best_fit_rf_nw = randomForest(outcome ~ ., data = final_training_data, 
                               ntree = n_trees, mtry = optimal_m1_rf_nw, nodesize = 4)

testing_yHat_rf_nw = predict(best_fit_rf_nw, testingX)

# Confusion Matrix (save the result if needed)
table(testingY, testing_yHat_rf_nw)
# write.csv(table(testingY, testing_yHat_rf_nw), "confusion_matrices/confusion_matrix_rf_nw.csv")


# Results

# testing_accuracy_rf_nw = 0.4106905
testing_accuracy_rf_nw = compute_eval_metrics(testing_yHat_rf_nw, testingY)[1]
testing_accuracy_rf_nw

# testing_avg_class_accuracy_rf_nw = 0.1921901
testing_avg_class_accuracy_rf_nw = compute_eval_metrics(testing_yHat_rf_nw, testingY)[2]
testing_avg_class_accuracy_rf_nw

# testing_f1_score_rf_nw = 0.1689349
testing_f1_score_rf_nw = compute_eval_metrics(testing_yHat_rf_nw, testingY)[3]
testing_f1_score_rf_nw




###############################
### Applying Random Forests ###                       
###       (Weighted)        ###
###############################

# w suffix stands for non_weighted

n_trees = 500

m_values_w = seq(from = 2, to = 19, by = 1)
n_m_values_w = length(m_values_w)

rf_eval_metrics_w = setNames(data.frame(matrix(ncol = 4, nrow = n_m_values_w)),
                              c("m", "accuracy", "avg_class_accuracy", "f1"))
rf_eval_metrics_w[, "m"] = m_values_w

# Note: Each iteration takes about 15 minutes for training
for(k in 15:15){
  
  m = m_values_w[k]
  
  forest = randomForest(outcome ~ ., data = training_data, 
                        ntree = n_trees, mtry = m, nodesize = 4,
                        weights = training_weights)
  
  validation_yHat = predict(forest, validationX)
  
  eval_metrics = compute_eval_metrics(validation_yHat, validationY)
  rf_eval_metrics_w[k, "accuracy"] = eval_metrics[1]
  rf_eval_metrics_w[k, "avg_class_accuracy"] = eval_metrics[2]
  rf_eval_metrics_w[k, "f1"] = eval_metrics[3]
  
  print(paste0("Iteration ", k, " out of ", n_m_values_w))
  
  # Save the results just-in-case R crashes
  # filename = paste0("rf_results/forest_results_weighted_iteration_", m,"_", n_trees,"ntrees.csv")
  # write.csv(rf_eval_metrics_w, filename, row.names = FALSE)
}

# Save/Load results if needed
# write.csv(rf_eval_metrics_w, "rf_eval_metrics_w.csv", row.names = FALSE)
# rf_eval_metrics_w = read.csv("forest_validation_results_w.csv")

plot(m_values_w, rf_eval_metrics_w[, "accuracy"], type = 'b', col = "black", 
     ylim = c(0.20, 0.40), xlab = "m", ylab = "Evaluation Metric Value", 
     main = "m vs Evaluation Metric (Non - Weighted Random Forests)",
     pch = 20, lwd = 0.5)
points(m_values_w, rf_eval_metrics_w[, "avg_class_accuracy"], type = 'b', col = "red",
       pch = 20, lwd = 0.5)
points(m_values_w, rf_eval_metrics_w[, "f1"], type = 'b', col = "blue",
       pch = 20, lwd = 0.5)
legend(x = "topright", legend=c("Classification Accuracy", "Average Class Accuracy", "F1 - Score"),
       col=c("black", "Red", "Blue"), lty=1, pch = c(16))


# max_val_accuracy_rf_w = 0.3386939
max_val_accuracy_rf_w = max(rf_eval_metrics_w[, "accuracy"])
max_val_accuracy_rf_w

# max_val_avg_class_accuracy_rf_w = 0.2191808
max_val_avg_class_accuracy_rf_w = max(rf_eval_metrics_w[, "avg_class_accuracy"])
max_val_avg_class_accuracy_rf_w

# Note: Two Optimal m values will be chosen by considering the
#       Classification Accuracy and the Average Class Accuracy
#       Afterwards, two models will be created with the two m values

# Optimal m when Classification Accuracy is used as the Evaluation Metric
# optimal_m1_rf_w = 12
optimal_m1_rf_w = m_values_w[which.max(rf_eval_metrics_w[, "accuracy"])]
optimal_m1_rf_w


# Optimal Lambda when Average Class Accuracy is used as the Evaluation Metric
# optimal_m2_rf_w = 2
optimal_m2_rf_w = m_values_w[which.max(rf_eval_metrics_w[, "avg_class_accuracy"])]
optimal_m2_rf_w


# Applying the Optimal m1 to Test Dataset (Accuracy as Metric)
best_fit1_rf_w = randomForest(outcome ~ ., data = final_training_data, 
                              ntree = n_trees, mtry = optimal_m1_rf_w, nodesize = 4,
                              weights = final_training_weights)

testing_yHat1_rf_w = predict(best_fit1_rf_w, testingX)

# Confusion Matrix (save the result if needed)
table(testingY, testing_yHat1_rf_w)
# write.csv(table(testingY, testing_yHat1_rf_w), "confusion_matrices/confusion_matrix_rf_w.csv")


# Results

# testing_accuracy1_rf_w = 0.3268017
testing_accuracy1_rf_w = compute_eval_metrics(testing_yHat1_rf_w, testingY)[1]
testing_accuracy1_rf_w

# testing_avg_class_accuracy1_rf_w = 0.205411
testing_avg_class_accuracy1_rf_w = compute_eval_metrics(testing_yHat1_rf_w, testingY)[2]
testing_avg_class_accuracy1_rf_w

# testing_f1_score1_rf_w = 0.2040879
testing_f1_score1_rf_w = compute_eval_metrics(testing_yHat1_rf_w, testingY)[3]
testing_f1_score1_rf_w


# Applying the Optimal m2 to Test Dataset (Accuracy as Metric)
best_fit2_rf_w = randomForest(outcome ~ ., data = final_training_data, 
                              ntree = n_trees, mtry = optimal_m2_rf_w, nodesize = 4,
                              weights = final_training_weights)

testing_yHat2_rf_w = predict(best_fit2_rf_w, testingX)

# Confusion Matrix (save the result if needed)
table(testingY, testing_yHat2_rf_w)
# write.csv(table(testingY, testing_yHat2_rf_w), "confusion_matrices/confusion_matrix_rf_w2.csv")


# Results
# testing_accuracy2_rf_w = 0.3281347
testing_accuracy2_rf_w = compute_eval_metrics(testing_yHat2_rf_w, testingY)[1]
testing_accuracy2_rf_w

# testing_avg_class_accuracy21_rf_w = 0.2115887
testing_avg_class_accuracy21_rf_w = compute_eval_metrics(testing_yHat2_rf_w, testingY)[2]
testing_avg_class_accuracy21_rf_w

# testing_f1_score2_rf_w = 0.2092088
testing_f1_score2_rf_w = compute_eval_metrics(testing_yHat2_rf_w, testingY)[3]
testing_f1_score2_rf_w



############# 
#  THE END  #
#############

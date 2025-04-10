# download the dataset in google sheets 'ML_dataset' named 'Copy of KNN_Imputed' in an csv file, and import from text (base)
# assign the data as a variable called data
data <- ML_dataset...Copy.of.KNN_Imputed

#give column names
colnames(data) <- c('t5_spring40',	'ancestry',	'heterozygosity', 'sex_binary',	'tarsus_length', 
                    'fat_score', 'tail_length',	'wing_cord', 'kipps', 'distal', 'p9','p10', 
                    'bearing_fall_1',	'doy_fall_r1')


#multiple logistic regression
Multiple_logistic_model = glm(t5_spring40 ~ ancestry + heterozygosity + 
                                sex_binary + tarsus_length + fat_score + tail_length + 
                                wing_cord + kipps + distal + p9 + p10 + bearing_fall_1 + doy_fall_r1, 
                              family = binomial(link="logit"), 
                              data = data)
summary(Multiple_logistic_model)

# Make predictions on the training data
predictions <- predict(Multiple_logistic_model, type = "response")

# Convert probabilities to binary predictions (0 or 1)
binary_predictions <- ifelse(predictions > 0.5, 1, 0)

# Create a confusion matrix
conf_matrix <- table(data$t5_spring40, binary_predictions)
conf_matrix

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy






#ROC curve
# Load the pROC package
library(pROC)
#multiple logistic regression
Multiple_logistic_model = glm(t5_spring40 ~ ancestry + heterozygosity + 
                                sex_binary + tarsus_length + fat_score + tail_length + 
                                wing_cord + kipps + distal + p9 + p10 + bearing_fall_1 + doy_fall_r1, 
                              family = binomial(link="logit"), 
                              data = data)
# Obtain predicted probabilities
predicted_probabilities <- predict(Multiple_logistic_model, type = "response")
# Compute ROC curve
roc_curve <- roc(data$t5_spring40, predicted_probabilities)
# Plot ROC curve
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)
# Add AUC value to the plot
legend("bottomright", paste("AUC =", round(auc(roc_curve), 2)), col = "blue", lty = 1, lwd = 2)



#multiple logistic regression removing anything with p value more than 0.7
Multiple_logistic_model = glm(t5_spring40 ~ heterozygosity + 
                                 fat_score + tail_length + 
                                wing_cord + kipps + distal + p9 + p10, 
                              family = binomial(link="logit"), 
                              data = data)
summary(Multiple_logistic_model)

#multiple logistic regression removing anything with p value more than 0.4
Multiple_logistic_model = glm(t5_spring40 ~ heterozygosity + 
                                fat_score + tail_length + 
                                kipps + distal + p9 + p10, 
                              family = binomial(link="logit"), 
                              data = data)
summary(Multiple_logistic_model)

#multiple logistic regression removing anything with p value more than 0.3
Multiple_logistic_model = glm(t5_spring40 ~ heterozygosity + 
                                fat_score + tail_length + 
                                kipps + distal + p9, 
                              family = binomial(link="logit"), 
                              data = data)
summary(Multiple_logistic_model)

#multiple logistic regression removing anything with p value more than 0.3, gave the highest AIC compared to further removal
Multiple_logistic_model = glm(t5_spring40 ~ heterozygosity + 
                                fat_score + tail_length + 
                                kipps + p9, 
                              family = binomial(link="logit"), 
                              data = data)
summary(Multiple_logistic_model)

# Make predictions on the training data
predictions <- predict(Multiple_logistic_model, type = "response")

# Convert probabilities to binary predictions (0 or 1)
binary_predictions <- ifelse(predictions > 0.5, 1, 0)

# Create a confusion matrix
conf_matrix <- table(data$t5_spring40, binary_predictions)
conf_matrix

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy


#ROC curve
# Load the pROC package
library(pROC)
#multiple logistic regression
Multiple_logistic_model = glm(t5_spring40 ~ heterozygosity + 
                                fat_score + tail_length + 
                                kipps + p9, 
                              family = binomial(link="logit"), 
                              data = data)
# Obtain predicted probabilities
predicted_probabilities <- predict(Multiple_logistic_model, type = "response")
# Compute ROC curve
roc_curve <- roc(data$t5_spring40, predicted_probabilities)
# Plot ROC curve
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)
# Add AUC value to the plot
legend("bottomright", paste("AUC =", round(auc(roc_curve), 2)), col = "blue", lty = 1, lwd = 2)








#Probit regression
Probit_model = glm(t5_spring40 ~ ancestry + heterozygosity + 
                     sex_binary + tarsus_length + fat_score + tail_length + 
                     wing_cord + kipps + distal + p9 + p10 + bearing_fall_1 + doy_fall_r1, 
                   family = binomial(link = "probit"), 
                   data = data)
summary(Probit_model)

# Make predictions on the training data
predictions <- predict(Probit_model, type = "response")

# Convert probabilities to binary predictions (0 or 1)
binary_predictions <- ifelse(predictions > 0.5, 1, 0)

# Create a confusion matrix
conf_matrix <- table(data$t5_spring40, binary_predictions)
conf_matrix

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy


#ROC curve
# Load the pROC package
library(pROC)
#multiple logistic regression
Probit_model = glm(t5_spring40 ~ ancestry + heterozygosity + 
                     sex_binary + tarsus_length + fat_score + tail_length + 
                     wing_cord + kipps + distal + p9 + p10 + bearing_fall_1 + doy_fall_r1, 
                   family = binomial(link = "probit"), 
                   data = data)
# Obtain predicted probabilities
predicted_probabilities <- predict(Probit_model, type = "response")
# Compute ROC curve
roc_curve <- roc(data$t5_spring40, predicted_probabilities)
# Plot ROC curve
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)
# Add AUC value to the plot
legend("bottomright", paste("AUC =", round(auc(roc_curve), 2)), col = "blue", lty = 1, lwd = 2)


#Probit regression - removed anythign with p value above 0.7
Probit_model = glm(t5_spring40 ~ heterozygosity + 
                       fat_score + tail_length + 
                     wing_cord + kipps + distal + p9 + p10, 
                   family = binomial(link = "probit"), 
                   data = data)
summary(Probit_model)

#Probit regression - removed anythign with p value above 0.4
Probit_model = glm(t5_spring40 ~ heterozygosity + 
                     fat_score + tail_length + 
                     kipps + distal + p9 + p10, 
                   family = binomial(link = "probit"), 
                   data = data)
summary(Probit_model)

#Probit regression - removed anythign with p value above 0.27
Probit_model = glm(t5_spring40 ~ heterozygosity + 
                     fat_score + tail_length + 
                     kipps + distal + p9, 
                   family = binomial(link = "probit"), 
                   data = data)
summary(Probit_model)

#Probit regression - removed anythign with p value above 0.4 - gave the highest AIC compared to removing further
Probit_model = glm(t5_spring40 ~ heterozygosity + 
                     fat_score + tail_length + 
                     kipps + p9, 
                   family = binomial(link = "probit"), 
                   data = data)
summary(Probit_model)

# Make predictions on the training data
predictions <- predict(Probit_model, type = "response")

# Convert probabilities to binary predictions (0 or 1)
binary_predictions <- ifelse(predictions > 0.5, 1, 0)

# Create a confusion matrix
conf_matrix <- table(data$t5_spring40, binary_predictions)
conf_matrix

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy


#ROC curve
# Load the pROC package
library(pROC)
#multiple logistic regression
Probit_model = glm(t5_spring40 ~ heterozygosity + 
                     fat_score + tail_length + 
                     kipps + p9, 
                   family = binomial(link = "probit"), 
                   data = data)
# Obtain predicted probabilities
predicted_probabilities <- predict(Probit_model, type = "response")
# Compute ROC curve
roc_curve <- roc(data$t5_spring40, predicted_probabilities)
# Plot ROC curve
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)
# Add AUC value to the plot
legend("bottomright", paste("AUC =", round(auc(roc_curve), 2)), col = "blue", lty = 1, lwd = 2)




































 






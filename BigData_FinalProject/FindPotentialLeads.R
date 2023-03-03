install.packages("tidyverse")
install.packages("plotly")
install.packages("fastDummies")
install.packages("gbm")
install.packages("randomForest")   # Install randomForest package
install.packages("ROCR")
library(caret)
library('fastDummies')
library(tidyverse) # includes dplyr, ggplot2, and other useful packages
library(caret) # for machine learning and cross-validation
library(e1071) # for Support Vector Machines
library(plotly)
library(class)
library(gbm)
library(rpart)
library(randomForest)  
library(ROCR)

rm(list = ls())
# Supress warnings
options(warn=-1)

# Load data
leads.df <- read.csv("C:/Users/DELL/Downloads/Leads.csv")
leads.df

leads.df[leads.df == "Select"] <- NA
leads.df[leads.df == ""] <- NA

head(leads.df)

#Exclude irrelevant columns
leads.df <- leads.df[, !(names(leads.df) %in% c("Prospect.ID", "Lead.Number"))]
dim(leads.df)

#To get percentage of nulls in individual columns
round(colSums(is.na(leads.df)) * 100 / nrow(leads.df), 2)

# Method for dropping columns with an excess of nulls

rmissingvaluecol <- function(leads.dff, threshold) {
  missing_pct <- colSums(is.na(leads.dff)) * 100 / nrow(leads.dff)
  cols_to_remove <- names(leads.dff[, missing_pct >= threshold])
  cat("Columns having more than", threshold, "percent missing values: ", length(cols_to_remove), "\n")
  cat(cols_to_remove, "\n")
  return(leads.dff[, !(names(leads.dff) %in% cols_to_remove)])
}

# Dropping columns having 40% missing values
modified_leads.df <- rmissingvaluecol(leads.df, 40)

dim(modified_leads.df)

# Obtain the quantity of converted and non-converted leads
leads.dfg <- aggregate(leads.df["Lead.Origin"], by = list(leads.df$Converted), FUN = length)
colnames(leads.dfg) <- c("Converted", "Count")
leads.dfg$Converted <- as.character(leads.dfg$Converted)

# Correlation Matrix need to define
numeric_cols <- sapply(leads.df, is.numeric)
numeric_df <- leads.df[, numeric_cols]

# Calculate the correlation matrix
cor_matrix <- cor(numeric_df,use = "pairwise.complete.obs")

# Convert the correlation matrix to a data frame
cor_df <- reshape2::melt(cor_matrix)

# Plot the correlation matrix using plotly
plot_ly(cor_df, x = ~Var1, y = ~Var2, z = ~value, type = "heatmap",
        colorscale = "RdBu",  text = ~text) %>%
  layout(title = "Correlation Matrix")

#Evaluating target class distribution
fig <- plot_ly(leads.dfg, x = ~Converted, y = ~Count, color = ~Converted, text = ~Count, type = "bar") %>%
  layout(title = "Evaluating target class distribution", xaxis = list(title = "Converted"), yaxis = list(title = "Count"), colorway = c("red", "blue"))

fig

#Scatter plot between Leads and Total Time Spent
fig <- plot_ly(modified_leads.df, x = ~`Total.Time.Spent.on.Website`, color = ~Converted, type = "scatter", mode = "markers") %>%
  layout(title = "Scatter plot between Leads and Total Time Spent", xaxis = list(title = "Total Time Spent on Website"), yaxis = list(title = "Count"))

fig

#Box plot for Total Visits
fig <- plot_ly(modified_leads.df, y = ~TotalVisits, type = "box") %>%
  layout(title = "Box plot for TotalVisits")

fig

# Remove outliers
upperlimit <- quantile(modified_leads.df$TotalVisits, 0.990, na.rm=TRUE)
modified_leads.df <- modified_leads.df[(modified_leads.df$TotalVisits <= upperlimit),]
lowerlimit <- quantile(modified_leads.df$TotalVisits, 0.01, na.rm=TRUE)
modified_leads.df <- modified_leads.df[(modified_leads.df$TotalVisits >= lowerlimit),]

fig <- plot_ly(modified_leads.df, y = ~TotalVisits, type = "box", 
               title = "After removing the outliers")
fig

#Total Time Spent on Website
fig <- plot_ly(modified_leads.df, y = ~Total.Time.Spent.on.Website, type = "box", 
               title = "Total Time Spent on Website")
fig

#Page Views Per Visit
fig <- plot_ly(modified_leads.df, y = ~Page.Views.Per.Visit, type = "box", 
               title = "Page Views Per Visit")
fig

# Removing top and bottom 1% 
upperlimit <- quantile(modified_leads.df$`Page.Views.Per.Visit`, 0.99, na.rm = TRUE)
modified_leads.df <- modified_leads.df[modified_leads.df$`Page.Views.Per.Visit` <= upperlimit, ]
lowerlimit <- quantile(modified_leads.df$`Page.Views.Per.Visit`, 0.01, na.rm = TRUE)
modified_leads.df <- modified_leads.df[modified_leads.df$`Page.Views.Per.Visit` >= lowerlimit, ]

fig <- plot_ly(modified_leads.df, y = ~`Page.Views.Per.Visit`, type = "box", 
               title = "After removing the outliers")
fig

############################### ANALYSING CATEGORICAL VARIABLES#############################

###### CITY

df_for_plot <- modified_leads.df[complete.cases(modified_leads.df$City), ]
ggplot(df_for_plot, aes(x = City, fill = factor(Converted))) +
  geom_bar(na.rm = TRUE) +
  labs(title = "Leads by City", x = "City", y = "Count") +
  scale_fill_manual(values = c("steelblue", "orange"), labels = c("Not Converted", "Converted")) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

modified_leads.df$City <- ifelse(is.na(modified_leads.df$City), "Not Specified", modified_leads.df$City)

###### SPECIALIZATION

modified_leads.df$Specialization <- ifelse(modified_leads.df$Specialization %in% c("Finance Management", "Human Resource Management",
                                                                                   "Marketing Management", "Operations Management",
                                                                                   "IT Projects Management", "Supply Chain Management",
                                                                                   "Healthcare Management", "Hospitality Management",
                                                                                   "Retail Management"), "Management_Specializations",
                                           modified_leads.df$Specialization)

df_for_plot <- modified_leads.df[complete.cases(modified_leads.df$Specialization), ]

ggplot(df_for_plot, aes(x = Specialization, fill = factor(Converted))) +
  geom_bar() +
  labs(title = "Specialization", x = "Specialization", y = "Count") +
  scale_fill_manual(values = c("steelblue", "orange"), labels = c("Not Converted", "Converted")) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

modified_leads.df$Specialization[is.na(modified_leads.df$Specialization)] <- "Not Specified"
dim(modified_leads.df)

###### WHAT IS YOUR CURRENT OCCUPATION

df_for_plot <- modified_leads.df[complete.cases(modified_leads.df$What.is.your.current.occupation), ]
ggplot(df_for_plot, aes(x = What.is.your.current.occupation, fill = factor(Converted))) +
  geom_bar() +
  labs(title = "WHAT IS YOUR CURRENT OCCUPATION", x = "OCCUPATION", y = "Count") +
  scale_fill_manual(values = c("steelblue", "orange"), labels = c("Not Converted", "Converted")) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Replace all NA values in 'What is your current occupation' with 'Not Specified'
modified_leads.df$`What.is.your.current.occupation` <- ifelse(
  is.na(modified_leads.df$`What.is.your.current.occupation`), 
  "Not Specified", 
  modified_leads.df$`What.is.your.current.occupation`
)


# Replace 'google' with 'Google' and 'Facebook' with 'Social Media'
modified_leads.df$`Lead.Source` <- ifelse(
  modified_leads.df$`Lead.Source` == "google", 
  "Google", 
  modified_leads.df$`Lead.Source`
)
modified_leads.df$`Lead.Source` <- ifelse(
  modified_leads.df$`Lead.Source` == "Facebook", 
  "Social Media", 
  modified_leads.df$`Lead.Source`
)

# Replace low frequency values with 'Others'
modified_leads.df$`Lead.Source` <- ifelse(
  modified_leads.df$`Lead.Source` %in% c("bing", "Click2call", "Press_Release", "youtubechannel", "welearnblog_Home", 
                                         "WeLearn", "blog", "Pay per Click Ads", "testone", "NC_EDM"), 
  "Others", 
  modified_leads.df$`Lead.Source`
)

df_for_plot <- modified_leads.df[complete.cases(modified_leads.df$Lead.Source), ]
ggplot(df_for_plot, aes(x = Lead.Source, fill = factor(Converted))) +
  geom_bar() +
  labs(title = "Lead Source", x = "Lead Source", y = "Count") +
  scale_fill_manual(values = c("steelblue", "orange"), labels = c("Not Converted", "Converted")) +
  theme_minimal()+theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Replace NaN values in 'Lead.Source' with 'Others'
modified_leads.df$`Lead.Source` <- ifelse(
  is.na(modified_leads.df$`Lead.Source`), 
  "Others", 
  modified_leads.df$`Lead.Source`
)

###### LAST ACTIVITY

# Replace low frequency values with 'Others'
modified_leads.df$`Last.Activity` <- ifelse(
  modified_leads.df$`Last.Activity` %in% c("Unreachable", "Unsubscribed", "Had a Phone Conversation", 
                                           "Approached upfront", "View in browser link Clicked", "Email Marked Spam", 
                                           "Email Received", "Resubscribed to emails", "Visited Booth in Tradeshow"), 
  "Others", 
  modified_leads.df$`Last.Activity`
)

df_for_plot <- modified_leads.df[complete.cases(modified_leads.df$Last.Activity), ]
ggplot(df_for_plot, aes(x = Last.Activity, fill = factor(Converted))) +
  geom_bar() +
  labs(title = "Lead Activity", x = "Last.Activity", y = "Count") +
  scale_fill_manual(values = c("steelblue", "orange"), labels = c("Not Converted", "Converted")) +
  theme_minimal()+theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Replace NaN values in 'Last.Activity' with 'Others'
modified_leads.df$`Last.Activity` <- ifelse(
  is.na(modified_leads.df$`Last.Activity`), 
  "Others", 
  modified_leads.df$`Last.Activity`
)

###### WHAT MATTERS MOST TO YOU IN CHOOSING A COURSE

modified_leads.df$`What.matters.most.to.you.in.choosing.a.course` <- ifelse(
  is.na(modified_leads.df$`What.matters.most.to.you.in.choosing.a.course`), 
  "Better Career Prospects", 
  modified_leads.df$`What.matters.most.to.you.in.choosing.a.course`
)

# Define columns to drop

cols_to_drop <- c('Country','Do.Not.Call','Search','Magazine','Newspaper.Article','X.Education.Forums','Newspaper',
                  'Digital.Advertisement','Through.Recommendations','Receive.More.Updates.About.Our.Courses',
                  'Update.me.on.Supply.Chain.Content','Get.updates.on.DM.Content','I.agree.to.pay.the.amount.through.cheque','Tags')

# Drop the highly skewed columns

modified.df <- subset(modified_leads.df, select = !(names(modified_leads.df) %in% cols_to_drop))
modified.df <- na.omit(modified.df)

non_numeric_cols <- names(modified.df)[!sapply(modified.df, is.numeric)]

df_dummies <- dummy_cols(modified.df)
df_dummies <- subset(df_dummies, select = !(names(df_dummies) %in% non_numeric_cols))
dim(df_dummies)

names(df_dummies) <- gsub("[,&-]+", ".", names(df_dummies))
names(df_dummies) <- gsub("\\s+", ".", names(df_dummies))

# Partition Data
set.seed(2)
train.index <- sample(c(1:dim(df_dummies)[1]), dim(df_dummies)[1]*0.8)
train.df <- df_dummies[train.index, ]
valid.df <- df_dummies[-train.index, ]

############################# Logistic regression #########################################

logit.model <- glm(Converted ~ ., data = train.df, family = "binomial") 
options(scipen=999) # remove scientific notation
summary(logit.model)

logit.model.pred <- predict(logit.model, valid.df, type = "response")

# Map into TP, FP, TN, FN
confusionMatrix(as.factor(ifelse(logit.model.pred > 0.5, "Positive", "Negative")), as.factor(ifelse(valid.df$Converted==1,"Positive", "Negative")))

logit_prob <- prediction(logit.model.pred, valid.df$Converted)
logit_perf <- performance(logit_prob, "tpr", "fpr")
plot(logit_perf, main = "Logit ROC Curve", col = "red")
abline(0, 1, lty = 2, col = "black")
logit_auc <- performance(logit_prob, measure = "auc")@y.values[[1]]
cat("Logit AUC:", logit_auc, "\n")

############################# Support Vector Machine  #####################################

svm.model <- svm(Converted ~ ., data = train.df,kernel = "radial")

# Find Support Vectors
dim(svm.model$SV)

## predict class membership: Validation
svm_pred_valid <- predict(svm.model, newdata = valid.df)

confusionMatrix(as.factor(ifelse(svm_pred_valid > 0.5, "Positive", "Negative")), 
                as.factor(ifelse(valid.df$Converted==1,"Positive", "Negative")))

svm_prob <- prediction(svm_pred_valid, valid.df$Converted)
svm_perf <- performance(svm_prob, "tpr", "fpr")
plot(svm_perf, main = "SVM ROC Curve", col = "red")
abline(0, 1, lty = 2, col = "black")
svm_auc <- performance(svm_prob, measure = "auc")@y.values[[1]]
cat("SVM AUC:", svm_auc, "\n")

############################# K Nearest Neighbor ###########################################

knn_model <- train(Converted ~ ., data = train.df, method = "knn", 
                   trControl = trainControl(method = "cv", number = 5), tuneLength = 10)
knn_model_pred <- predict(knn_model, newdata = valid.df)

confusionMatrix(as.factor(ifelse(knn_model_pred > 0.5, "Positive", "Negative")), 
                as.factor(ifelse(valid.df$Converted==1,"Positive", "Negative")))

knn_prob <- prediction(knn_model_pred, valid.df$Converted)
knn_perf <- performance(knn_prob, "tpr", "fpr")
plot(knn_perf, main = "KNN ROC Curve", col = "red")
abline(0, 1, lty = 2, col = "black")
knn_auc <- performance(knn_prob, measure = "auc")@y.values[[1]]
cat("KNN AUC:", knn_auc, "\n")

############################ Gradient Boosting ##############################################

leads.gbm <- gbm(formula = Converted ~ .,distribution = "bernoulli",
                 data = train.df,n.trees = 1000,interaction.depth = 7,
                 shrinkage = 0.01, cv.folds=3)

leads.iter <- gbm.perf(leads.gbm,method="cv") 

# Prediction
leads.predict <- predict(leads.gbm, valid.df, n.trees = leads.iter,type = "response")

confusionMatrix(as.factor(ifelse(leads.predict > 0.5, "Positive", "Negative")), 
                as.factor(ifelse(valid.df$Converted==1,"Positive", "Negative")))

gm_prob <- prediction(leads.predict, valid.df$Converted)
gm_perf <- performance(gm_prob, "tpr", "fpr")
plot(gm_perf, main = "GB ROC Curve", col = "red")
abline(0, 1, lty = 2, col = "black")
gm_auc <- performance(gm_prob, measure = "auc")@y.values[[1]]
cat("GB AUC:", gm_auc, "\n")

############################ Decision Tree ################################################

dt_model <- rpart(Converted ~ ., data = train.df)
# Make predictions on the testing set
dt_model.pred <- predict(dt_model, valid.df)
confusionMatrix(as.factor(ifelse(dt_model.pred > 0.5, "Positive", "Negative")), 
                as.factor(ifelse(valid.df$Converted==1,"Positive", "Negative")))

dt_prob <- prediction(dt_model.pred, valid.df$Converted)
dt_perf <- performance(dt_prob, "tpr", "fpr")
plot(dt_perf, main = "Decision Tree ROC Curve", col = "red")
abline(0, 1, lty = 2, col = "black")
dt_auc <- performance(dt_prob, measure = "auc")@y.values[[1]]
cat("DT AUC:", dt_auc, "\n")

############################ Random Forest ################################################

rf_model <- randomForest(Converted ~ ., data = train.df, ntree = 500)
# Make predictions on the testing set
rf_model.pred <- predict(rf_model, valid.df)
confusionMatrix(as.factor(ifelse(rf_model.pred > 0.5, "Positive", "Negative")), 
                as.factor(ifelse(valid.df$Converted==1,"Positive", "Negative")))
varImpPlot(rf_model, main = "Random Forest Feature Importance")

rf_prob <- prediction(rf_model.pred, valid.df$Converted)
rf_perf <- performance(rf_prob, "tpr", "fpr")
plot(rf_perf, main = "Random Forest ROC Curve", col = "red")
abline(0, 1, lty = 2, col = "black")
rf_auc <- performance(rf_prob, measure = "auc")@y.values[[1]]
cat("RF AUC:", rf_auc, "\n")

############################ Ensemble Model ################################################

# Build an Ensemble Model with Multiple Types of Models

#Defining the predictors and outcome
predictors<- names(df_dummies)[-which(names(df_dummies) == "Converted")]
outcomeName<-c("Converted")

# Create models
knn_model <- train(Converted ~ ., data = train.df, method = "knn", 
                   trControl = trainControl(method = "cv", number = 10), tuneLength = 10)
logistic_model <- glm(as.formula(paste(outcomeName, "~", paste(predictors, collapse = "+"))), data = train.df, family = binomial)
tree_model <- rpart(as.formula(paste(outcomeName, "~", paste(predictors, collapse = "+"))), data = train.df)
leads.gbm <- gbm(formula = Converted ~ .,distribution = "bernoulli",
                 data = train.df,n.trees = 1000,interaction.depth = 7,
                 shrinkage = 0.01, cv.folds=3)

# Make predictions
knn_pred <- predict(knn_model, newdata = valid.df)
confusionMatrix(as.factor(ifelse(knn_pred > 0.5, "Positive", "Negative")), as.factor(ifelse(valid.df$Converted==1,"Positive", "Negative")))
logistic_pred <- predict(logistic_model, newdata = valid.df, type = "response")
confusionMatrix(as.factor(ifelse(logistic_pred > 0.5, "Positive", "Negative")), as.factor(ifelse(valid.df$Converted==1,"Positive", "Negative")))
tree_pred <- predict(tree_model, newdata = valid.df, type = "vector")
confusionMatrix(as.factor(ifelse(tree_pred > 0.5, "Positive", "Negative")), as.factor(ifelse(valid.df$Converted==1,"Positive", "Negative")))


knn_prob <- prediction(knn_pred, valid.df$Converted)
logistic_prob <- prediction(logistic_pred, valid.df$Converted)
tree_prob <- prediction(tree_pred, valid.df$Converted)
gbm_prob <- prediction(leads.predict, valid.df$Converted)

# Generate ROC curves and calculate AUC scores for each model
knn_perf <- performance(knn_prob, "tpr", "fpr")
logistic_perf <- performance(logistic_prob, "tpr", "fpr")
tree_perf <- performance(tree_prob, "tpr", "fpr")
gbm_perf <- performance(gbm_prob, "tpr", "fpr")

# Plot the ROC curves for each model
plot(knn_perf, main = "ROC Curves", col = "red")
abline(0, 1, lty = 2, col = "black")
plot(logistic_perf, add = TRUE, col = "blue")
plot(tree_perf, add = TRUE, col = "green")
plot(gbm_perf, add = TRUE, col = "pink")

legend("bottomright", c("KNN", "Logistic", "Tree", "GBM"), lty = 1, col = c("red", "blue", "green", "pink"))

# Calculate AUC scores for each model
knn_auc <- performance(knn_prob, measure = "auc")@y.values[[1]]
logistic_auc <- performance(logistic_prob, measure = "auc")@y.values[[1]]
tree_auc <- performance(tree_prob, measure = "auc")@y.values[[1]]
gbm_auc <- performance(gbm_prob, measure = "auc")@y.values[[1]]

# Print AUC scores
cat("k-NN AUC:", knn_auc, "\n")
cat("Logit AUC:", logistic_auc, "\n")
cat("Decision Tree AUC:", tree_auc, "\n")
cat("GBM AUC:", gbm_auc, "\n")

# Averaging ensemble
ensemble_prob <- (knn_pred + logistic_pred + tree_pred + leads.predict) / 4
ensemble_pred <- ifelse(ensemble_prob >= 0.5, "Positive", "Negative")

confusionMatrix(as.factor(ensemble_pred), as.factor(ifelse(valid.df$Converted==1,"Positive", "Negative")))

en_averaging_prob <- prediction(ensemble_prob, valid.df$Converted)
en_averaging_perf <- performance(en_averaging_prob, "tpr", "fpr")
plot(en_averaging_perf, main = "Averaging ensemble ROC Curve", col = "red")
abline(0, 1, lty = 2, col = "black")
en_averaging_auc <- performance(en_averaging_prob, measure = "auc")@y.values[[1]]
cat("Averaging ensemble AUC:", en_averaging_auc, "\n")

# Weighted averaging ensemble
knn_weight <- 1/8
logistic_weight <- 1/8
tree_weight <- 1/4
gbm_weight <- (1/4) * 2

ensemble_weighted_prob <- (knn_weight * knn_pred) + (logistic_weight * logistic_pred) + (tree_weight * tree_pred) + (gbm_weight * leads.predict)
ensemble_weighted_pred <- ifelse(ensemble_weighted_prob >= 0.5, "Positive", "Negative")

confusionMatrix(as.factor(ensemble_weighted_pred), as.factor(ifelse(valid.df$Converted==1,"Positive", "Negative")))

en_weight_prob <- prediction(ensemble_weighted_prob, valid.df$Converted)
en_weight_perf <- performance(en_weight_prob, "tpr", "fpr")
plot(en_weight_perf, main = "Weighted averaging ensemble ROC Curve", col = "red")
abline(0, 1, lty = 2, col = "black")
en_weight_auc <- performance(en_weight_prob, measure = "auc")@y.values[[1]]
cat("Weighted averaging ensemble AUC:", en_weight_auc, "\n")

############################ Neural Networks ###############################################

#install.packages("neuralnet")
library(neuralnet)
library(nnet)

nn <- neuralnet(Converted ~ ., data = train.df, hidden = 3)

# Predict on the validation set
nn.pred <- compute(nn, valid.df[, -1])

# Convert predictions to binary values
nn.pred.bin <- ifelse(nn.pred$net.result > 0.5, 1, 0)

# Evaluate the model on the validation set
confusionMatrix(as.factor(nn.pred.bin), as.factor(valid.df$Converted))

# Load the pROC package
library(pROC)

# Compute the ROC curve and AUC
roc.obj <- roc(valid.df$Converted, nn.pred$net.result)
auc <- auc(roc.obj)

# Plot the ROC curve
plot(roc.obj, main = "ROC Curve for Neural Network Model")

# Add the AUC to the plot
legend("bottomright", paste("AUC =", round(auc, 3)), bty = "n")
#Summarized script
#Study title: "Machine learning-based prediction of clinical outcomes for children during emergency department triage "
#Data analysis: "Tadahiro Goto, Kohei Hasegawa"
#Date: "November/11/2018"

#1. Upload packages
library("e1071")
library("tidyverse")
library("pROC")
library("ModelMetrics")
library("caret")
library(dplyr)
library(DataExplorer)
library( caret)
library(haven)
x <- read_sav("ed2007-spss.sav")
y2007 <- x %>% 
select(AGE,SEX,ARRIVE,TEMPF,PULSE,RESPR,BPSYS,BPDIAS,POPCT,SEEN72,RFV1,IMMED,LEFTBMSE,LEFTAMSE,LEFTAMA,DIEDED,DOA,ADMIT,HDSTAT,ADMITHOS,TRANSFER)%>%
filter(AGE<19)
y2007 <- y2007 %>% 
rename(
  ARREMS = ARRIVE,
  IMMEDR = IMMED,
  LEFTBTRI = LEFTBMSE,
  LEFTATRI = LEFTAMSE,
  TRANOTH = TRANSFER
)
x <- read_sav("ed2008-spss.sav")
y2008 <- x %>% 
select(AGE,SEX,ARRIVE,TEMPF,PULSE,RESPR,BPSYS,BPDIAS,POPCT,SEEN72,RFV1,IMMED,LEFTBMSE,LEFTAMSE,LEFTAMA,DIEDED,DOA,ADMIT,HDSTAT,ADMITHOS,TRANSFER)%>%
filter(AGE<19)
y2008 <- y2008 %>% 
rename(
  ARREMS = ARRIVE,
  IMMEDR = IMMED,
  LEFTBTRI = LEFTBMSE,
  LEFTATRI = LEFTAMSE,
  TRANOTH = TRANSFER)

x <- read_sav("ed2009-spss.sav")
y2009 <- x %>% 
select(AGE,SEX,ARREMS,TEMPF,PULSE,RESPR,BPSYS,BPDIAS,POPCT,SEEN72,RFV1,IMMEDR,LEFTBMSE,LEFTAMSE,LEFTAMA,DIEDED,DOA,ADMIT,HDSTAT,ADMITHOS,TRANOTH)%>%
filter(AGE<19)
y2009 <- y2009 %>% 
rename(
  LEFTATRI = LEFTAMSE
)

x <- read_sav("ed2010-spss.sav")
y2010 <- x %>% 
select(AGE,SEX,ARREMS,TEMPF,PULSE,RESPR,BPSYS,BPDIAS,POPCT,SEEN72,RFV1,IMMEDR,LEFTBTRI,LEFTATRI,LEFTAMA,DIEDED,DOA,ADMIT,HDSTAT,ADMITHOS,TRANOTH)%>%
filter(AGE<19)
x <- read_sav("ed2011-spss.sav")
y2011 <- x %>% 
select(AGE,SEX,ARREMS,TEMPF,PULSE,RESPR,BPSYS,BPDIAS,POPCT,SEEN72,RFV1,IMMEDR,LEFTBTRI,LEFTATRI,LEFTAMA,DIEDED,DOA,ADMIT,HDSTAT,ADMITHOS,TRANOTH)%>%
filter(AGE<19)
x <- read_sav("ed2012-spss.sav")
y2012 <- x %>% 
select(AGE,SEX,ARREMS,TEMPF,PULSE,RESPR,BPSYS,BPDIAS,POPCT,SEEN72,RFV1,IMMEDR,LEFTBTRI,LEFTATRI,LEFTAMA,DIEDED,DOA,ADMIT,HDSTAT,ADMITHOS,TRANOTH)%>%
filter(AGE<19)
x <- read_sav("ed2013-spss.sav")
y2013 <- x %>% 
select(AGE,SEX,ARREMS,TEMPF,PULSE,RESPR,BPSYS,BPDIAS,POPCT,SEEN72,RFV1,IMMEDR,LEFTBTRI,LEFTATRI,LEFTAMA,DIEDED,DOA,ADMIT,HDSTAT,ADMITHOS,TRANOTH)%>%
filter(AGE<19)
x <- read_sav("ed2014-spss.sav")
y2014 <- x %>% 
select(AGE,SEX,ARREMS,TEMPF,PULSE,RESPR,BPSYS,BPDIAS,POPCT,SEEN72,RFV1,IMMEDR,LEFTBTRI,LEFTATRI,LEFTAMA,DIEDED,DOA,ADMIT,HDSTAT,ADMITHOS,TRANOTH)%>%
filter(AGE<19)
x <- read_sav("ed2015-spss.sav")
y2015 <- x %>% 
select(AGE,SEX,ARREMS,TEMPF,PULSE,RESPR,BPSYS,BPDIAS,POPCT,SEEN72,RFV1,IMMEDR,LEFTBTRI,LEFTATRI,LEFTAMA,DIEDED,DOA,ADMIT,HDSTAT,ADMITHOS,TRANOTH)%>%
filter(AGE<19)

mydataset= bind_rows(y2007,y2008)

names(y2009) = names(mydataset)
mydataset= bind_rows(mydataset,y2009)
names(y2010) = names(mydataset)
mydataset= bind_rows(mydataset,y2010)
mydataset= bind_rows(mydataset,y2011)
mydataset= bind_rows(mydataset,y2012)
mydataset= bind_rows(mydataset,y2013)
mydataset= bind_rows(mydataset,y2014)
mydataset= bind_rows(mydataset,y2015)


triaged <- mydataset %>%  #triage darand
filter(IMMEDR==1 |IMMEDR==2 |IMMEDR==3 |IMMEDR==4 |IMMEDR==5)
not_doa <- triaged %>% # ghable vorood namordand 2
filter(DOA == 0 )
entebagh <- not_doa %>%  # entebagh alaeme hayati darand 140
filter(BPDIAS<200)
entebagh <- entebagh %>%  # entebagh alaeme hayati darand 0
filter(BPSYS<300)
entebagh <- entebagh %>%  # entebagh alaeme hayati darand 0
filter(POPCT<=100)
entebagh <- entebagh %>%  # entebagh alaeme hayati darand 28 
filter(PULSE<300)
entebagh <- entebagh %>%  # entebagh alaeme hayati darand 55
filter(RESPR<80)
not_lwbs <- entebagh %>% #tarke ghable triage 351
filter(LEFTBTRI == 0)
not_lwbs <- not_lwbs %>% #tarke bade triage 560
filter(LEFTATRI == 0)
not_lwbs <- not_lwbs %>% #tarke bade shoro darman triage 252
filter(LEFTAMA == 0)

mydataset = not_lwbs
outcome <- ifelse(mydataset$ADMIT==1,1,
          ifelse(mydataset$DIEDED==1,1,
            ifelse(mydataset$HDSTAT==2,1,0)))
mydataset3 <- cbind(mydataset,outcome)
df_models_triage <- mydataset3 %>% 
rename(
  triage = IMMEDR
)

# df_models_triage[,1:22]< sapply(df_models_triage[,1:22],as.character)
df_models_triage[,1:22]< sapply(df_models_triage[,1:22],as.numeric)


#################################################################
## Reference model -- logistic regression with triage variable ##
#################################################################
# Split into training/test sets
n <-  nrow(df_models_triage)
n_train <- round(0.7 * n) # 70% of the dataset
set.seed(1)
train_indices <- sample(1:n, n_train) 
df_train_triage <- df_models_triage[ train_indices, ]  
df_test_triage  <- df_models_triage[-train_indices, ] 

# Fit logistic regression
fit_logistic_triage <- glm(outcome ~ triage,
                         family=binomial(link="logit"), 
                         data=df_train_triage)  
# Prediction in test set
pred_logistic_prob_triage <- predict(fit_logistic_triage, 
                                   newdata = df_test_triage, type = "response")

# ROC 
(roc_logistic_triage <- roc(response = df_test_triage$outcome,
                          predictor= pred_logistic_prob_triage) )
plot(roc_logistic_triage, legacy.axes=TRUE)

# Confusion Matrix -- threshold = prevalence
# prevalence <- mean(as.numeric(df_test_triage$outcome)-1)
prevalence <- quantile(as.numeric(df_test_triage$outcome)-1)
raz = c(1, ) # inja ro ezafe kardam
pred_logistic_class_triage <- ifelse(pred_logistic_prob_triage >=  prevalence, 1, 0) 
confusionMatrix(factor((data = pred_logistic_class_triage),levels=c(1,0)),   #injaro ham taghir dadam
                     factor((reference = df_test_triage$outcome),levels=c(0,1)),
                     mode      = "sens_spec",
                     positive  = "Yes")


# Alternate cut-off for severe class imbalance
(threshold_triage_topleft <- coords(roc_logistic_triage, 
                                  x="best", best.method = "closest.topleft",transpose = TRUE) ) 
pred_triage_class_topleft <- ifelse(pred_logistic_prob_triage >= threshold_triage_topleft[[1]], 1, 0) %>% factor(labels = c("No", "Yes"))
caret::confusionMatrix(factor((data      = pred_triage_class_topleft),levels=1:490),
                     factor((reference = df_test_triage$outcome),levels=1:490),
                     mode      = "sens_spec",
                     positive  = "Yes")


#######################################
## L# Data preparation for ML models ##
#######################################

# Split into training and test sets for ML models
n <-  nrow(df_models_triage)
n_train <- round(0.7 * n) # 70% of the dataset

set.seed(1)
train_indices <- sample(1:n, n_train) # Create a vector of indices which is an 80% random sample
df_train <- df_models_triage[ train_indices, ]  
df_test  <- df_models_triage[-train_indices, ] 

summary(df_train)


###################################################
## Logistic regression with Lasso regularization ##
###################################################
library("glmnet")
# Create matrices of training set
y <- df_train %>% select(outcome) %>% 
data.matrix() -1  # Outcome (factor) is coded as 1 (no) and 2 (yes). Thus, substract by 1
x <- df_train %>% select(-outcome) %>%
data.matrix()

# Prediction in test set
# Create matrices of test set
y_test <- df_test %>% select(outcome)  %>% data.matrix() -1 # Outcome (factor) is coded as 1 (no) and 2 (yes). Thus, substract by 1
x_test <- df_test %>% select(-outcome) %>% data.matrix()

# Sanity check -- no NA alllowed
sum(is.na(y)); sum(is.na(x))

# Fit Lasso in training set with cross validation to identify the best lambda 
set.seed(1)
fit_lasso_cv <- cv.glmnet(x, y, 
                        family = "binomial",
                        type.measure = "mse", nfolds = 10,
                        standardize = TRUE) 

plot(fit_lasso_cv)
fit_lasso_cv$lambda.min # minimal lambda
coef(fit_lasso_cv, s = "lambda.min") # coefficients for min lambda

# Selected variable and plot of coefficients
tbl_varImp_lasso <- as.data.frame(as.matrix(coef(fit_lasso_cv, s = "lambda.min")))
colnames(tbl_varImp_lasso) <- "coefficient"
tbl_varImp_lasso$predictor <- rownames(tbl_varImp_lasso)
tbl_varImp_lasso[-1,] %>%
ggplot(aes(x=reorder(predictor, coefficient), y=coefficient)) +
geom_bar(stat= "identity") +
coord_flip() +
xlab("Predictor") + ylab("Importance")

# Prediction
pred_lasso_prob <- predict(fit_lasso_cv, newx = x_test, 
                         s = "lambda.min", type="response") %>% as.vector()
hist(pred_lasso_prob)
# ROC
(roc_lasso <- roc(response  = y_test, 
                predictor = pred_lasso_prob) )
plot(roc_lasso) 

# Confusion matrix
prevalence <- mean(as.numeric(df_test$outcome)) 
pred_lasso_class <- ifelse(pred_lasso_prob >=  prevalence, 1, 0) %>% factor(labels = c(0, 1))
caret::confusionMatrix(data      = pred_lasso_class, 
                     reference = factor(df_test$outcome),
                     mode      = "sens_spec",
                     positive  = "1")

# Alternate cut-off for severe class imbalance 
( threshold_lasso_topleft <- coords(roc_lasso, 
                                  x="best", best.method = "closest.topleft",transpose = TRUE) )
pred_lasso_class_topleft <- ifelse(pred_lasso_prob >=  threshold_lasso_topleft[[1]], 1, 0) %>% factor(labels = c(0, 1))
caret::confusionMatrix(data      = pred_lasso_class_topleft, 
                     reference = factor(df_test$outcome),
                     mode      = "sens_spec",
                     positive  = "1")



###################
## Random forest ##
###################
library("caret")
set.seed(1)

# Set Training Control
myTrainingControl <- trainControl(method = "cv", 
                                number = 10, 
                                savePredictions = TRUE, 
                                classProbs = TRUE, 
                                verboseIter = FALSE)
# Train RF
library(randomForest)
fit_RF <- caret::train(outcome ~ .,   
                       data = df_train, 
                       method = "ranger", 
                       tuneLength = 3,     
                       importance = "permutation",
                       trControl = myTrainingControl)
print(fit_RF)


# Variable importance + plot
fit_RF$variable.importance

varImp(fit_RF)
tbl_varImp_RF <- varImp(fit_RF)[[1]]  
tbl_varImp_RF$predictor <- rownames(tbl_varImp_RF) 

tbl_varImp_RF %>%
ggplot(aes(x=reorder(predictor, Overall), y=Overall)) +
geom_bar(stat= "identity") +
coord_flip() +
xlab("Predictor") + ylab("Importance")
#names(df_test) = names(df_train)
# Prediction in test set
?predict
# pred_RF_prob <- predict(fit_RF, data = df_test, type="prob")
pred_RF_prob <- predict(fit_RF, df_test)
# hist(pred_RF_prob$Yes)
hist(pred_RF_prob[pred_RF_prob>=0.01])

# ROC
(roc_RF <- roc(response = df_test$outcome,
             predictor= pred_RF_prob) )
plot(roc_RF, legacy.axes=TRUE)


# Confusion Matrix
prevalence <- mean(as.numeric(df_test$outcome)) 
pred_RF_class <- ifelse(pred_RF_prob >= prevalence, 1, 0) %>% factor(labels = c(0,1))
confusionMatrix(data      = pred_RF_class,    
              reference = factor(df_test$outcome),
              mode      = "sens_spec",
              positive  = "1")

# Alternate cut-off for severe class imbalance 
( threshold_RF_topleft <- coords(roc_RF, 
                               x="best", best.method = "closest.topleft",transpose = T) )
pred_RF_class_topleft <- ifelse(pred_RF_prob >= threshold_RF_topleft[[1]], 1, 0) %>% factor(labels = c(0, 1))
caret::confusionMatrix(data      = pred_RF_class_topleft,
                     reference = factor(df_test$outcome),
                     mode      = "sens_spec",
                     positive  = "1")



###########################################################
## Gradient boosted desicion tree (GBDT) -- xgboost:tree ##
###########################################################
library("xgboost")
set.seed(1)
# Set Training Control
myTrainingControl <- trainControl(method = "cv", 
                                number = 10, 
                                savePredictions = TRUE, 
                                classProbs = TRUE, 
                                verboseIter = FALSE) 

fit_xgbTree <- caret::train(outcome ~ ., 
                          data = df_train, 
                          method = "xgbTree", 
                          tuneLength = 3,
                          trControl = myTrainingControl)

print(fit_xgbTree)

# Variable importance + plot
varImp(fit_xgbTree)
tbl_varImp_xgb <- varImp(fit_xgbTree)[[1]] 
tbl_varImp_xgb$predictor <- rownames(tbl_varImp_xgb) 

tbl_varImp_xgb %>%
ggplot(aes(x=reorder(predictor, Overall), y=Overall)) +
geom_bar(stat= "identity") +
coord_flip() +
xlab("Predictor") + ylab("Importance")

# Prediction in test set
pred_xgb_prob <- predict(fit_xgbTree, df_test)

# hist(pred_RF_prob$Yes)
hist(pred_RF_prob[pred_xgb_prob>=0.01])

# ROC
(roc_xgb <- roc(response = df_test$outcome,
              predictor= pred_xgb_prob) )
plot(roc_xgb, legacy.axes=TRUE)

# Confusion Matrix
prevalence <- mean(as.numeric(df_test$outcome))  
pred_xgb_class <- ifelse(pred_xgb_prob >= prevalence, 1, 0) %>% factor(labels = c(0, 1))
confusionMatrix(data      = pred_xgb_class,    
              reference = factor(df_test$outcome),
              mode      = "sens_spec",
              positive  = "1")

# Alternate cut-off for severe class imbalance
( threshold_xgb_topleft <- coords(roc_xgb, 
                                x="best", best.method = "closest.topleft",transpose = T) ) # Another option is "youden"
pred_xgb_class_topleft <- ifelse(pred_xgb_prob >= threshold_xgb_topleft[[1]], 1, 0) %>% factor(labels = c(0, 1))
caret::confusionMatrix(data      = pred_xgb_class_topleft,
                     reference = factor(df_test$outcome),
                     mode      = "sens_spec",
                     positive  = "1")



#############################
## Neural Network by keras ##
#############################
library(keras)
library(tensorflow)
library(corrplot)

# Data to use
df_models_ANN <- df_models_triage

# Split into training and test sets
n <-  nrow(df_models_ANN)
n_train <- round(0.7 * n) 
set.seed(1)
train_indices <- sample(1:n, n_train) 
df_train_ANN <- df_models_ANN[ train_indices, ]  
df_test_ANN  <- df_models_ANN[-train_indices, ] 

# Further split into y (outcome vector) and x (predictors)
y_train_vec <- as.vector(df_train_ANN$outcome)
y_test_vec  <- as.vector( df_test_ANN$outcome)
x_train_tbl <- df_train_ANN %>% select(-outcome)
x_test_tbl  <- df_test_ANN  %>% select(-outcome)
ncol(x_train_tbl)


# Building our Artificial Neural Network
use_session_with_seed(4) 

# Hyperparameters
install_tensorflow(version = '1.14.0')
regularization_factor <- 0.002
opt_adam              <- optimizer_adam (lr = 0.002, beta_1 = 0.90, beta_2 = 0.998, epsilon = 10^-8, decay = 0)
opt_nadam             <- optimizer_nadam(lr = 0.002, beta_1 = 0.90, beta_2 = 0.998, epsilon = 10^-8, schedule_decay = 0.004)
batch_size            <- 2048     
epochs                <- 200

# Structure model for critical care outcome
# (See below for hospitalization outcome)
model_keras <- keras_model_sequential()
model_keras %>% 
# (1) 1st Hidden Layer-------------------------------------------------
layer_dense (units              = 64,
           kernel_initializer = "he_normal", 
           kernel_regularizer = regularizer_l2(regularization_factor), 
           activation         = "relu",    
           input_shape        = ncol(x_train_tbl)) %>%  
layer_dropout (rate = 0.15) %>%  
layer_batch_normalization(axis = -1, momentum = 0.98, epsilon = 0.002) %>%  
# (2) 2nd Hidden Layer-------------------------------------------------
layer_dense (units              = 32,
           kernel_initializer = "he_normal", 
           kernel_regularizer = regularizer_l2(regularization_factor), 
           activation         = "relu") %>% 
layer_dropout (rate = 0.15) %>% 
# (4) 4th Hidden Layer-------------------------------------------------
layer_dense (units              = 24,
           kernel_regularizer = regularizer_l2(regularization_factor), 
           activation         = "relu") %>% 
layer_dropout (rate = 0.15) %>% 
# (5) Output Layer-----------------------------------------------------
layer_dense (units              = 1, 
           kernel_initializer = "glorot_normal", 
           activation         = "sigmoid") %>% 
# (6) Compile Model-----------------------------------------------------
compile (optimizer = opt_adam,
       loss      = 'binary_crossentropy', 
       metrics   = c('accuracy') )
model_keras

# Fit ANN model
system.time ( 
history <- fit (
  object           = model_keras,             
  x                = as.matrix (x_train_tbl), 
  y                = y_train_vec,             
  batch_size       = batch_size,
  epochs           = epochs,     
  validation_split = 0.20) ) 

print (history)

# Predict Model with “Test” Data
# Generates class probabilities as a numeric matrix indicating the probability of being a class
yhat_keras_prob_vec <- predict_proba (object = model_keras, 
                                    x = as.matrix(x_test_tbl)) %>% as.vector()
hist(yhat_keras_prob_vec)

# ROC
(roc_ANN_all <- roc(response  = y_test_vec, 
                  predictor = yhat_keras_prob_vec) )
plot(roc_ANN_all) 

# Confusion matrix
prevalence <- mean(y_test_vec)  # Define the prevalence (ie, cut-off)
pred_ANN_all_class <- ifelse(yhat_keras_prob_vec >= prevalence, 1, 0) %>% factor(labels = c(0, 1))
y_test_factor  <- factor(y_test_vec, labels = c(0, 1))

caret::confusionMatrix(data      = pred_ANN_all_class, 
                     reference = y_test_factor ,
                     mode      = "sens_spec",
                     positive  = "1")

# Alternate cut-off for severe class imbalance (see page 439 in Kuhn's textbook)
( threshold_ANN_all_topleft <- coords(roc_ANN_all, 
                                    x="best", best.method = "closest.topleft",transpose=T) ) 
pred_ANN_all_class_topleft <- ifelse(yhat_keras_prob_vec >= threshold_ANN_all_topleft[[1]], 1, 0) %>% factor(labels = c(0, 1))
y_test_factor  <- factor(y_test_vec, labels = c(0, 1))
caret::confusionMatrix(data      = pred_ANN_all_class_topleft, 
                     reference = y_test_factor ,
                     mode      = "sens_spec",
                     positive  = "1")

# Decision curve analysis
# DCA analysis
dcadata <- df_test %>%select(c(outcome))   
source("dca.r") # This file is avaible at http://www.decisioncurveanalysis.org
library(reshape2)
dcadata$logistic<-as.numeric(pred_logistic_prob_triage)
dcadata$lasso<-as.numeric(pred_lasso_prob)
dcadata$randomforest<-as.numeric(pred_RF_prob$Yes)
dcadata$boosting<-as.numeric(pred_xgb_prob$Yes)
dcadata$neuralnet<-as.numeric(yhat_keras_prob_vec)
data.set <- dcadata

attach(data.set)
data.set$outcome<-as.numeric(data.set$outcome)
data.set$outcome<-data.set$outcome-1

dca(data=data.set, outcome="outcome", 
  predictors=c("logistic", "lasso", "randomforest", "boosting", "neuralnet"), xstart=0, ymin=0)

dcaoutput <- dca(data=data.set, outcome="outcome", 
               predictors=c("logistic", "lasso","randomforest", "boosting",  "neuralnet"), xstart=0,xstop=0.3, ymin=0)

dcadf <- data.frame(dcaoutput$net.benefit)
temp <- melt(dcadf, id="threshold",
           measure=c("logistic", "lasso", "randomforest", "boosting", "neuralnet"))

ggplot(temp,
     aes(x=threshold,
         y=value,
         colour=variable,
         group=variable)) + geom_line() +
coord_cartesian(xlim = c(0, 0.3), ylim= c(0, 0.05)) + 
labs(x="Threshold probability (%)") + labs(y="Net benefit") +
theme_minimal() +
scale_color_discrete(name = "Model", 
                     labels = c("Reference model", 
                                "Logistic regression with Lasso regularization", 
                                "Random forest",
                                "Gradient boosted decision tree",
                                "Deep neural network")) 


#########################################################
## Neural Network by keras for hospitalization outcome ##
#########################################################
# Building our Artificial Neural Network
use_session_with_seed(4) # This line for reproducibility

# Hyperparameters
regularization_factor <- 0.002
opt_adam              <- optimizer_adam (lr = 0.002, beta_1 = 0.90, beta_2 = 0.998, epsilon = 10^-8, decay = 0)
opt_nadam             <- optimizer_nadam(lr = 0.002, beta_1 = 0.90, beta_2 = 0.998, epsilon = 10^-8, schedule_decay = 0.004)
batch_size            <- 1024    
epochs                <- 200

# Structure model
model_keras <- keras_model_sequential()
model_keras %>% 
# (1) 1st Hidden Layer-------------------------------------------------
layer_dense (units              = 32, 
           kernel_initializer = "he_normal", 
           kernel_regularizer = regularizer_l2(0.001), # L2 regularlization
           activation         = "relu",    
           input_shape        = ncol(x_train_tbl)) %>% 
layer_dropout (rate = 0.10) %>%  
# (2) 2nd Hidden Layer-------------------------------------------------
layer_dense (units              = 32,
           kernel_regularizer = regularizer_l2(0.001), 
           activation         = "relu") %>% 
layer_dropout (rate = 0.10) %>%  
# (3) 3rd Hidden Layer-------------------------------------------------
layer_dense (units              = 16,
           kernel_regularizer = regularizer_l2(0.001), 
           activation         = "relu") %>% 
layer_dropout (rate = 0.10) %>%  
# (4) Output Layer-----------------------------------------------------
layer_dense (units              = 1, 
           kernel_initializer = "uniform", 
           activation         = "sigmoid") %>% 
# (5) Compile Model-----------------------------------------------------
compile (optimizer = 'adam', 
       loss      = 'binary_crossentropy', 
       metrics   = c('accuracy') ) 
model_keras

# Fit ANN model
system.time ( 
history <- fit (
  object           = model_keras,             
  x                = as.matrix (x_train_tbl),
  y                = y_train_vec,            
  batch_size       = batch_size,     
  epochs           = epochs,     
  validation_split = 0.20) ) 
print (history)

# Predict Model with “Test” Data
# Generates class probabilities as a numeric matrix indicating the probability of being a class
yhat_keras_prob_vec <- predict_proba (object = model_keras, 
                                    x = as.matrix(x_test_tbl)) %>% as.vector()
hist(yhat_keras_prob_vec)

# ROC
(roc_ANN_all <- roc(response  = y_test_vec, 
                  predictor = yhat_keras_prob_vec) )
plot(roc_ANN_all) 

# Confusion matrix
prevalence <- mean(y_test_vec)  # Define the prevalence 
pred_ANN_all_class <- ifelse(yhat_keras_prob_vec >= prevalence, 1, 0) %>% factor(labels = c(0, 1))
y_test_factor  <- factor(y_test_vec, labels = c(0, 1))

caret::confusionMatrix(data      = pred_ANN_all_class, 
                     reference = y_test_factor ,
                     mode      = "sens_spec",
                     positive  = "1")

# Alternate cut-off for severe class imbalance 
( threshold_ANN_all_topleft <- coords(roc_ANN_all, 
                                    x="best", best.method = "closest.topleft",transposeose=T) ) 
pred_ANN_all_class_topleft <- ifelse(yhat_keras_prob_vec >= threshold_ANN_all_topleft[[1]], 1, 0) %>% factor(labels = c(0, 1))
y_test_factor  <- factor(y_test_vec, labels = c(0, 1))
caret::confusionMatrix(data      = pred_ANN_all_class_topleft, 
                     reference = y_test_factor ,
                     mode      = "sens_spec",
                     positive  = "1")


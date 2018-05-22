# Parts of this code was borrowed from: https://www.r-bloggers.com/customer-analytics-using-deep-learning-with-keras-to-predict-customer-churn/

# Install the following packages in R:
  
# pkgs <- c("keras", "tidyquant", "rsample", "recipes", "yardstick", "corrr", "ggplot2", "caret", "devtools")
# install.packages(pkgs)
# To install lime, magick should be installed as follows:
    
# In terminal execute export PGK_CONFIG_PATH=/opt/local/lib/pkgconfig and in R:
    
# devtools::install_github("ropensci/magick",force=TRUE)
# install.packages('lime')
# Install Keras if you have not installed it before:
    
# library(keras)
# install_keras()
# For further instructions please see https://keras.rstudio.com. To check to see if keras was installed successfully:
    
# is_keras_available() # if the response was TRUE you're all good to go.
  
# LOAD LIBRARIES
library(keras)
library(lime)
library(tidyquant)
library(rsample)
library(recipes)
library(yardstick)
library(corrr)
library(ggplot2)
library(caret) 

# IMPORT DATA
raw_churn_data_df <- read.csv("data/WA_Fn-UseC_-Telco-Customer-Churn.csv")
# 7043 rows
nrow(raw_churn_data_df)
# A glimpse into the data
glimpse(raw_churn_data_df)
summary(raw_churn_data_df)

length(unique(raw_churn_data_df$customerID)) # 7043
# Each customer has a unique customerID, this field doesn't include any useful information
# for our prediction purposes

# PREPROCESS DATA

# Checking for missing value
sapply(raw_churn_data_df, function(x) sum(is.na(x)))
# 11 missing values in TotalCharge

# Remove unnecessary data
churn_data_df <- raw_churn_data_df %>%
  dplyr::select(-customerID) %>% # Dropping customerID column since has no predictive info
  tidyr::drop_na() %>% # This will remove 11 rows where TotalCharge is NA
  dplyr::select(Churn, everything()) # Reordering the columns to keep churn as first

# 7032 rows left
nrow(churn_data_df)

glimpse(churn_data_df)
summary(churn_data_df)

# SPLIT INTO TRAIN/TEST SETS

# Split test/training sets
set.seed(300)
train_test_split <- rsample::initial_split(churn_data_df, prop = 0.75)

# Retrieve train and test sets
train_df <- rsample::training(train_test_split) # 5274 rows
test_df  <- rsample::testing(train_test_split) # 1758 rows

# Transformations

# A quick test is to see if the log transformation increases the magnitude of the correlation 
#   between independent variable x and response variable y
# Determine if log transformation improves correlation 
# between TotalCharges and Churn
train_df %>%
  dplyr::select(Churn, TotalCharges) %>%
  dplyr::mutate(
    Churn = Churn %>% as.factor() %>% as.numeric(),
    LogTotalCharges = log(TotalCharges)
  ) %>%
  corrr::correlate() %>% # similar to stats::cor()
  corrr::focus(Churn) %>% # equivalent to dplyr::select
  corrr::fashion() # converts the correlations into a cleanly formatted fashion
#         rowname Churn
#    TotalCharges  -.20
# LogTotalCharges  -.25

### one-hot encoding:  transforming categorical features into separate features
#     All non-numeric data will need to be converted to dummy variables.
### centering: normalize data to have a mean of zero
### scaling: normalize data to have a standard deviation of one
# ANNs typically perform faster and often times with higher accuracy when the features
#   are scaled and/or normalized (aka centered and scaled, also known as standardizing)
# ANNs use gradient descent, weights tend to update faster after standardization

# We have four features that are multi-category: Contract, Internet Service, Multiple Lines, and Payment Method.

# PREPROCESSING WITH RECIPES
# CREATE A RECIPE
# A recipe is a description of what steps should be applied to a data set in order to get it ready for data analysis.
rec_obj <- recipes::recipe(Churn ~ ., data = train_df) %>% # object = Churn ~ . meaning “Churn” is the outcome and all other features are predictors
  recipes::step_discretize(tenure, options = list(cuts = 6)) %>% #  convert numeric data into a factor with bins having approximately the same number of data points (based on a training set)
  recipes::step_log(TotalCharges) %>% # creates a specification of a recipe step that will log transform data
  recipes::step_dummy(all_nominal(), -all_outcomes()) %>% # creates a a specification of a recipe step that will convert nominal data into one or more numeric binary model terms for the levels of the original data
  recipes::step_center(all_predictors(), -all_outcomes()) %>% # creates a specification of a recipe step that will normalize numeric data to have a mean of zero
  recipes::step_scale(all_predictors(), -all_outcomes()) %>% # creates a specification of a recipe step that will normalize numeric data to have a standard deviation of one
  recipes::prep(data = train_df) # estimates the required parameters from a training set that can be later applied to other data sets

# Print the recipe object
rec_obj

# BAKING TRAIN AND TEST DATASETS WITH THE RECIPE
# We can apply the “recipe” to any data set with the bake() function, and it processes the data following our recipe steps.
# Predictors
x_train_df <- recipes::bake(rec_obj, newdata = train_df)[,-1] # excluding the target variable
x_test_df  <- recipes::bake(rec_obj, newdata = test_df)[,-1] # excluding the target variable

glimpse(x_train_df)

# TARGET (RESPONSE VARIABLE)
# Response variables for training and testing sets
y_train_vec <- ifelse(dplyr::pull(train_df, Churn) == "Yes", 1, 0)
y_test_vec  <- ifelse(dplyr::pull(test_df, Churn) == "Yes", 1, 0)

# Building our Artificial Neural Network
model_keras <- keras::keras_model_sequential() # Keras Model composed of a linear stack of layers

model_keras %>% 
  # First hidden layer
  layer_dense(
    units              = 10, 
    kernel_initializer = "uniform", # random uniform
    activation         = "relu", 
    input_shape        = ncol(x_train_df)) %>%  # Defining the shape of the input to the 1st hidden layer
  # Dropout to prevent overfitting
  layer_dropout(rate = 0.1) %>%
  # Second hidden layer
  layer_dense(
    units              = 6, 
    kernel_initializer = "uniform", # random uniform
    activation         = "relu") %>% 
  # Dropout to prevent overfitting
  layer_dropout(rate = 0.05) %>%
  # Output layer
  layer_dense(
    units              = 1, 
    kernel_initializer = "uniform", # random uniform
    activation         = "sigmoid") %>% 
  # Compile ANN
  compile(
    optimizer = 'adam',
    # Useful read: https://machinelearningmastery.com/adam-optimization-algorithm-for-deep-learning/
    # Original paper on Arxiv: https://arxiv.org/pdf/1412.6980.pdf 
    loss      = 'binary_crossentropy',
    metrics   = c('accuracy')
  )
model_keras

# Fit the keras model to the training data
fit_keras <- fit(
  object           = model_keras, 
  x                = as.matrix(x_train_df), 
  y                = y_train_vec,
  batch_size       = 50, 
  epochs           = 35,
  validation_split = 0.30
)

# Print the final model
fit_keras

# Plot the training/validation history of our Keras model
plot(fit_keras) +
  theme_tq() +
  scale_color_tq() +
  scale_fill_tq() +
  labs(title = "Training Results for a 16x12 Network")

# MAKING PREDICTIONS
# Predicted Class
yhat_keras_class_vec <- predict_classes(object = model_keras, x = as.matrix(x_test_df)) %>%
  as.vector()

# Predicted Class Probability
yhat_keras_prob_vec  <- predict_proba(object = model_keras, x = as.matrix(x_test_df)) %>%
  as.vector()

# INSPECT PERFORMANCE WITH YARDSTICK
# Format test data and predictions for yardstick metrics
estimates_keras_tbl <- tibble(
  truth      = as.factor(y_test_vec) %>% fct_recode(yes = "1", no = "0"),
  estimate   = as.factor(yhat_keras_class_vec) %>% fct_recode(yes = "1", no = "0"),
  class_prob = yhat_keras_prob_vec
)
estimates_keras_tbl

# Plot the distribution of probabilities by the truth
ggplot(as.data.frame(estimates_keras_tbl), aes(x=class_prob, colour=truth)) + geom_density()

# the default is to classify 0 as the positive class instead of 1. The following command will assign 1 to positive
options(yardstick.event_first = FALSE)

# CONFUSION TABLE
# Confusion Table
estimates_keras_tbl %>% conf_mat(truth, estimate)

# ACCURACY
estimates_keras_tbl %>% metrics(truth, estimate)

# AUC
estimates_keras_tbl %>% roc_auc(truth, class_prob)

# sensitivity
sensitivity(estimates_keras_tbl$truth, estimates_keras_tbl$estimate)

# specificity
specificity(estimates_keras_tbl$truth, estimates_keras_tbl$estimate)

# PRECISION AND RECALL
tibble(
  precision = estimates_keras_tbl %>% yardstick::precision(truth, estimate),
  recall    = estimates_keras_tbl %>% yardstick::recall(truth, estimate)
)

# F1-Statistic
estimates_keras_tbl %>% f_meas(truth, estimate, beta = 1)



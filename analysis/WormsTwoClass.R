#--------------------------------------
# This script sets out to build a
# feature-based classifier on 
# WormsTwoClass data from:
# http://www.timeseriesclassification.com/description.php?Dataset=WormsTwoClass
#--------------------------------------

#--------------------------------------
# Author: Trent Henderson, 9 April 2021
#--------------------------------------

# ----------------------- Read in data --------------------

# Load in .arff file and wrangle into tidy format
# NOTE: THIS SHOULD BE ADAPTED TO WEBSCRAPE RATHER THAN USE DOWNLOADED FILES

train <- foreign::read.arff("data/WormsTwoClass_TRAIN.arff") %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = att1:att900, names_to = "timepoint", values_to = "value") %>%
  mutate(timepoint = gsub("att", "\\1", timepoint)) %>%
  mutate(timepoint = as.numeric(timepoint)) %>%
  mutate(id = as.character(id)) %>%
  mutate(id = as.integer(id))

test <- foreign::read.arff("data/WormsTwoClass_TEST.arff") %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = att1:att900, names_to = "timepoint", values_to = "value") %>%
  mutate(timepoint = gsub("att", "\\1", timepoint)) %>%
  mutate(timepoint = as.numeric(timepoint)) %>%
  mutate(id = as.character(id)) %>%
  mutate(id = as.integer(id))

# ----------------------- Calculate features --------------

# Calculations

train_feats <- calculate_features(data = train, id_var = "id", time_var = "timepoint", values_var = "value",
                            feature_set = "catch22")

test_feats <- calculate_features(data = test, id_var = "id", time_var = "timepoint", values_var = "value",
                                  feature_set = "catch22")

# Normalisation

train_norm1 <- train_feats %>%
  dplyr::group_by(names) %>%
  dplyr::mutate(values = (values-mean(values, na.rm = TRUE))/sd(values, na.rm = TRUE)) %>%
  dplyr::ungroup()

test_norm1 <- test_feats %>%
  dplyr::group_by(names) %>%
  dplyr::mutate(values = (values-mean(values, na.rm = TRUE))/sd(values, na.rm = TRUE)) %>%
  dplyr::ungroup()

# Rejoin group labels

get_labs <- function(data1,data2){
  group_labels <- data1 %>%
    group_by(id, target) %>%
    summarise(counter = n()) %>%
    ungroup() %>%
    dplyr::select(-c(counter))
  
  tmp <- data2 %>%
    left_join(group_labels, by = c("id" = "id"))
}

train_norm1 <- get_labs(train,train_norm1)
test_norm1 <- get_labs(test,test_norm1)

# ----------------------- Data vis ------------------------

plot_feature_matrix(train_norm1, is_normalised = TRUE, id_var = "id")
plot_low_dimension(train_norm1, is_normalised = TRUE, id_var = "id", group_var = "target", plot = TRUE)

# ----------------------- Regression prep ----------------

# Create wide dataframe

train_wide <- train_norm1 %>%
  pivot_wider(id_cols = c("id","target"), names_from = "names", values_from = "values") %>%
  mutate(target = as.numeric(target)) %>%
  mutate(target = target-1) %>% # Recode into [0,1]
  mutate(target = as.factor(target)) %>%
  dplyr::select(-c(id))

test_wide <- test_norm1 %>%
  pivot_wider(id_cols = c("id","target"), names_from = "names", values_from = "values") %>%
  mutate(target = as.numeric(target)) %>%
  mutate(target = target-1) %>% # Recode into [0,1]
  mutate(target = as.factor(target)) %>%
  dplyr::select(-c(id))

# ----------------------- Classification -----------------

# Set up 10-fold cross validation procedure with 3 repeats

train_control <- trainControl(method = 'repeatedcv', 
                        number = 10, 
                        repeats = 3, 
                        search = 'grid',
                        )

# Define some mtry parameters to fit over

tunegrid <- expand.grid(.mtry = (1:15))

# Train RandomForest model

set.seed(123)

rf.mod <- train(
  x = train_wide[,-1],
  y = train_wide$target,
  ntree = 1000,
  method = "rf",
  metric = "Accuracy",
  tuneGrid = tunegrid,
  trControl = train_control
)

svm.mod <- train(
  x = train_wide[,-1],
  y = train_wide$target,
  method = "svmLinear",
  metric = "Accuracy",
  trControl = train_control
)

# Retrieve classification accuracy on test set

y_pred_rf <- predict(rf.mod, newdata = test_wide[,-1])
tab_rf <- table(test_wide$target, y_pred_rf)
caret::confusionMatrix(tab_rf)

y_pred_svm <- predict(svm.mod, newdata = test_wide[,-1])
tab_svm <- table(test_wide$target, y_pred_svm)
caret::confusionMatrix(tab_svm)

y_pred_gp <- predict(gp.mod, newdata = test_wide[,-1])
tab_gp <- table(test_wide$target, y_pred_gp)
caret::confusionMatrix(tab_gp)

# Make variable importance plot

CairoPNG("output/varimp.png", 800, 600)
plot(varImp(rf.mod))
dev.off()

# Plot grid search parameters

CairoPNG("output/gridsearch.png", 800, 600)
plot(rf.mod)
dev.off()

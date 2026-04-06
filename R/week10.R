# Script Settings and Resources 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(haven) 
library(tidyverse) 
library(caret) 

# Data Import and Cleaning 
gss_tbl <- read_sav("../data/GSS2016.sav") |> # read_sav turns dataset into tibble (checked with is_tibble()) missing data is read as "NA", appears to be correct
  drop_na(mosthrs) |># Dropping missing values here as per assignment 
  mutate(mosthrs = as.integer(mosthrs)) |> # Set these to integer values instead of doubles 
  select(-hrs1, -hrs2) |> # Getting rid of these variables as per assignment 
  select(where(~ mean(is.na(.)) < 0.75)) |># This call takes the piped data set, calcs the proportion of missingness (mean) and makes sure it is less than 0.75
  mutate(across(everything(), as.numeric)) # So everything in the data set is numeric for ML tasks 
# Visualization 
gss_tbl |> 
  ggplot(aes(x = mosthrs)) + 
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Maximum Hours Worked Last Week",
    subtitle = "GSS 2016 Data",
    x = "Work Hours",
    y = "Number of Respondents"
  ) +
  theme_minimal()

# Analysis 

## Training and test sets 
set.seed(42) # You know, the universal answer to the meaning of life (setting seed for reproducability)
index <- createDataPartition(gss_tbl$mosthrs, p = 0.75, list = FALSE) # Created index to split up data 
train_data <- gss_tbl[index, ] # Training data set
test_data <- gss_tbl[-index, ] # Testing data set

## 10 fold cross-validation 
cv_ten <- trainControl(
  method = "cv", # Set for cross validation 
  number = 10 # sets number of folds 
)

## Different models to test 
### Impute (saves time)
medimp <- "medianImpute"

### OLS model 
ols_model <- train(
  mosthrs ~ ., # Most hours predicted from everything else
  data = train_data, # Only using training data as is convention
  method = "lm", #"lm" for OLS model 
  preProcess =  c("nzv", medimp), #Median imputation, added nsv because caret was throwing an fat error
  trControl = cv_ten, # Used preset cross-validation from above
  na.action = na.pass #allows the NA
)

### Elastic net model 
enet_grid <- expand.grid(
  alpha = seq(0, 1, by = 0.1),
  lambda = seq(0.0001, 0.1, length = 10)
) # Grid parameters for testing different alpha/lambda values

#### Actual model specifications 
en_model <- train(
  mosthrs ~ ., 
  data = train_data, 
  method = "glmnet", # Method for elastic net 
  preProcess =  c("nzv", medimp), 
  tuneGrid = enet_grid, # Grid defined above 
  trControl = cv_ten,
  na.action = na.pass
) # Same args as previous model not commented on 

### Random Forest 
rf_grid <- expand.grid(
  mtry = c(100, 190, 280), #Mtry to force trees to be different, based on number of predictors (apparently convention is p/3 which in this case is 190) I also looked at smaller and bigger trees 
  splitrule = "variance", # Decides on splitting the data based on variance
  min.node.size = 5 # Limits the "growth" of new trees
)

#### Actual model specifications
rf_model <- train(
  mosthrs ~ ., 
  data = train_data, 
  method = "ranger", # Method for random forest 
  preProcess =  c("nzv", medimp), 
  tuneGrid = rf_grid, # Grid defined above
  trControl = cv_ten, 
  na.action = na.pass
) # Args defined in previous models not commented on

### XGboost model 
xgb_grid <- expand.grid(
  nrounds = c(50, 100), # total number of sequential trees, trying multiple options to prevent overfitting
  eta = c(0.01, 0.1), # Learning rate between two options in order to balance learning speed with finding optimal solution (0.3 default lacks sufficient depth) 
  max_depth = c(3, 6), # 3 can find simple interactions, 6 can find complex interactions but may overfit
  subsample = c(0.8, 1), # Percentage of respondents used to build each tree 0.8 to randomly select 80% of participants, 0.1 to have everyone included
  colsample_bytree = c(0.33, 0.66, 1), # Splits predictors into random percentages (apparently 1/3 is rule of thumb here?)
  gamma = 0, # Turns off pruning penalty 
  min_child_weight = 1 # default but caret requires this, minimum node weight
)

#### Actual model specifications 
xgb_model <- train(
  mosthrs ~ ., 
  data = train_data, 
  method = "xgbTree", # Specifies model as xgboost
  preProcess = c("nzv", medimp),
  trControl = cv_ten, 
  tuneGrid = xgb_grid,  # Grid defined above
  na.action = na.pass
) # Args defined in previous models not commented on

###### NOTE: I used an older version of xgboost because apparently caret throws a catestrophic error if I don't, this could have implications in later steps. 

## 10 fold CV estimates (training set)
cv_est <- rbind(
  OLS = getTrainPerf(ols_model),
  ElasticNet = getTrainPerf(en_model),
  RandomForest = getTrainPerf(rf_model),
  XGBoost = getTrainPerf(xgb_model)
)

### Print to check 
print(cv_est)
## Holdout CV estimates (Test set) dataframe
holdout_est <- as.data.frame(rbind(
  OLS = postResample(pred = ols_preds, obs = test_data$mosthrs),
  ElasticNet = postResample(pred = en_preds, obs = test_data$mosthrs),
  RandomForest = postResample(pred = rf_preds, obs = test_data$mosthrs),
  XGBoost = postResample(pred = xgb_preds, obs = test_data$mosthrs)
))

### Print to check 
print(holdout_est)

#### NOTE: used getTrainPerf to look at model performance, postResample as a specific call to resample post training

# Publication 

## Formatting function 
format_ml_assign <- function(x) {
  x_rounded <- sprintf("%.2f", x) # Rounds to two decimals 
  x_no_zero <- str_replace(x_rounded, "^0\\.", ".") # Drop leading zero for positives
  x_final <- str_replace(x_no_zero, "^-0\\.", "-.") # Drop leading zero for negatives (Doesn't occur)
  return(x_final)
} 

## Table 1 tibble 
table1_tbl <- tibble(
  algo = c("OLS regression", "elastic net", "random forest", "eXtreme Gradient Boosting"),
  cv_rsq = format_ml_assign(cv_est$TrainRsquared), # Pull r squared from main cv 
  ho_rsq = format_ml_assign(holdout_est$Rsquared) # Pull r squared from holdout 
) 

## Write csv
  write_csv(table1_tbl, "../out/table1.csv")

# Assignment Questions: 
## 1: How did your results change between models? Why do you think this happened, specifically?
  
### Based on the outputs sen in table 1, it appears that as model 
### complexity goes up, as does prediction. OLS regression performed very 
### poorly with an R^2 of .02/.01. This was improved significantly when 
### Coefficients were penalized in elastic net regression which had an R^2
### of (0.43/0.42). Finally, the two best performing models were random forest
### and xgboost. Both we're comparable at an r^2 of around .6. These likely out
### performed the simpler regressions due the ability to model complex, 
### non-linear relationships
  
## 2: How did your results change between k-fold CV and holdout CV? Why do you think this happened, specifically?
  
### For the linear regression models (OLS/Elastic Net), the holdout R^2 dropped
### slightly compared to the holdout cv R^2. This is expected because models 
### generally perform worse on completely untrained data. Interestingly, for
### random forest and xgboost, the holdout r^2 was actually higher. This could 
### have been due to a mechanical error in my code, or, the hyper parameters
### were overtuned to prevent overfitting.
  
## 3: 
  

  

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
  preProcess = medimp, #Median imputation 
  trControl = cv_ten, # Used preset cross-validation from above
  na.action = na.pass #allows the NA
)

#### Results of model 
print(ols_model$results)


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
  preProcess = medimp, 
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
  preProcess = medimp, 
  tuneGrid = rf_grid, # Grid defined above
  trControl = cv_ten, 
  na.action = na.pass
) # Args defined in previous models not commented on



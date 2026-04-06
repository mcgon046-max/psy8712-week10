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

ols_model <- train(
  mosthrs ~ ., # Most hours predicted from everything else
  data = train_data, # Only using training data as is convention
  method = "lm", #"lm" for OLS model 
  preProcess = medimp, #Median imputation 
  trControl = cv_ten, # Used preset cross-validation from above
  na.action = na.pass #allows the NA
)

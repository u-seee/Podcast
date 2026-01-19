
# Install pacman if needed
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

# Install (if missing) and load packages in one call
pacman::p_load(
  
yardstick,
dials,
workflows,
tidyverse,
janitor,
skimr,
scales,
ggthemes,
kableExtra,
flextable,
paletteer,
patchwork,
ggcorrplot,
GGally,
RPostgres,
doParallel,
future,
readr,
caret,
rsample,
recipes,
tidymodels,
finetune,
stacks,
bonsai,
vip,
xgboost,
here
)


# Import data
train_pod <- read_csv(here("data","podcast_train (1).csv")) %>%
  clean_names() %>%
  select(-id)

str(train_pod)

test_pod <- read_csv(here("data","podcast_test (2).csv")) %>%
  clean_names() %>%
  select(-id)

str(test_pod)

# Import data for submission to kaggle
submit <- read_csv(here("data","podcast_submission (1).csv"))

# Remove the target variable from the submit dataframe leaving only id column
submit_1 <- submit %>%
  select(-Listening_Time_minutes)
submit_1


# Set global theme
theme_set(theme_light())


# Column names of numeric variables
numeric_var <- train_pod %>%
  select(where(is.numeric)) %>%
  colnames()

# Numeric variables excluding target variables
num_non_target <- train_pod %>%
  select(where(is.numeric),  -listening_time_minutes) %>%
  colnames()


# Basic Summary statistics
train_pod %>%
  skim()

test_pod %>%
  skim()

# target variable counts and percentage
train_pod %>%
  tabyl(listening_time_minutes) %>%
  arrange(-n) %>%
  head(20)

# Target variable distribution
train_target_dist <- train_pod %>%
  select(listening_time_minutes) %>%
  ggplot(aes(listening_time_minutes)) +
  geom_histogram(
    aes(listening_time_minutes),
    alpha = 0.6,
    position = "identity",
    bins = 40,
    color = "black",
    fill = "gray80"
  ) +
  geom_rug(aes(color = listening_time_minutes), show.legend = FALSE) +
  labs(title = "Target variable Distridution",
       caption = "Data source: Kaggle.com, Predict Podcast Listening Time",
       x = "Target variable",
       y = "Counts"
       
  ) 
train_target_dist


# Distribution of target variable by genre variable
train_pod %>%
  select(listening_time_minutes, genre) %>%
  ggplot(aes(as.factor(genre), listening_time_minutes, fill = genre)) +
  geom_boxplot(alpha = 0.4, position = "identity", outlier.color = "red") +
  scale_fill_paletteer_d("MoMAColors::Doughton") +
  labs(title = "Distridution of Target variable by Genre",
       caption = "Data source: Kaggle.com, Predict Podcast Listening Time",
       y = "Target variable",
       x = NULL,
       fill = "Genre") +
  coord_flip()

train_pod %>%
  ggplot(aes(listening_time_minutes)) +
  geom_histogram(binwidth = 10, fill = "gray", color = "white") +
  facet_wrap(~ genre, scales = "free_x") +
  theme_minimal()


# Distribution of Target variable by Publication day and Genre
train_pod %>%
  ggplot(aes(y = publication_day, x = listening_time_minutes, fill = genre))+
  geom_boxplot() +
  facet_wrap(~ genre, scales = "free_x") +
  labs(title = "Distridution of Target variable by Publication day and Genre",
       caption = "Data source: Kaggle.com, Predict Podcast Listening Time",
       x = "Target variable",
       y = NULL)

# Distribution of target variable by publication_time and genre
train_pod %>%
  ggplot(aes(x = publication_time, y = listening_time_minutes, fill = genre))+
  geom_boxplot() +
  facet_wrap(~ genre, scales = "free") +
  labs(title = "Distribution of Target variable by Publication time and Genre",
       caption = "Data source: Kaggle.com, Predict Podcast Listening Time",
       x = "Target variable",
       y = NULL)

# Distribution of target variable by episode_sentiment and genre
train_pod %>%
  drop_na() %>%
  ggplot(aes(x = episode_sentiment, y = listening_time_minutes, fill = genre))+
  geom_col() +
  facet_wrap(~ genre, scales = "free") +
  labs(title = "Distribution of Target variable by episode_sentiment and Genre",
       caption = "Data source: Kaggle.com, Predict Podcast Listening Time",
       x = "Target variable",
       y = NULL) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Distribution of all numeric variable | train data
num_dist <- train_pod %>%
  drop_na() %>%
  select(all_of(num_non_target)) %>%
  pivot_longer(cols = everything()) %>%
  ggplot(aes(x = value, fill = name)) +
  geom_histogram() +
  facet_wrap(~ name, scales = "free")
num_dist

# Use skim() to get missing values, filter for missing values and plot showing the 
# missing rate for test and train sets
train_pod %>%
  skim() %>%
  filter(n_missing != 0) %>%
  as_tibble() %>%
  select(skim_variable, n_missing, complete_rate) %>%
  mutate(missing_rate = round(abs(1 - complete_rate) * 100, 1)) %>%
  ggplot(aes(x = fct_reorder(skim_variable, n_missing),
             y = n_missing, fill = skim_variable,
             label = paste0(missing_rate, "%")))+
  geom_col()+
  geom_text(
    size = 3,
    hjust = -0.1,
    vjust = 0.25,
    col = "black"
  ) +
  coord_flip() +
  labs(
    title = "Missing Data rate using skimr package | Train",
    caption = "Data source: Kaggle.com | Predict Podcast Listening Time",
    x = NULL,
    y = NULL
  )



test_pod %>%
  skim() %>%
  filter(n_missing != 0) %>%
  as_tibble() %>%
  select(skim_variable, n_missing, complete_rate) %>%
  mutate(missing_rate = round(abs(1 - complete_rate) * 100, 1)) %>%
  ggplot(aes(x = fct_reorder(skim_variable, n_missing),
             y = n_missing, fill = skim_variable,
             label = paste0(missing_rate, "%")))+
  geom_col()+
  geom_text(
    size = 3,
    hjust = -0.1,
    vjust = 0.25,
    col = "black"
  ) +
  coord_flip() +
  labs(
    title = "Missing Data rate using skimr package | Test",
    caption = "Data source: Kaggle.com | Predict Podcast Listening Time",
    x = NULL,
    y = NULL
  ) # The variables with missing values are all numeric data type and we can check the
    # distribution of all numeric variables in the num_dist plot we built above

## Visually identify outliers for numeric variables in the train and test data
long_train <- train_pod %>%
  select_if(is.numeric) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to =  "Value")
long_train

long_test <- test_pod %>%
  select_if(is.numeric) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to =  "Value")
long_test

clean_train <- long_train %>%
  filter(is.finite(Value)) %>%  # Removes `Inf`, `-Inf`
  drop_na(Value)                # Removes `NA`


# Precompute outlier information for each Variable group for train 
clean_train2 <- clean_train %>%
  group_by(Variable) %>%
  mutate(
    q1 = quantile(Value, 0.25, na.rm = TRUE),
    q3 = quantile(Value, 0.75, na.rm = TRUE),
    iqr = q3 - q1,
    lower = q1 - 1.5 * iqr,
    upper = q3 + 1.5 * iqr,
    is_outlier = Value < lower | Value > upper,
    out_label = ifelse(is_outlier,round(Value, 1), "")
  ) %>%
  ungroup()


#  Create the boxplot with labels for outliers for train
train_outlier <- clean_train2 %>%
  ggplot(aes(x = Variable, y = Value)) +
  geom_boxplot(outlier.colour = "red", outlier.size = 2) +
  geom_text(aes(label = out_label),
    vjust = -0.5, size = 3, color = "black") +
  theme_minimal() +
  labs(
    title = "Boxplot of Numeric Variables to Identify Outliers",
    x = "Variables",
    y = "Values"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
train_outlier


clean_test <- long_test %>%
  filter(is.finite(Value)) %>%  # Removes `Inf`, `-Inf`
  drop_na(Value)                # Removes `NA`

# Precompute outlier information for each Variable group for test
clean_test2  <- clean_test %>%
  group_by(Variable) %>%
  mutate(
    q1 = quantile(Value, 0.25, na.rm = TRUE),
    q3 = quantile(Value, 0.75, na.rm = TRUE),
    iqr = q3 - q1,
    lower = q1 - 1.5 * iqr,
    upper = q3 + 1.5 * iqr,
    is_outlier = Value < lower | Value > upper,
    out_label = ifelse(is_outlier,round(Value, 1), "")
  ) %>%
  ungroup()
  
# #  Create the boxplot with labels for outliers for test
test_outlier <- clean_test2 %>%
  ggplot(aes(x = Variable, y = Value)) +
  geom_boxplot(outlier.colour = "red", outlier.size = 2) +
  geom_text(aes(label = out_label),
    vjust = -0.5, size = 3, color = "black") +
  theme_minimal() +
  labs(
    title = "Boxplot of Numeric Variables to Identify Outliers",
    x = "Variables",
    y = "Values"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
  
test_outlier

pair_plot <- train_pod %>%
  group_by(listening_time_minutes) %>% # to ensure that each unique value of listening_time_minutes gets a sample of 5% of its rows
  slice_sample(prop = 0.05) %>%
  ungroup() %>%
  select_if(is.numeric) %>%
  drop_na() %>%
  ggpairs(
    lower = list(continuous = wrap(
      "smooth",
      alpha = 0.7,
      size = 0.8,
      color = "steelblue"
    )),
    diag = list(continuous = wrap("barDiag", fill = "steelblue", color = "gray80", binwidth = 5)),
    upper = list(continuous = wrap("cor", size = 3))
  )
print(pair_plot)

# Replace extreme values since it may distort the imputation of missing values later
median_episode <- median(train_pod$episode_length_minutes, na.rm = TRUE)
median_number <- median(train_pod$number_of_ads, na.rm = TRUE)

train_pod <- train_pod %>%
  mutate(episode_length_minutes = ifelse(episode_length_minutes > 300,
                                         median_episode,
                                         episode_length_minutes),
         number_of_ads = ifelse(number_of_ads > 11.9,
                                median_number,
                                number_of_ads))

test_pod <- test_pod %>%
  mutate(episode_length_minutes = ifelse(episode_length_minutes > 300,
                                         median_episode,
                                         episode_length_minutes),
         number_of_ads = ifelse(number_of_ads > 11.9,
                                median_number,
                                number_of_ads))

# Investigate episode_length_minutes missingness to know if missingness significantly
# varies across categories using a Chi-squared test
train_pod <- train_pod %>%
  mutate(genre = as.factor(genre))

test_pod1 <- test_pod %>%
  mutate(genre = as.factor(genre))

table(train_pod$genre, is.na(train_pod$episode_length_minutes)) %>%
  chisq.test()
# Since the Chi-Square test resulted in a very low p-value (< 2.2e-16), this 
#indicates a significant association between genre and missingness in episode_length_minutes.
# Recall that the distribution of episode_length_minutes is right skewed.
# Considering all this, we impute the median of episode_length_minutes per genre.

train_pod1 <- train_pod %>% 
  group_by(genre) %>%
  mutate(episode_length_minutes = if_else(is.na(episode_length_minutes),
                                          median_episode,
                                          episode_length_minutes)) %>%
  ungroup()
nrow(train_pod)

# For test
test_pod1 <- test_pod1 %>% 
  group_by(genre) %>% 
  mutate(episode_length_minutes = if_else(is.na(episode_length_minutes),
                                          median_episode,
                                          episode_length_minutes)) %>% 
  ungroup()
str(test_pod1)

# Investigate guest_popularity_percentage missingness to know if missingness significantly
# varies across categories using a Chi-squared test
table(train_pod1$genre, is.na(train_pod$guest_popularity_percentage)) %>%
  chisq.test()

# Since the Chi-Square test resulted in a very low p-value (< 2.2e-16), this 
#indicates a significant association between genre and missingness in guest_popularity_percentage.
# Recall that the distribution of guest_popularity_percentage is relatively a uniform distribution.
# Considering all this, we impute the mean of guest_popularity_percentage per genre.
mean_percentage <- mean(train_pod1$guest_popularity_percentage, na.rm=TRUE)

train_pod1 <- train_pod1 %>%
  group_by(genre) %>%
  mutate(guest_popularity_percentage =           
    if_else(is.na(guest_popularity_percentage),
    mean_percentage,
    guest_popularity_percentage)) %>%
  ungroup()
nrow(train_pod1)

# For test set
test_pod1 <- test_pod1 %>%
  group_by(genre) %>%
  mutate(guest_popularity_percentage =            
    if_else(is.na(guest_popularity_percentage),       
    mean_percentage,
    guest_popularity_percentage)) %>%
  ungroup()
nrow(test_pod1)

# Distribution showing the count of each unique value in number_of_ads
barplot(table(train_pod1$number_of_ads), col = "lightblue", border = "black",
        main = "Bar Chart of Number of Ads")

# Replace the single missing value of number_of_ads with the mode of the distribution
mode <- as.numeric(names(sort(table(train_pod1$number_of_ads),decreasing=TRUE)[1]))

train_pod1 <- train_pod1 %>%
  mutate(number_of_ads = coalesce(number_of_ads, mode))

# Convert all character variables to factor
train_pod1 <- train_pod1 %>%
  mutate(across(.cols = where(is.character), as.factor))

test_pod1 <- test_pod1 %>%
  mutate(across(.cols = where(is.character), as.factor))
nrow(test_pod1)


# Create a variable that indicates if publication_day falls on weekends
train_pod1 <- train_pod1 %>%
  mutate(is_weekend = factor(if_else(publication_day %in% c("Saturday", "Sunday"),1,0)))

test_pod1 <- test_pod1 %>%
  mutate(is_weekend = factor(if_else(publication_day %in% c("Saturday", "Sunday"),1,0)))

# Turn the values of episode_length_minutes into categories
train_pod1 <- train_pod1 %>%
  mutate(episode_category = factor(case_when(
    episode_length_minutes < 20 ~ "short",
    episode_length_minutes >= 20 & episode_length_minutes < 60 ~ "medium",
    TRUE ~ "long"
  )))

test_pod1 <- test_pod1 %>%
  mutate(episode_category = factor(case_when(
    episode_length_minutes < 20 ~ "short",
    episode_length_minutes >= 20 & episode_length_minutes < 60 ~ "medium",
    TRUE ~ "long"
  )))

# create a variable called podcast frequency
podcast_freq <- train_pod1 %>%
  count(podcast_name, name = "pod_fre")

train_pod1 <- train_pod1 %>%
  left_join(podcast_freq, by = "podcast_name")


test_pod1 <- test_pod1 %>%
  left_join(podcast_freq, by = "podcast_name")


# Create  Host vs. Guest Popularity Difference and Ratio to gauge relative influence.
train_pod1 <- train_pod1 %>%
  mutate(
    host_guest_diff = host_popularity_percentage - guest_popularity_percentage,
    host_guest_ratio = host_popularity_percentage / (guest_popularity_percentage + 1)   # Avoid division by zero
  )

test_pod1 <- test_pod1 %>%
  mutate(
    host_guest_diff = host_popularity_percentage - guest_popularity_percentage,
    host_guest_ratio = host_popularity_percentage / (guest_popularity_percentage + 1)
  )

# Create a variable that shows the ratio of number_of_ads to episode_length_minutes
train_pod1 <- train_pod1 %>%
  mutate(ad_density = number_of_ads / (episode_length_minutes + 1))
test_pod1 <- test_pod1 %>%
  mutate(ad_density = number_of_ads / (episode_length_minutes + 1))


# Create a variable showing ratio of guest_popularity_percentage and episode_length_minutes
train_pod1 <- train_pod1 %>%
  mutate( epi_guest = guest_popularity_percentage /(episode_length_minutes + 1))

test_pod1 <- test_pod1 %>%
  mutate( epi_guest = guest_popularity_percentage /(episode_length_minutes + 1))

# Create a variable showing ratio of host_popularity_percentage and episode_length_minutes
train_pod1 <- train_pod1 %>%
  mutate(epi_host = host_popularity_percentage /(episode_length_minutes + 1))

test_pod1 <- test_pod1 %>%
  mutate(epi_host = host_popularity_percentage /(episode_length_minutes + 1))



# Convert number_of_ads to integer
train_pod1 <- train_pod1 %>%
  mutate(number_of_ads = as.integer(number_of_ads))

test_pod1 <- test_pod1 %>%
  mutate(number_of_ads = as.integer(number_of_ads))



# Data Splitting
set.seed(1957)

train_split <- initial_split(train_pod1, prop =0.7, strata = "listening_time_minutes")
pod_train <- training(train_split)
pod_test <- testing(train_split)
nrow(pod_train)
nrow(pod_test)

# Generate 3 folds of cross validation for pod_train
set.seed(405)

folds_cv <- vfold_cv(data = pod_train, v = 3)
folds_cv

# Check the number of of columns in each fold to avoid variability across fold
# This will help you when tuning mtry hyperparameter
map(folds_cv$splits, ~ ncol(analysis(.x))) # this returns 19 for each fold, so 
                                          #  your mtry tuning range should'nt be                                           # > 19

# Levels of factor variables for each fold
map(folds_cv$splits, ~ map(analysis(.x), ~ if (is.factor(.)) levels(.)))



# Build a recipe
recipe_pod <- recipe(listening_time_minutes ~ ., data = pod_train) %>%
  update_role(episode_title, new_role = "ID") %>%
  step_nzv(all_predictors()) %>% 
  step_other(all_nominal_predictors(), threshold = 0.05) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_YeoJohnson(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors())
recipe_pod

# # Train the recipe on pod_train
recipe_prep <- recipe_pod %>%
  prep(training = pod_train)
recipe_prep

# To get a dataframe of pod_train that reflects the pre-processing steps in the
# recipe, we use bake()
bake_train <- recipe_prep %>%
  bake(new_data = NULL) # baked_train will have more number of variables because of step_dummy in the recipe
str(bake_train)

# Apply the recipe on the original test data ie test_pod1
bake_test <- recipe_prep %>%
  bake(new_data = test_pod1)
str(bake_test)


# model Spec
rf_spec_reg <- rand_forest(mtry = tune(), trees = tune(), min_n = tune()) %>%
  set_engine("ranger") %>%
  set_mode("regression")

rf_workflow_reg <- workflow() %>%
  add_recipe(recipe_pod) %>%
  add_model(rf_spec_reg)

## Define tuning grid using grid_latin_hypercube() because train set is large
rf_params <- parameters(
  dials::mtry(range = c(2, 10)),
  dials::trees(range = c(200, 500)), 
  dials::min_n(range = c(2, 10))
)

rf_grid_lhs <- grid_space_filling(
  rf_params,
  size = 3
)



# Tune
rf_tune_reg <- tune_grid(
  rf_workflow_reg,
  resamples = folds_cv,
  grid = rf_grid_lhs,
  control = control_grid(save_pred = TRUE, save_workflow = TRUE, verbose = TRUE)
)

rf_tune_reg

# Save the tuning output because tuning large dataset takes long
saveRDS(rf_tune_reg, "rf_tune_results.rds")

# Explore the results
collect_metrics(rf_tune_reg)
show_best(rf_tune_reg, metric = "rmse")

# Select the best the best model and finalize your workflow
best_rf <- select_best(rf_tune_reg, metric = "rmse")
best_rf

final_rf_workflow <- finalize_workflow(rf_workflow_reg, best_rf)

# Final random forest model
final_rf_fit <- fit(final_rf_workflow, data = pod_train)

# Save model so as not to rerun it when in a new session
saveRDS(final_rf_fit, "final_rf_model.rds") 



# Prediction on test set from initial_split()
prediction <- predict(final_rf_fit, pod_test)
prediction

# Combine pod_test with prediction
pod_test_pred <- pod_test %>%
  bind_cols(prediction)

# Calculate RMSE of pod_test
test_rmse <- pod_test_pred %>%
  rmse(truth=listening_time_minutes, estimate=.pred)
test_rmse

# Predict on original test set
predic <- predict(final_rf_fit, test_pod1)
predic



# Join the prediction to submit_1 for kaggle submission
rf_pred1 <- submit_1 %>%
  mutate(listening_time_minutes = predic$.pred) 
  

# Convert to csv for kaggle submission
rf_pred1 %>%
  write_csv("rf_submit.csv")
 # This model gave a private score of 12.80538 on kaggle


## Xgboost
# Define and tune xgb
xgb_spec_reg <- boost_tree(tree_depth=tune(), trees = tune(), learn_rate = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

xgb_workflow_reg <- workflow() %>%
  add_recipe(recipe_pod) %>%
  add_model(xgb_spec_reg)

## Define tuning grid using grid_latin_hypercube() because train set is large
rf_params <- parameters(
  dials::tree_depth(range = c(3, 13)),
  dials::trees(range = c(100, 500)), 
  dials::learn_rate(range = c(0.001, 0.3))
)

xgb_grid_lhs <- grid_space_filling(
  rf_params,
  size = 3
)


# Tune
xgb_tune_reg <- tune_grid(
  xgb_workflow_reg,
  resamples = folds_cv,
  grid = xgb_grid_lhs,
  control = control_grid(save_pred = TRUE, save_workflow = TRUE, verbose = TRUE)
)

# Save the tuning output beacuse tuning large datasets takes long
saveRDS(xgb_tune_reg, "xgb_tune_results.rds")


# Explore the results
collect_metrics(xgb_tune_reg)
show_best(rf_tune_reg, metric = "rmse")


# Select the best the best model and finalize your workflow
best_xgb <- select_best(xgb_tune_reg, metric = "rmse")
best_xgb

# Finalize workflow
final_xgb_workflow <- finalize_workflow(xgb_workflow_reg, best_xgb)


# Final xgb model
final_xgb_fit <- fit(final_xgb_workflow, data = pod_train)

# Save model so as not to rerun it when in a new session
saveRDS(final_xgb_fit, "final_xgb_model.rds") 


# Prediction on test set from initial_split()
prediction_xgb <- predict(final_xgb_fit, pod_test)
prediction_xgb


# Combine pod_test with prediction
pod_test_pred1 <- pod_test %>%
  bind_cols(prediction_xgb)



# Calculate RMSE of pod_test
test_rmse1 <- pod_test_pred1 %>%
  rmse(truth=listening_time_minutes, estimate=.pred)
test_rmse1

# Predict on original test set
predic1 <- predict(final_xgb_fit, test_pod1)
predic1



# Join the prediction to submit_1 for kaggle submission
xgb_pred1 <- submit_1 %>%
  mutate(listening_time_minutes = predic1$.pred)

xgb_pred1 %>%
  write_csv("xgb_submit.csv")
# This model gave a private score of 13.39505 on kaggle


## Stack the Models
# Create a data stack
ensemble_stk_reg <- stacks() %>%
  add_candidates(rf_tune_reg) %>%
  add_candidates(xgb_tune_reg)

# Disable parallel processing to save memory for stacking process
plan(sequential) 


# Blend predictions first (without model specification)
ensemble_stk_reg <- ensemble_stk_reg %>%
  blend_predictions(penalty = 10^seq(-3, 0, length = 10)) 

# Explicitly use linear regression as the meta-learner
stack_fit_reg <- ensemble_stk_reg %>%
  fit_members()

# Save model so as not to rerun it when in a new session
saveRDS(stack_fit_reg, "final__model.rds") 


# Save with xz compression (highest level) to make the size less than 2gb so that it can be pushed to github
saveRDS(stack_fit_reg, file = "final__model_compressed.rds", compress = "xz", compression_level = 9)

# Check new size
file.size("final__model_compressed.rds") / (1024^3)  # In GB

# Visualize the results of the meta learner
autoplot(stack_fit_reg) 
  #The result shows that higher penalties eliminate weaker models(num_members)
  #The model performs best with minimal regularization or penalty (rmse)
  # Stronger penalty reduces model's ability to explain variance (rsq)

# See the model contributions or weights in the stacking
print(stack_fit_reg)



# Generate predictions on original test set
predictions1 <- test_pod1 %>%
  mutate(.pred = predict(stack_fit_reg, new_data = test_pod1)$.pred)
str(predictions1)



# Join the prediction to submit_1 for kaggle submission
stack_pred1 <- submit_1 %>%
  mutate(listening_time_minutes = predictions1$.pred)

stack_pred1 %>%
  write_csv("stack_submit.csv")
# This model gave a private score of 12.78835 on kaggle.


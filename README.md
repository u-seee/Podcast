Podcast Listening Time Prediction
Predicting how many minutes a listener will spend on a podcast episode
End-to-end regression project in R using the tidymodels ecosystem for the Kaggle competition:
Predict Podcast Listening Time.

Dataset

podcast_train.csv →  Training data
podcast_test.csv → Test data for submission
Target: listening_time_minutes (continuous, right-skewed)


Key Highlights

Extensive EDA: Target distribution, genre-wise boxplots & histograms, missing data analysis, outlier detection, full pair plots (GGally)
Smart Feature Engineering:
Genre-specific imputation for missing values (episode_length_minutes, guest_popularity_percentage)
Capped extreme values in episode_length_minutes and number_of_ads
New features: is_weekend, episode_category (short/medium/long), podcast_freq, host_guest_diff, host_guest_ratio, ad_density, epi_guest, epi_host

Preprocessing: tidymodels recipe with NZV removal, step_other(), one-hot encoding, Yeo-Johnson transformation, and normalization
Models:
Tuned Random Forest (ranger)
Tuned XGBoost
Stacked Ensemble (best performer)



Final Results (Private Kaggle Score)
Model,Private Score
Random Forest,12.805
XGBoost,13.395
Stacked Ensemble,12.788

Technologies

Core: tidymodels, ranger, xgboost, stacks, finetune
EDA & Viz: tidyverse, GGally, ggcorrplot, patchwork, vip
Others: janitor, skimr, here, pacman

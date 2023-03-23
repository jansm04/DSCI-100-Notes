install.packages("tidymodels")

# load packages
library(tidyverse)
library(tidymodels) # needed for knn classification

install.packages("parsnip")
install.packages("kknn")
library(kknn)


# =======================================
# CHAPTER 7: REGRESSION 1: KNN REGRESSION
# =======================================



sacramento <- read_csv("data/sacramento.csv")
sacramento <- sacramento |> filter(sq__ft != 0)
glimpse(sacramento)


set.seed(5)









# Using One Predictor:

#   - we can do this using mutate() and summarize()

small_sacramento <- slice_sample(sacramento, n = 30)
small_sacramento

sacramento_plot <- small_sacramento |> ggplot(aes(x = sq__ft, y = price)) +
  geom_point(alpha = 0.5) +
  labs(x = "Square Feet", y = "Price (USD)") +
  scale_y_continuous(labels = dollar_format())
sacramento_plot


nearest_neighbors <- small_sacramento |>
  mutate(dist = abs(2000 - sq__ft)) |>
  arrange(dist) |>
  slice(1:5)
nearest_neighbors

prediction <- nearest_neighbors |>
  summarize(price_prediction = mean(price))
prediction















# Regression Process:

# 1) initial_split()
sacramento_split <- initial_split(sacramento, prop = 0.75, strata = price)
sacramento_training <- training(sacramento_split)
sacramento_testing <- testing(sacramento_split)


# NOTE: we can not measure accuracy in regression, since we are dealing with numerical values - so, we must
#       use RMSPE (Root Mean Square Prediction Error)
#       - here we compute the 'accuracy' by averaging the distance between each prediction and its
#         actual value
# also:
#       - predicting on training data = RMSE
#       - predicting on testing data = RMSPE



# 2) vfold_cv()
sacr_vfold <- vfold_cv(sacramento_training, v = 5, strata = price)



# 3) recipe
sacr_recipe <- recipe(price ~ sq__ft, data = sacramento_training) |>
  step_scale(all_predictors()) |>
  step_center(all_predictors())



# 4) nearest_neighbors() + tune()
knn_spec <- nearest_neighbor(weight_func = "rectangular", neighbors = tune()) |>
  set_engine("kknn") |>
  set_mode("regression")
# NOTE: since we are performing regression, we pass "regression" into set_mode() parameter


# 5) workflow() + tune_grid()
grid_vals <- tibble(neighbors = seq(from = 1, to = 200, by = 3))

sacr_results <- workflow() |>
  add_recipe(sacr_recipe) |>
  add_model(knn_spec) |>
  tune_grid(resamples = sacr_vfold, grid = grid_vals) |>
  collect_metrics() |>
  filter(.metric == "rmse")
sacr_results
# since were working on the training data, this would be RMSE

sacr_min <- sacr_results |> filter(mean == min(mean))
k_min <- sacr_min |> pull(neighbors)
k_min


# 6) New model with best K
knn_best_spec <- nearest_neighbor(weight_func = "rectangular", neighbors = k_min) |>
  set_engine("kknn") |>
  set_mode("regression")

sacr_fit <- workflow() |>
  add_recipe(sacr_recipe) |>
  add_model(knn_best_spec) |>
  fit(data = sacramento_training)


# 7) Make predictions
predictions <- predict(sacr_fit, sacramento_testing) |>
  bind_cols(sacramento_testing) |>
  metrics(truth = price, estimate = .pred) |>
  filter(.metric == "rmse") |>
  pull(.estimate)
predictions
# make sure to have .pred NOT .pred_class
# since were working on the testing data, this would be RMSPE









# Strengths

#   - is a simple, intuitive algorithm
#   - does not require many assumptions about what the data might look like
#   - good for non linear relationships

# Weaknesses

#   - slow as data gets bigger
#   - performs poorly for large number of predictors
#   - predicts poorly beyond range of values inputted in data






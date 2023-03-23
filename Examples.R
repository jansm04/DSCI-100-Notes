# load packages
library(tidyverse)
library(tidymodels) # needed for knn classification

library(kknn)






# ==============================================================================================
# CLASSIFICATION
# ==============================================================================================
# ==============================================================================================
# TRAINING

# read data
fruit <- read_csv("data/fruit_data.csv") |>
  mutate(fruit_name = as_factor(fruit_name))
fruit

# observe data
fruit |>
  ggplot(aes(x = height, y = width, color = fruit_name)) +
  geom_point()

# split data in training and testing sets
fruit_split <- initial_split(data = fruit, prop = 0.8, strata = fruit_name)
fruit_train <- training(fruit_split)
fruit_test <- testing(fruit_split)

# specify number of folds
fruit_vfold <- vfold_cv(data = fruit_train, v = 5, strata = fruit_name)

# specify model
knn_spec <- nearest_neighbor(weight_func = "rectangular", neighbors = tune()) |>
  set_engine("kknn") |>
  set_mode("classification")

# build recipe
fruit_recipe <- recipe(fruit_name ~ height + width, data = fruit_train) |>
  step_scale(all_predictors()) |>
  step_center(all_predictors())

# create a column of K values
k_vals <- tibble(neighbors = seq(from = 1, to = 10, by = 1))

# compute the best K
best_k <- workflow() |>
  add_recipe(fruit_recipe) |>
  add_model(knn_spec) |>
  tune_grid(resamples = fruit_vfold, grid = k_vals) |>
  collect_metrics() |>
  filter(.metric == "accuracy") |>
  filter(mean == max(mean)) |>
  pull(neighbors)

# observe neighbors vs accuracy plot
accuracies |>
  ggplot(aes(x = neighbors, y = mean)) +
  geom_point() +
  geom_line()

# specify new model with best K value
knn_best_spec <- nearest_neighbor(weight_func = "rectangular", neighbors = best_k) |>
  set_engine("kknn") |>
  set_mode("classification")

# fit knn model with best specs
fruit_fit <- workflow() |>
  add_recipe(fruit_recipe) |>
  add_model(knn_best_spec) |>
  fit(data = fruit_train)


# ==============================================================================================
# TESTING

# get predictions on test data
predictions <- predict(fruit_fit, fruit_test) |>
  bind_cols(fruit_test)

# evaluate accuracy of classifier using test data
accuracy <- predictions |>
  metrics(truth = fruit_name, estimate = .pred_class) |>
  filter(.metric == "accuracy") |>
  pull(.estimate)

# check confusion matrix of classifier with test data
mat <- predictions |>
  conf_mat(truth = fruit_name, estimate = .pred_class)

# ==============================================================================================


















# ==============================================================================================
# REGRESSION
# ==============================================================================================
# ==============================================================================================
# TRAINING

diamonds_small <- slice_sample(diamonds, n = 400)

diamonds_small |> ggplot(aes(x = carat, y = price)) +
  geom_point(alpha = 0.5) +
  labs(x = "Carat", y = "Price")



diamonds_split <- initial_split(diamonds_small, prop = 0.75, strata = price)
diamonds_training <- training(diamonds_split)
diamonds_testing <- testing(diamonds_split)

knn_spec <- nearest_neighbor(weight_func = "rectangular", neighbors = tune()) |>
  set_engine("kknn") |>
  set_mode("regression")

diamonds_recipe <- recipe(price ~ carat, data = diamonds_training) |>
  step_scale(all_predictors()) |>
  step_center(all_predictors())

diamonds_vfold <- vfold_cv(data = diamonds_training, v = 5, strata = price)

k_vals <- tibble(neighbors = seq(from = 1, to = 200, by = 3))

diamonds_results <- workflow() |>
  add_recipe(diamonds_recipe) |>
  add_model(knn_spec) |>
  tune_grid(resamples = diamonds_vfold, grid = k_vals) |>
  collect_metrics() |>
  filter(.metric == "rmse")
diamonds_results


k_plot <- diamonds_results |>
  ggplot(aes(x = neighbors, y = mean)) +
  geom_point() +
  geom_line()
k_plot

k_min <- diamonds_results |>
  filter(mean == min(mean)) |>
  pull(neighbors)

k_min



knn_best_spec <- nearest_neighbor(weight_func = "rectangular", neighbors = k_min) |>
  set_engine("kknn") |>
  set_mode("regression")

knn_fit <- workflow() |>
  add_recipe(diamonds_recipe) |>
  add_model(knn_best_spec) |>
  fit(data = diamonds_training)

# ==============================================================================================
# TESTING



predictions <- predict(knn_fit, diamonds_testing) |>
  bind_cols(diamonds_testing) |>
  metrics(truth = price, estimate = .pred) |>
  filter(.metric == "rmse")
predictions


# ==============================================================================================




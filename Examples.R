# load packages
library(tidyverse)
library(tidymodels) # needed for knn classification

library(kknn)


marathon <- read_csv("data/marathon_small.csv")
marathon

marathon_plot <- marathon |>
  ggplot(aes(x = bmi, y = km5_time_seconds)) +
  geom_point() +
  labs(x = "BMI", y = "5K Race Time")
marathon_plot

set.seed(5)


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

predictions <- predict(knn_fit, diamonds_testing) |>
  bind_cols(diamonds_testing) |>
  metrics(truth = price, estimate = .pred) |>
  filter(.metric == "rmse")
predictions


predict_diamonds <- tibble(carat = seq(from = 0.1, to = 2.5, by = 0.01))

predictions_2 <- predict(knn_fit, predict_diamonds) |>
  bind_cols(predict_diamonds)
View(predictions_2)

plot_final <- ggplot(diamonds_training, aes(x = carat, y = price)) +
  geom_point(alpha = 0.4) +
  geom_line(data = predictions_2,
            mapping = aes(x = carat, y = .pred),
            color = "blue") +
  xlab("Carat") +
  ylab("Price") +
  scale_y_continuous(labels = dollar_format()) +
  ggtitle(paste0("K = ", k_min)) +
  theme(text = element_text(size = 12))
plot_final


ggsave("data/diamonds_regression_plot.png", plot_final)

predictions_2 |> filter(carat == 1.06) |>
  pull(.pred)







wdbc<- read_csv("data/wdbc.csv") |>
  mutate(diagnosis = as_factor(diagnosis))

wdbc |> ggplot(aes(x = fractal_dimension_mean, y = radius_mean, color = diagnosis)) +
  geom_point()



cancer_split <- initial_split(data = wdbc, prop = 0.8, strata = diagnosis)
cancer_train <- training(cancer_split)
cancer_test <- testing(cancer_split)


knn_spec <- nearest_neighbor(weight_func = "rectangular", neighbors = tune()) |>
  set_engine("kknn") |>
  set_mode("classification")

cancer_recipe <- recipe(diagnosis ~ fractal_dimension_mean + radius_mean, data = cancer_train) |>
  step_scale(all_predictors()) |>
  step_center(all_predictors())

cancer_vfold <- vfold_cv(data = cancer_train, k = 5, strata = diagnosis)

grid_vals <- tibble(neighbors = seq(from = 1, to = 200, by = 4))

cancer_results <- workflow() |>
  add_recipe(cancer_recipe) |>
  add_model(knn_spec) |>
  tune_grid(resamples = cancer_vfold, grid = grid_vals) |>
  collect_metrics()

cancer_results |>
  filter(.metric == "accuracy") |>
  filter(mean == min(mean)) |>
  pull(neighbors)






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




marathon <- marathon |>
  slice_sample(n = 400)

marathon |>
  ggplot(aes(x = age, y = km10_time_seconds, color = sex)) +
  geom_point()

marathon_split <- initial_split(data = marathon, prop = 0.75, strata = sex)
marathon_train <- training(marathon_split)
marathon_test <- testing(marathon_split)

marathon_vfold <- vfold_cv(data = marathon_train, v = 5, strata = sex)

marathon_recipe <- recipe(sex ~ km10_time_seconds, data = marathon_train) |>
  step_scale(all_predictors()) |>
  step_center(all_predictors())

knn_reg <- nearest_neighbor(weight_func = "rectangular", neighbors = tune()) |>
  set_engine("kknn") |>
  set_mode("regression")

k_vals <- tibble(neighbors = seq(from = 1, to = 5, by = 1))

best_k <- workflow() |>
  add_recipe(marathon_recipe) |>
  add_model(knn_reg) |>
  tune_grid(resamples = marathon_vfold, grid = k_vals)

best_k






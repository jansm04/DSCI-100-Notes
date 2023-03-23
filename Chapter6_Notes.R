install.packages("tidymodels")

# load packages
library(tidyverse)
library(tidymodels) # needed for knn classification

install.packages("parsnip")
install.packages("kknn")
library(kknn)


# ==================================================
# CHAPTER 6: CLASSIFICATION 2: EVALUATION AND TUNING
# ==================================================




# Creating a Training and Test Set:

#   - needed to determine whether or not classifier is accurate
#   - must ONLY use training set to make model
#   - can NOT use test set to make model

#   - prediction accuracy = (number of correct predictions) / (number of total predictions)

#   - when splitting data, we need to do it randomly (in case variables are arranged in groups)
#   - to do this, we need to use SEEDS

# Ex:
set.seed(1)
random_numbers1 <- sample(0:9, 10, replace=TRUE)
random_numbers1

random_numbers2 <- sample(0:9, 10, replace=TRUE)
random_numbers2

set.seed(1)
random_numbers3 <- sample(0:9, 10, replace=TRUE)
random_numbers3

random_numbers4 <- sample(0:9, 10, replace=TRUE)
random_numbers4

# each seed has its own random combination (^ code above will produce the same sequence of outputs)

set.seed(3245)
random_numbers5 <- sample(0:9, 10, replace=TRUE)
random_numbers5

random_numbers6 <- sample(0:9, 10, replace=TRUE)
random_numbers6

# here we get a new combination because we have a new seed

# NOTE: if you want your analysis to be reproducible, set the seed only once at the top










# Initial Split Function:

set.seed(1)

cancer <- read_csv("data/wdbc.csv") |>
  mutate(diagnosis = as_factor(diagnosis))

cancer_plot <- cancer |>
  ggplot(aes(x = perimeter_mean, y = concavity_mean)) +
  geom_point(alpha = 0.5) +
  labs(x = "Perimeter", y = "Concavity", color = "Diagnosis") +
  scale_color_manual(labels = c("Malignant", "Benign"),
                     values = c("orange2", "steelblue2")) +
  theme(text = element_text(size = 12))
perim_concav
# view the predictors were working with


# Ex:
cancer_split <- initial_split(cancer, prop = 0.75, strata = diagnosis)
cancer_training <- training(cancer_split)
cancer_testing <- testing(cancer_split)
# initial_split() shuffles the data so the split is random
#   - prop = 0.75           means that we are splitting the data into 75% training and 25% testing
#   - strata = diagnosis    means that roughly same proportion of categories are ending up in each split











# Creating a Classification Model:

#   - very similar to process in Chapter 5, only we use training data

knn_fit <- nearest_neighbor(weight_func = "rectangular", neighbors = 5) |>
  set_engine("kknn") |>
  set_mode("classification")
# creates knn classification model

cancer_recipe <- recipe(diagnosis ~ perimeter_mean + concavity_mean, data  = cancer_training) |>
  step_scale(all_predictors()) |>
  step_center(all_predictors())
# creates recipe for scaling and centering, and sets formula

knn_fit <- workflow() |>
  add_recipe(cancer_recipe) |>
  add_model(knn_fit) |>
  fit(data = cancer_training)
# combine all information to finish classification model


# to make predictions, we simply pass test data into predict()
predictions <- predict(knn_fit, cancer_testing)
predictions
#   - here, predictions is a data frame with a prediction for each observation in cancer_testing

predictions_with_test_data <- predict(knn_fit, cancer_testing) |>
  bind_cols(cancer_testing)
predictions_with_test_data
# bind_cols() function merges two data frames side by side
#   - here, predictions are placed beside actual the cancer_testing data - so we can clearly see how
#     accurate the classification model is











# Computing the Accuracy:

predictions_with_test_data |>
  metrics(truth = diagnosis, estimate = .pred_class) |>
  filter(.metric == "accuracy") |>
  pull(.estimate)

# truth = diagnosis         means that the diagnosis column has the actual classifications
# estimate = .pred_class    means that the .pred_class columns has the predicted classifications

# we add the filter line so we only have the accuracy row
# we can also use pull() to get the accuracy % by itself








# Conf Mat Function:

#   - produces a matrix of actual vs. predicted categories

confusion <- predictions_with_test_data |>
  conf_mat(truth = diagnosis, estimate = .pred_class)
confusion

# this matrix shows that:
#     - out of the 53 malignant observations, 5 were incorrectly predicted to be benign
#     - out of the 89 benign observations, 4 were incorrectly predicted to be malignant
#     - out of the 142 observations, 133 were correctly predicted












# Cross-Validation:

#   - a good way of determining the accuracy of a classification model

#   - this process involves performing another split on the training data
#   - the portion of training data used for evaluating a K value is called the "validation set"

#   - KEY DIFFERENCE: when splitting data into training and testing, we only make two splits.
#     Here, we will split training data multiple ways, and see which split gives us the
#     highest K value


# Ex:

# create the 25/75 split of the training data into training and validation
cancer_split <- initial_split(cancer_training, prop = 0.75, strata = diagnosis)
cancer_subtrain <- training(cancer_split)
cancer_validation <- testing(cancer_split)

# recreate the standardization recipe from before
# (since it must be based on the training data)
cancer_recipe <- recipe(diagnosis ~ perimeter_mean + concavity_mean,
                        data = cancer_subtrain) |>
  step_scale(all_predictors()) |>
  step_center(all_predictors())

# fit the knn model (we can reuse the old knn_spec model from before)
knn_fit <- workflow() |>
  add_recipe(cancer_recipe) |>
  add_model(knn_spec) |>
  fit(data = cancer_subtrain)

# get predictions on the validation data
validation_predicted <- predict(knn_fit, cancer_validation) |>
  bind_cols(cancer_validation)

# compute the accuracy
acc <- validation_predicted |>
  metrics(truth = diagnosis, estimate = .pred_class) |>
  filter(.metric == "accuracy") |>
  select(.estimate) |>
  pull()
acc

#   - if we do this 5 times we will get 5 different values for accuracy
#   - we can then take the average of these 5 values to determine a pretty good value for accuracy

#   - in reality, we use the function vfold_cv() to make this easier, AND so that we use completely
#     validation sets each time (if we always randomize, some observations will overlap)




# Ex:

cancer_vfold <- vfold_cv(cancer_training, v = 5, strata = diagnosis)
# v = 5                 means we are creating 5 different validation sets (this would be 80/20 set ratio)
# strata = diagnosis    means all the splits have the same diagnosis proportions

# this would be called 5-fold cross-validation


# we can then create our model again, but instead of fit() we use fit_resamples()
# fit_resamples() is what actually runs cross validation

cancer_recipe <- recipe(diagnosis ~ perimeter_mean + concavity_mean,
                        data = cancer_training) |>
  step_scale(all_predictors()) |>
  step_center(all_predictors())

# fit the knn model (we can reuse the old knn_spec model from before)
knn_fit <- workflow() |>
  add_recipe(cancer_recipe) |>
  add_model(knn_spec) |>
  fit_resamples(resamples = cancer_vfold)

knn_fit



#   - we can then use the collect_metrics() function to get the MEAN accuracy of all the validation sets
#     (we also get the standard error - a measure of how uncertain we are of the mean value)

#   - usually, the more folds we use, the lower the standard error
#   - however using performing cross_validation with lots of folds takes lots of time
#   - so we usually only use 5-fold or 10-fold cross validation

# Ex:

knn_fit |> collect_metrics()
# here the mean accuracy is 0.887 and the standard error is 0.0109














# Parameter Value Selection:

#   - we can use cross-validation to figure out which K value gives us the highest accuracy, so we can then
#     pick that value for our classification model

#   - to be able to alter the neighbors parameter in the nearest_neighbors() function, we use tune()
#   - this allows us to pass in different values for K

# Ex:
knn_spec <- nearest_neighbor(weight_func = "rectangular", neighbors = tune()) |>
  set_engine("kknn") |>
  set_mode("classification")

# to create our range of K values, we can make a tibble:
k_vals <- tibble(neighbors = seq(from = 1, to = 100, by = 5))
k_vals

# then we use the workflow() function to again connect our information
# here we would have a tune_grid() line instead of fit() or fit_resamples():

knn_results <- workflow() |>
  add_recipe(cancer_recipe) |>
  add_model(knn_spec) |>
  tune_grid(resamples = cancer_vfold, grid = k_vals) |>
  collect_metrics()
knn_results
# resamples = cancer_vfold    means that we compute each model's accuracy using (in this case) 5-fold
#                             cross-validation
# grid = k_vals               means that we pass in all the values in k_vals as parameters for K

knn_results <- knn_results |> filter(.metric == "accuracy")


# we can now decide which K value to use by plotting neighbors vs mean as a line plot

# Ex:
cancer_k_plot <- knn_results |>
  ggplot(aes(x = neighbors, y = mean)) +
  geom_point() +
  geom_line() +
  labs(x = "Neighbors", y = "Accuracy")
cancer_k_plot
# here we can clearly see that K = 71 or K = 76 gives us the highest accuracy










# Under vs Over Fitting:

# under fitting: when the model isn't impacted enough by the data
#   - too big of a K value
#   - ex. K = 300

# over fitting: when the model is impacted too much by the data
#   - too small of a K value
#   - ex. K = 1










# Summary:

# 1) initial_split()

# 2) vfold_cv()

# 3) recipe

# 4) nearest_neighbors() + tune()

# 5) workflow() + tune_grid()

# 6) create model w/ best K

# 7) evaluate accuracy on test set










# Strengths:

#   - is a simple, intuitive algorithm
#   - requires few assumptions about what the data must look like]
#   - works for binary (two-class) and multi-class (more than 2 classes) classification problems.


# Weaknesses:

#   - becomes very slow as the training data gets larger
#   - may not perform well with a large number of predictors
#   - may not perform well when classes are imbalanced



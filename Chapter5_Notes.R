install.packages("tidymodels")

# load packages
library(tidyverse)
library(tidymodels) # needed for knn classification

install.packages("parsnip")
install.packages("kknn")
library(kknn)


# ====================================================
# CHAPTER 5: CLASSIFICATION 1: TRAINING AND PREDICTING
# ====================================================


# set seed
set.seed(1)


cancer <- read_csv("data/wdbc.csv")
cancer

glimpse(cancer) # used to look at data with lots of columns (flips so columns go down the page)






# Looking at Variables:

#   - when starting classification, make sure variable being classified is of type factor

# Ex: change diagnosis column from chr to factor
cancer <- mutate(cancer, diagnosis = as_factor(diagnosis))
glimpse(cancer)


#   - pull() allows us to extract a column
#   - levels() allows us to look at all categories in a variable

# Ex:
cancer |>
  pull(diagnosis) |>
  levels()








# Exploring Data:

#   - we can look at what percentage of data belongs to each category

# Ex:
num_obs <- nrow(cancer)
cancer |>
  group_by(diagnosis) |>
  summarize(count = n(),
            percentage = n() / num_obs * 100)


#   - we can also view the data before we classify to get a good sense of what we can expect

perim_concav <- cancer |>
  ggplot(aes(x = perimeter_mean, y = concavity_mean, color = diagnosis)) +
  geom_point(alpha = 0.6) +
  labs(x = "Perimeter",
       y = "Concavity",
       color = "Diagnosis") +
  scale_color_manual(labels = c("Malignant", "Benign"),
                     values = c("orange2", "steelblue2")) +
  theme(text = element_text(size = 12))
perim_concav








# Scaling Data:

#   - for knn classification, we need to standardize data, so that distances
#     between points are proportional

cancer_recipe <- recipe(diagnosis ~ ., data = cancer) |>
  step_scale(all_predictors()) |>
  step_center(all_predictors())
#   - step_scale() gives data a standard deviation of 1
#   - step_center() gives data a mean of 0

cancer_scaled <- cancer_recipe |>
  prep() |>
  bake(cancer)
#   - prep() finalizes recipe
#   - bake() actually scales and centers the data



standardized_plot <- cancer_scaled |>
  ggplot(aes(x = perimeter_mean, y = concavity_mean, color = diagnosis)) +
  geom_point(alpha = 0.6) +
  labs(x = "Perimeter (standardized)",
       y = "Concavity (standardized)",
       color = "Diagnosis") +
  scale_color_manual(labels = c("Malignant", "Benign"),
                     values = c("orange2", "steelblue2")) +
  theme(text = element_text(size = 12))
standardized_plot










# Classification with K Nearest Neighbors:

#   - to decide nearest points, we use Pythagorean theorem
#   - if we have K = 5, we would calculate distance between new point and all other points, and then
#     keep the 5 nearest points

#   - one way we can do this is with the mutate() function
new_obs_p <- 0
new_obs_c <- 3.5
cancer_scaled |>
  select(diagnosis, perimeter_mean, concavity_mean) |>
  mutate(dist_from_new_point = sqrt((perimeter_mean - new_obs_p)^2 +
                                      (concavity_mean - new_obs_c)^2)) |>
  arrange(dist_from_new_point) |>
  slice(1:5)

#   - this result shows that 3 of the 5 closest neighbors are classified as M - so we would classify
#     our new observation as M (Malignant)

#   - NOTE: when we have > 2 predictors, the straight line distance is calculated as if the plot is
#     3-dimensional, etc.


#   - another way to make classifications is to use built in functions
#   - nearest_neighbor() and recipe() will help us build a classifier

# Ex:

knn_spec <- nearest_neighbor(weight_func = "rectangular", neighbors = 5) |>
  set_engine("kknn") |>
  set_mode("classification")
knn_spec

#   - weight_func = "rectangular" means that each of the K neighbors get 1 vote
#   - neighbors = 5 means that we are using 5 neighbors
#   - set_engine("kknn") just means that we are performing knn classification
#   - set_mode("classification") means that we are building a classification model, not regression

#   - to set our predictors, we must use the fit() function
knn_fit <- knn_spec |>
  fit(diagnosis ~ perimeter_mean + concavity_mean, data = cancer_scaled)
#   - perimeter and concavity are the predictors
#   - diagnosis is being predicted

knn_fit

#   - to predict a new observation, we must use the predict() function
#   - takes two inputs - a classification model and an observation (as a tibble)
#   - outputs a data frame with one variable called ".pred_class"

new_obs <- tibble(perimeter_mean = 0, concavity_mean = 3.5)
new_obs
prediction <- predict(knn_fit, new_obs)
prediction












# Putting Everything Together with Workflow():

unscaled_cancer <- read_csv("data/wdbc.csv") |>
  mutate(diagnosis = as_factor(diagnosis))
# loads unscaled cancer data

knn_spec <- nearest_neighbor(weight_func = "rectangular", neighbors = 5) |>
  set_engine("kknn") |>
  set_mode("classification")
# creates knn model

knn_recipe <- recipe(diagnosis ~ perimeter_mean + concavity_mean, data = unscaled_cancer) |>
  step_scale(all_predictors()) |>
  step_center(all_predictors())
# creates centering and scaling recipe

# NOTE: notice that we can also just specify the formula in the recipe

knn_fit <- workflow() |>
  add_recipe(knn_recipe) |>
  add_model(knn_spec) |>
  fit(data = unscaled_cancer)
# combines all information: (+ data frame)
#     1) classification model
#     2) centering and scaling
#     3) formula

# NOTE: when using workflow(), we do not need prep() and bake() for scaling/centering
# NOTE: here, fit() does not need the formula because we already included it in recipe()

knn_fit

# to try predictor...
new_observations <- tibble(perimeter_mean = c(0.5, 0), concavity_mean = c(3.5, 0.5))
predictions <- predict(knn_fit, new_observations)
predictions




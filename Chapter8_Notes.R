install.packages("tidymodels")

# load packages
library(tidyverse)
library(tidymodels) # needed for knn classification

install.packages("parsnip")
install.packages("kknn")
library(kknn)




# ==========================================
# CHAPTER 8: REGRESSION 2: LINEAR REGRESSION
# ==========================================




# Simple Linear Regression:
# -   one predictor and one prediction
# -   computes the line of best fit by calculating the MINIMUM average squared vertical distance
#     (between the line and each point in the data)
# -   accuracy is determined using RMSPE






# Performing Linear Regression in R:

set.seed(1234)
# make sure to set seed so splits are the same

sacramento_data <- read_csv("data/sacramento.csv") |>
  filter(sq__ft > 0)
sacramento_data


sacramento_split <- initial_split(sacramento_data, prop = 0.6, strata = price)
sacramento_training <- training(sacramento_split)
sacramento_testing <- testing(sacramento_split)

lm_spec <- linear_reg() |>
  set_engine("lm") |>
  set_mode("regression")

lm_recipe <- recipe(price ~ sq__ft, data = sacramento_training)
# Note: with linear regression, we do NOT need to scale and center the data, since the line of best fit with the
# minimum average vertical squared distance will always be the same

lm_fit <- workflow() |>
  add_recipe(lm_recipe) |>
  add_model(lm_spec) |>
  fit(data = sacramento_training)

lm_fit
# Notice that when we call lm_fit, the program prints the slope and the intercept. It looks like this:

#     Coefficients:
#     (Intercept)       sq__ft
#         24438.5         128.2

# This tells us that the y-intercept of our line of best fit is 24438.5 and the slope is 128.2
# So, the equation for our line would be:

#     y = 128.2x + 24438.5

# (Where x = square feet and y = house price)

# Another way of reading this is that the cost of a house starts at $24438.50 for 0 square feet and
# increases by $128.20 per square foot

lm_test_results <- predict(lm_fit, sacramento_testing) |>
  bind_cols(sacramento_testing) |>
  metrics(truth = price, estimate = .pred)

lm_test_results
# this tells us that the final models RMSPE is 96,648










# Geom_Smooth Function:

# -   adds a simple linear regression line of best fit to the plot

lm_plot <- sacramento_training |>
  ggplot(aes(x = sq__ft, y = price)) +
  geom_point(alpha = 0.5) +
  scale_y_continuous(labels = dollar_format()) +
  geom_smooth(method = "lm", se = FALSE) +
  theme(text = element_text(size = 12))
# Note: geom_smooth() also adds some more info to the plot but we call se = FALSE to omit that info

lm_plot







# Extracting Coefficients:

# -   using the pull_workflow_fit() and tidy() function, we can print the slope and y-intercept of our
#     linear regression model

coeffs <- lm_fit |>
  pull_workflow_fit() |>
  tidy()
coeffs





# Advantages and Disadvantages of Linear Regression:

# Advantages:
# -   allows us to see what the prediction is when all the predictors are equal to zero
# -   allows us to see what the prediction would increase by per unit increase of the predictor

# Disadvantages:
# -   is not accurate when relationship between target and predictor variable is non-linear (ex.
#     oscillating or curved) --> in this case, the model will under fit and have a high RMSPE




# Note: Extrapolation - predicting outside the range of observations







# Multivariable Linear Regression:

# -   very similar to simple linear regression, except we use 2+ predictors
# -   instead of a line, this will compute a PLANE of best fit

# Ex:
mlm_recipe <- recipe(price ~ sq__ft + beds, data = sacramento_training)

mlm_fit <- workflow() |>
  add_recipe(mlm_recipe) |>
  add_model(lm_spec) |>
  fit(data = sacramento_training)

mlm_fit
# This prints

#     Coefficients:
#     (Intercept)       sq__ft         beds
#         64369.4        150.1     -23124.1


# Here we have 3 values:
#    64369.4    -> the vertical intercept (prediction when all predictors = 0)
#    150.1      -> the slope of the first predictor
#   -23124.1    -> the slope of the second predictor


# The mathematical equation here would be

#   z = 150.1x - 23124.1y + 64369.4

# (Where z = price, x = square feet, and y = # of bedrooms)



lm_mult_test_results <- mlm_fit |>
  predict(sacramento_testing) |>
  bind_cols(sacramento_testing) |>
  metrics(truth = price, estimate = .pred)

lm_mult_test_results
# RMSPE is calculated in the same way

mcoeffs <- mlm_fit |>
  pull_workflow_fit() |>
  tidy()
mcoeffs








# Other Issues With Linear Regression:

# Outliers:
# -   these are points that are very far away from the line of best fit
# -   big problem because they can have a large influence on the line of best fit

# Multicollinearity:
# -   can occur in multivariable linear regression
# -   happens when two of the predictor variables have a very strong linear relationship, making the
#     plane of best fit very unreliable, as the coefficients become super sensitive to changes in the data








library(tidyverse)

library(readxl)
getwd()


# =========================
# CHAPTER 3: WRANGLING DATA
# =========================



# Tidyverse Package

# includes...
#     dbplyr Package:
#           - select()
#           - filter()
#           - mutate()
#           - group_by()
#           - summarize()
#     tidyr Package:
#           - pivot_longer()
#           - pivot_wider()
#     purrr Package:
#           - map_df()




# What is a Data Frame?

#  - data frame: a tabular structure in R that stores variables, observations, and values

#  - variable: a characteristic, number or quantity that can be measured (usually the column)
#  - observation: all the entities of a row
#  - value: a single measurement of a variable for a given entity

#  - R stores columns in data frames as either VECTORS or LISTS





# Vectors and Lists

# Vectors:
#  - objects that contain one or more elements
#  - all elements must be the SAME type
# Ex:
#      Region     <- vector name
#     "Toronto"
#     "Montreal"    <- elements (all of same type - character)
#     "Vancouver"
#     "Ottawa"

# Lists:
#  - objects that contain one or more elements
#  - elements can be of DIFFERENT types
# Ex:
#      Region     <- list name
#     "Vancouver"
#       2016         <- elements (different types)
#       TRUE

# NOTE: a data frame is a special kind of list in which:
#           - each element is a vector or a list
#           - each element (vector or list) must be the same length







# Tidy Data:

#  - each row is an observation
#  - each variable has its own column
#  - each value is a single cell







# Pivot Longer Function:
#  - used for when column names are values, and we want to make a new column in which
#    the column names are elements
#  - makes the 'wide' data longer

# Ex:
madrid <- read_tsv("data/madrid_pollution.csv")
madrid

madrid2 <- pivot_longer(madrid,
                       cols = BEN:TOL,                       # specify which columns are being pivoted
                       names_to = "pollutant",               # name of column for names from old columns
                       values_to = "pollutant_level")        # name of column for values from old columns
madrid2





# Pivot Wider Function:
#  - used for when we when values as columns
#  - basically, the INVERSE of the pivot_longer() function

# Ex:
madrid3 <- pivot_wider(madrid2,
                       names_from = "pollutant",              # name of column that will take new columns names
                       values_from = "pollutant_level")       # name of column that will take values
madrid3                                                       #      for each new column name







# Mutate Function:
#  - used to modify or create new columns in data frames

dep_bay_temps <- read_csv("data/departure_bay_temperature.csv", skip = 2)
dep_bay_temps

# Ex 1:
dep_bay_temps2 <- mutate(dep_bay_temps, Year = as.integer(Year))
dep_bay_temps2
# Modifies year column by changing all values to int type

# Ex 2:
dep_bay_temps3 <- mutate(dep_bay_temps, Jan_plus_feb = Jan + Feb)
dep_bay_temps3
# Adds a new column for the sum of Jan and Feb for each row

# Ex 3:
dep_bay_temps4 <- mutate(dep_bay_temps, doubleJan = 2 * Jan)
dep_bay_temps4
# Adds a new column in which each Jan value is doubled


# Across Function:
#  - used for when we want to modify multiple columns
# Ex:
dep_bay_temps |> mutate(across(Jan:Dec, as.character))
# MODIFIES every column from Jan to Dec to a vector with the 'chr' type


# Rowwise Function:
#  - used for when we want to apply summary functions across columns but within a row
# Ex:
dep_bay_temps |>
  rowwise() |>
  mutate(Winter_high = max(c(Jan, Feb, Mar)))
# ADDS a new column with the max among specific rows









# Separate Function:
#  - used for when there are multiple values stored in one cell
# tidy_lang |> separate(col = value,
#                       into = c("most_at_home", "most_at_work"),
#                       sep = "/")

# Ex 1:
avocado <- read_csv("data/avocado_prices.csv")
avocado
avocado |> separate(col = Date,
                    into = c("year", "month", "day"),
                    sep = "-")





# Pipe Operator:
#  - takes a data frame on the left side of '|>' and passes it as the first argument
#    in the next function
#  - results in cleaner, easier to read code

# Ex:
# Without pipe operator:
avocado <- read_csv(("data/avocado_prices.csv"))
output_1 <- mutate(avocado, doubled_avg_price = average_price * 2)
output_2 <- filter(output_1, doubled_avg_price > 5)
output <- select(output_2, doubled_avg_price)
output
# With pipe operator:
avocado <- read_csv(("data/avocado_prices.csv"))
output <- avocado |>
  mutate(doubled_avg_price = average_price * 2) |>
  filter(doubled_avg_price > 5) |>
  select(doubled_avg_price)
output






# Group_By and Summary Function:
#  - group_by() takes one or more columns and groups all the similar values in each of those columns
#  - summarize() calculates a summary statistic within a group of values (ex. avg, min, max)
#  - group_by() essentially does nothing without summarize()

marathon <- read_csv("data/marathon_small.csv")

# Ex 1:
marathon |> group_by(sex)
# Creates two groups: male and female - however, data frame will look the same

# Ex 2:
marathon |> summarize(avg_5k_time = mean(km5_time_seconds, na.rm = TRUE))
# Calculates average 5k time among every person in data frame (as if it was one group)

# Ex 3:
marathon |>
  group_by(sex) |>
  summarize(avg_5k_time = mean(km5_time_seconds, na.rm = TRUE))
# With the summarize() function, we can calculate a summary statistic for the two
# groups for any column that we choose. In this case, we calculated the average 5k
# time for males and females

# Ex 4:
marathon |>
  group_by(sex) |>
  summarize(min_5k_time = min(km5_time_seconds, na.rm = TRUE))
# Calculates the min 5k time for males and females

# NOTE: must include 'na.rm = TRUE' in min() function or else output
# will be NA if even just one value in group is NA (same for mean and max)

# Ex 5:
marathon |>
  group_by(sex) |>
  summarize(count = n())
# Calculates the number of people in each group

# Ex 6:
marathon_g <- marathon |>
  group_by(sex, age) |>
  summarize(count = n())
# Here we have two variables in the group_by() parameters: sex and age. So,
# we will first group the rows by sex, and then for each group of sex (male
# and female), we will group the rows by age. Then we we will count the total
# number of rows in each group.
# NOTE: the number of groups will be (# of sexes) * (# of ages)
#     So, if there are 2 groups of sexes and 30 groups of ages, we will have
#     a total of 60 different groups
View(marathon_g)






# Map Function:
#  - used when we want to calculate summary statistics across many columns

# NOTE: we can also do this using summarize() + across()
marathon |> summarize(across(km5_time_seconds:km10_time_seconds, mean, na.rm = TRUE))

# Although, a better way might be to use the map_df() function from the purrr package:
marathon |>
  select(km5_time_seconds:km10_time_seconds) |>
  map_df(mean, na.rm = TRUE)


























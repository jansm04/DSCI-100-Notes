library(tidyverse)

library(readxl)
getwd()

# ==============================
# CHAPTER 1: R AND THE TIDYVERSE
# ==============================


# Reading Data:

avocado <- read_csv("data/avocado_prices.csv")
avocado
# csv files are most common (comma separated values)




# Filter function:
#  - Keeps only rows in which specified case is TRUE
#  - First parameter must be df (data frame)

# Ex 1:
avocado_filtered <- filter(avocado, yr == 2015)
avocado_filtered
# Keeps only rows in which year double is 2015

# Ex 2:
avocado_filtered2 <- filter(avocado, region == "Albany")
avocado_filtered2
# Keeps only rows in which region String is "Albany"

# Ex 3:
avocado_filtered3 <- filter(avocado, yr == 2015 & region == "Albany")
avocado_filtered3
# Keeps only rows in which year double is 2015 AND region String is "Albany"

# Ex 4:
avocado_filtered4 <- filter(avocado, average_price > 1.5)
avocado_filtered4
# Keeps only rows in which average price is greater than 1.5

# Ex 5:
region_names <- c("Albany", "Boston", "Chicago", "Denver")
avocado_filtered5 <- filter(avocado, region %in% region_names)
avocado_filtered5
# Keeps only rows in which region exists in region_names





# Select Function:
#  - Allows us to choose which columns to keep

# Ex 1:
avocado_select <- select(avocado, average_price)
avocado_select
# Selects average_price column

# Ex 2:
avocado_select2 <- select(avocado, average_price, region)
avocado_select2
# Selects average_price and region column

# Ex 3:
avocado_select3 <- select(avocado, -type)
avocado_select3
# Selects every column except for type column

# Ex 4:
avocado_select4 <- select(avocado, small_hass_volume : extra_l_hass_volume)
avocado_select4
# Selects range of columns





# Arrange Function:
#  - Orders column from least to greatest - ascending order
#  - Can also order from greatest to least if column is wrapped in desc() - descending order

# Ex 1:
avocado_arrange <- arrange(avocado, average_price)
avocado_arrange
# Arranges rows in order of lowest average price to highest average price

# Ex 2:
avocado_arrange2 <- arrange(avocado, desc(average_price))
avocado_arrange2
# Arranges rows in order of highest average price to lowest average price




# Slice Function:
#  - Like the select function, but for rows

# Ex 1:
avocado_slice <- slice(avocado_arrange, 1:5)
avocado_slice
# Selects first 5 rows

# Ex 1:
avocado_slice2 <- slice(avocado_arrange, 1)
avocado_slice2
# Selects first row

# Ex 3:
avocado_slice3 <- slice(avocado_arrange, 23)
avocado_slice3
# Selects 23rd row

library(tidyverse) # loads ggplot2 package

library(readxl)
getwd()


# ===========================
# CHAPTER 4: VISUALIZING DATA
# ===========================


# Guiding Principles:

#  - Scatter Plots visualize the relationship between TWO quantitative variables
#  - Line Plots visualize trends with respect to an INDEPENDANT, ordered quantity (ex. time)
#  - Bar Plots visualize comparisons of AMOUNTS
#  - Histograms visualize the DISTRIBUTION of one quantitative variable




# Rules of Thumb:

# Conveying the Message:
#  - make sure visualization answers question as simply as possible
#  - use legends and labels
#  - ensure texts, symbols, etc. are big enough to read
#  - no overlapping
#  - make sure to use contrasting color schemes

# Minimizing Noise:
#  - use colors wisely (too many can be distracting)
#  - avoid over plotting (will make data illegible)
#  - choose size of plot wisely
#  - don't adjust axes to zoom in onto small differences





# Plots Basic Info:

# Ex:
co2_mm |> ggplot(aes(x = year, y = ppm)) +
  geom_point() +
  labs(x = "Year", y = "PPM", title = "Year vs PPM") +
  theme(text = element_text(size = 15))

#  - aes() is for aesthetic mappings - tells R which columns will be used as properties
#  - geom_point() is the geometric object - tells R what kind of plot we want
#  - labs() is used for labels
#  - theme() is used for text size, and other things






# Options (Plot Size):
# options(repr.plot.width = 15, repr.plot.height = 12)







# Scatter Plot:

# Characteristics:
#  - Strength: if plots are close together and exhibit clear trend, plot is 'strong'
#              if plots are widely scattered, then plot is 'weak'
#  - Direction: if y variable increases as x increases, plot shows a positive relationship
#               if y variable decreases as x increases, plot shows a negative relationship
#               if neither occur, plot shows no relationship
#  - Shape: if you can draw a rough straight line through the points, then plot is 'linear'
#           if you can not draw a rough straight line through the points, then plot is 'nonlinear'


insurance <- read_csv("data/insurance.csv")
View(insurance)

# Ex 1:
insurance |>
  ggplot(aes(x = bmi, y = charges)) +
  geom_point() +
  labs(x = "BMI", y = "Medical Charges")
# shows a scatter plot of BMI vs medical charges

# Ex 2:
insurance |>
  ggplot(aes(x = bmi, y = charges, colour = smoker)) +
  geom_point() +
  labs(x = "BMI", y = "Medical Charges", colour = "Smoker?")
# shows a scatter plot of BMI vs medical charges, with plots colored depending on
# whether or not the person is a smoker

# Ex 3:
insurance |>
  ggplot(aes(x = bmi, y = charges, colour = smoker, shape = smoker)) +
  geom_point() +
  labs(x = "BMI", y = "Medical Charges", colour = "Smoker?", shape = "Smoker?")
# shows a scatter plot of BMI vs medical charges, with plots colored AND shaped depending on
# whether or not the person is a smoker

# Ex 4:
insurance |>
  ggplot(aes(x = bmi, y = charges)) +
  geom_point(aes(colour = smoker, shape = smoker)) +
  labs(x = "BMI", y = "Medical Charges", colour = "Smoker?", shape = "Smoker?")
# same as Ex 3, but color and shape are now passed into aes() function in geom_point()

# Ex 5:
library(scales)
insurance |>
  ggplot(aes(x = bmi, y = charges)) +
  geom_point() +
  labs(x = "BMI", y = "Medical Charges") +
  scale_x_log10(labels = label_comma()) +
  scale_y_log10(labels = label_comma())
# shows a scatter plot with log scales
#  - more useful when most data is clumped and there are some values that are very far away

# Ex 6:
insurance |>
  ggplot(aes(x = bmi, y = charges, colour = smoker)) +
  geom_point(alpha = 0.5) +
  labs(x = "BMI", y = "Medical Charges", colour = "Smoker?")
# same as Ex 2, but with alpha() - adjusts transparency





# Line Plot:

world_vac <- read_csv("data/world_vaccination.csv")

# Ex 1:
world_vac |>
  filter(vaccine == "polio" & who_region == "Africa") |>
  ggplot(aes(x = yr, y = pct_vaccinated)) +
  geom_line() +
  labs(x = "Year", y = "Percent Vaccinated", title = "Percent of People Vaccinated for Polio in Africa")
# shows how the percent of people vaccinated for polio has trended in Africa over the last 40 years

# Ex 2:
world_vac |>
  filter(vaccine == "polio") |>
  ggplot(aes(x = yr, y = pct_vaccinated, color = who_region)) +
  geom_line() +
  labs(x = "Year",
       y = "Percent Vaccinated",
       title = "Percent of People Vaccinated for Polio",
       color = "Region")
# shows how the percent of people vaccinated for polio has trended in different regions over the last 40 years

# Ex 3:
world_vac |>
  filter(vaccine == "polio") |>
  ggplot(aes(x = yr, y = pct_vaccinated)) +
  geom_line(aes(color = who_region)) +
  labs(x = "Year",
       y = "Percent Vaccinated",
       title = "Percent of People Vaccinated for Polio",
       color = "Region")
# same as Ex 2, but color aes is in geom_line() parameter






# Bar Plot:

# Ex 1:
world_vac |>
  filter(yr == 2017 & vaccine == "polio") |>
  ggplot(aes(x = who_region, y = pct_vaccinated)) +
  geom_bar(stat = "identity") +
  labs(x = "Region", y = "Percent Vaccinated")
# Compares percent vaccinated in 2017 for polio across different regions

# Ex 2:
world_vac |>
  filter(yr == 2017) |>
  ggplot(aes(x = who_region, y = pct_vaccinated, fill = vaccine)) +
  geom_bar(stat = "identity") +
  labs(x = "Region", y = "Percent Vaccinated", fill = "Vaccine")
# Compares percent vaccinated in 2017 across different regions, with
# different colours for different vaccines

# Ex 3:
world_vac |>
  filter(yr == 2017) |>
  ggplot(aes(x = who_region, y = pct_vaccinated, fill = vaccine)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Region", y = "Percent Vaccinated", fill = "Vaccine")
# Same as Ex 3, but bars are shown as ratios between different vaccines

# Ex 4:
world_vac |>
  filter(yr == 2017) |>
  ggplot(aes(x = who_region, y = pct_vaccinated, fill = vaccine)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Region", y = "Percent Vaccinated", fill = "Vaccine")
# Same as Ex 3, but bars are differentiated by vaccine and shown side by side

# Ex 5:
world_vac |>
  group_by(vaccine) |>
  summarize(count = n()) |>
  ggplot(aes(x = vaccine, y = count)) +
  geom_bar(stat = "identity") +
  labs(x = "Vaccine", y = "Count")
# One way of counting # of observations for different vaccines

# Ex 6:
world_vac |>
  ggplot(aes(x = vaccine)) +
  geom_bar() +
  labs(x = "Vaccine", y = "Count")
# Another (easier) way of counting # of observations for different vaccines

# Ex 7:
world_vac |>
  ggplot(aes(x = vaccine, fill = who_region)) +
  geom_bar() +
  labs(x = "Vaccine", y = "Count", fill = "Region")
# Counts # of observations for different vaccines, and shows how many are from what region

# Ex 8:
world_vac |>
  filter(yr == 2017 & vaccine == "polio") |>
  ggplot(aes(x = who_region, y = pct_vaccinated)) +
  geom_bar(stat = "identity") +
  labs(x = "Region", y = "Percent Vaccinated") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Same as Ex 1, but x-axis labels are rotated

# Ex 9:
world_vac |>
  filter(yr == 2017 & vaccine == "polio") |>
  ggplot(aes(x = who_region, y = pct_vaccinated)) +
  geom_bar(stat = "identity") +
  labs(x = "Region", y = "Percent Vaccinated") +
  coord_flip()
# Same as Ex 1, but axes are flipped






# Histogram:

# Ex 1:
diamonds |>
  filter(carat >= 1) |>
  ggplot(aes(x = price)) +
  geom_histogram() +
  labs(x = "Price")
# Price distribution for diamonds with a carat > 1

# Ex 3:
diamonds |>
  filter(carat >= 1) |>
  ggplot(aes(x = price)) +
  geom_histogram(binwidth = 500) +
  labs(x = "Price")
# Price distribution for diamonds with a carat > 1, but with binwidth()

# Ex 3:
diamonds |>
  filter(carat >= 1) |>
  ggplot(aes(x = price)) +
  geom_histogram(binwidth = 500) +
  geom_vline(xintercept = 5000, linetype = "dashed", size = 1) +
  labs(x = "Price")
# Price distribution for diamonds with a carat > 1, but with binwidth() and vline()

# Ex 4:
insurance |>
  ggplot(aes(x = bmi, fill = as_factor(children))) +
  geom_histogram(binwidth = 5) +
  labs(x = "BMI", fill = "# of Children")
# as_factor() function
# no position = "identity" -> groups of bars are stacked

# Ex 5:
insurance |>
  ggplot(aes(x = bmi, fill = as_factor(children))) +
  geom_histogram(binwidth = 5, position = "identity") +
  labs(x = "BMI", fill = "# of Children")
# as_factor() function
# with position = "identity" -> groups of bars overlap





# Facet Grid Function

# Ex 1:
insurance |>
  ggplot(aes(x = bmi, fill = as_factor(children))) +
  geom_histogram(binwidth = 5) +
  facet_grid(rows = vars(children)) +
  labs(x = "BMI", fill = "# of Children")
# Multiple plots differing in # of children, listed in rows

# Ex 2:
insurance |>
  ggplot(aes(x = bmi, fill = as_factor(children))) +
  geom_histogram(binwidth = 5) +
  facet_grid(cols = vars(children)) +
  labs(x = "BMI", fill = "# of Children")
# Multiple plots differing in # of children, listed in columns










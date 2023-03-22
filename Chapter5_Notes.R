install.packages("tidymodels")

# load packages
library(tidyverse)
library(tidymodels) # needed for knn classification

# set seed
set.seed(1)


marketing <- read_tsv("data/marketing_campaign.csv")
marketing
View(marketing)

marketing_data_tidy <- marketing |>
  select(Year_Birth, Education, Income, Education)

marketing_basic_ed <- marketing_data_tidy |>
  filter(Education == "Basic")
marketing_basic_ed

marketing_basic_ed_count <- summarize(marketing_basic_ed, n())
count <- pull(marketing_basic_ed_count, 1)
count

marketing_grad_ed <- marketing_data_tidy |>
  filter(Education == "Graduation") |>
  head(count)
marketing_grad_ed

marketing_data <- rbind(marketing_basic_ed,
                            marketing_grad_ed)
View(marketing_data)



marketing_plot <- marketing_data |> ggplot(aes(x = Year_Birth, y = Income, color = Education)) +
  geom_point() +
  labs(x = "Year of Birth", y = "Income")
marketing_plot







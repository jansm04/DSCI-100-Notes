install.packages("tidymodels")

# load packages
library(tidyverse)
library(tidymodels) # needed for knn classification

install.packages("parsnip")
install.packages("kknn")
library(kknn)






# =====================
# CHAPTER 9: CLUSTERING
# =====================



# Clustering: making classifications WITHOUT a response variable







# Advantages and Disadvantages of Clustering:

# Advantages:
# -   requires no additional input on the data (ex. we do not need to annotate observations with human-made labels)
# -   allows us to split observations into distinct types

# Disadvantages:
# -   no response variable, so it is difficult to evaluate the quality of our model







set.seed(1)
# this is important since we will be randomizing the initial points


beers <- read_csv("data/beers.csv")






# K-Means Clustering:

# -   this algorithm works by creating an initial randomized set of clusters, computing the
#     center of each cluster, and then re-clustering the points based on their distance to the
#     center (repeated as many times as needed)

# -   we can express this algorithm using two major steps:
#       1) Center Update - calculate the center of every cluster
#       2) Label Update - reassign each data point to the cluster with the closest center

# -   we can measure the quality of a cluster using WSSD (Within-cluster Sum of Square Distances)
# -   calculated the same way we would calculate the dist. between two points (Euclidean distance formula)

# -   since this is randomized, we will get different values for WSSD with each different initial center
# -   so, we need to perform the process multiple times, and then take the computation with the lowest
#     WSSD







# Choosing K:

# -   if K is too small, then multiple clusters can be put into one big cluster --> gives us a large WSSD
# -   if K is too large, then clusters get divided into sub-clusters --> gives us a lower WSSD, but does
#     not decrease by a sufficient amount

# -   the way we can choose K is by plotting the # of clusters vs the WSSD, and then choosing the K at the
#     'elbow' point












# Clustering Process With Specific K:

# clean data
clean_beer <- beers |>
  filter(!is.na(ibu)) |>
  select(ibu, abv)
clean_beer

# scale data, so that the straight-line distance can be even
scaled_beer <- clean_beer |>
  mutate(across(everything(), scale))
scaled_beer

# visualization:
beers_plot <- ggplot(scaled_beer, aes(x = ibu, y = abv)) +
  geom_point() +
  xlab("IBU") +
  ylab("ABV") +
  theme(text = element_text(size = 12))
beers_plot



# start with k means function, specifying number of clusters
beers_clust <- kmeans(scaled_beer, centers = 2)
beers_clust
# this also gives a lot of information about the clusters, such as WSSD, and cluster means

# to tidy this information, we can use augment(), which creates a table of our clustered data
beers_clustered_data <- augment(beers_clust, scaled_beer)
beers_clustered_data


# we can then plot this data frame to see our clusters
cluster_plot <- beers_clustered_data |>
  ggplot(aes(x = ibu, y = abv, color = .cluster), size = 2) +
  geom_point() +
  labs(x = "Flipper Length (standardized)",
       y = "Bill Length (standardized)",
       color = "Cluster")
cluster_plot














# Clustering Process With K Selection:

# start by making a table of Ks
beer_clust_ks <- tibble(k = 1:9)
beer_clust_ks


# then use rowwise() and mutate() to use each K value as an input for the kmeans() function
# -   NOTE: we can also use nstart to specify how many different times we want to perform the kmeans
#     algorithm before we choose the best possible clustering (lowest WSSD)

beer_clust_ks <- beer_clust_ks |>
  rowwise() |>
  mutate(beer_clusts = list(kmeans(scaled_beer, nstart = 10, k)))
beer_clust_ks



# we can view the info from a kmeans() function by pulling the column of 'infos' and using pluck()
beer_clust_ks |>
  pull(beer_clusts) |>
  pluck(1)
# this tells us the WSSD for when we have a K value of 1



# we can again use mutate to apply the glance() function to each object
beer_clust_ks <- beer_clust_ks |>
  mutate(glanced = list(glance(beer_clusts)))
beer_clust_ks




# finally we can extract each WSSD by using the unnest() function, which turns each data frame
# into a much simpler data types
clustering_stats <- beer_clust_ks |>
  unnest(glanced)
clustering_stats
# the tot.withinss column is our WSSD



# we can then plot this data as such
elbow_plot <- clustering_stats |>
  ggplot(aes(x = k, y = tot.withinss)) +
  geom_point() +
  geom_line() +
  labs(x = "Number of Clusters", y = "WSSD")
elbow_plot
# this tells us that 2 clusters is the right number of clusters, since there is an 'elbow' at that
# point in the line graph









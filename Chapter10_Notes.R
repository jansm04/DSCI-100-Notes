install.packages("tidymodels")

# load packages
library(tidyverse)
library(tidymodels) # needed for knn classification

install.packages("parsnip")
install.packages("kknn")
library(kknn)




# =================================
# CHAPTER 10: STATISTICAL INFERENCE
# =================================




# Key Terms:


# Population - the entire set of observations in the data
#     (ex. all college students in Canada)

# Population Parameter - a numerical characteristic of a whole population
#     (ex. the proportion of college students in Canada with an iPhone)

# Sample - a subset of observations from the population
#     (ex. 10 college students in Canada)

# Point Estimate - a numerical characteristic of a sample that can be used to estimate the
#     population parameter
#     (ex. the proportion of the 10 college students that have an iPhone)

# Statistical Inference - the process of using a sample to make a conclusion about a population
#     (ex. if 3 of those 10 students have an iPhone, then we can estimate that 30% of all college
#      students in Canada have an iPhone)









set.seed(4321) # DO NOT CHANGE
can_seniors <- tibble(age = (rexp(2000000, rate = 0.1)^2) + 65) |>
  filter(age <= 117, age >= 65)
can_seniors


pop_dist <- ggplot(can_seniors, aes(x = age)) +
  geom_histogram(binwidth = 1) +
  xlab("Age") +
  ggtitle("Population distribution")
pop_dist


pop_parameters <- can_seniors |>
  summarize(pop_mean = mean(age), pop_med = median(age), pop_sd = sd(age))
pop_parameters







# Taking a Sample:

# takes a sample size of 40
sample_1 <- can_seniors |>
  rep_sample_n(size = 40)
sample_1

# plots the distribution from the sample
sample_1_dist <- ggplot(sample_1, aes(x = age)) +
  geom_histogram(binwidth = 1) +
  xlab("Age") +
  ggtitle("Population distribution")
sample_1_dist

# computes point estimates from the sample (ex. mean, median, standard deviation)
sample_1_estimates <- sample_1 |>
  summarize(sample_1_mean = mean(age), sample_1_med = median(age), sample_1_sd = sd(age))
sample_1_estimates





# if we take another sample, the data will be different
sample_2 <- can_seniors |>
  rep_sample_n(size = 40)
sample_2

# (slightly) different distribution
sample_2_dist <- ggplot(sample_2, aes(x = age)) +
  geom_histogram(binwidth = 1) +
  xlab("Age") +
  ggtitle("Population distribution")
sample_2_dist

# (slightly) different point estimates
sample_2_estimates <- sample_2 |>
  summarize(sample_2_mean = mean(age), sample_2_med = median(age), sample_2_sd = sd(age))
sample_2_estimates



# Rep_sample_n() Function:

# -   since the values are different, we can't tell which one is a better estimate for the entire population
# -   to prevent inaccuracies, we can take multiple samples and then take the average estimate of all the
#     samples together
# -   to do this, we can use reps = n (where n is the number of different samples we want to take)

samples <- can_seniors |>
  rep_sample_n(size = 10, reps = 100)
# takes 100 samples of size 10

samples
# the column replicate shows the sample # that each observation belongs to

# now we can group each sample by the replicate and see 2000 different point estimates
sample_estimates <- samples |>
  group_by(replicate) |>
  summarize(samples_mean = mean(age), samples_med = median(age), samples_sd = sd(age))
sample_estimates

# we can visualize the distributions of all the means as well
sampling_distributions <- sample_estimates |>
  ggplot(aes(x = samples_mean)) +
  geom_histogram(bins = 12) +
  labs(x = "Sample Means", y = "Count")
sampling_distributions

# we can also calculate the mean of the means
sample_estimates |>
  summarize(mean = mean(samples_mean))
# this tells us that the mean age is 78.3





# NOTE: one way we can improve a point estimate by increasing the size of the samples
# larger sample size = more accurate point estimate

# Ex:
samples_n100 <- can_seniors |>
  rep_sample_n(size = 100, reps = 100)
samples_n100
# takes 100 samples of size 100

sample_estimates_n100 <- samples_n100 |>
  group_by(replicate) |>
  summarize(samples_mean = mean(age), samples_med = median(age), samples_sd = sd(age))
sample_estimates_n100

# note the distribution will be more compact, since each sample will be closer to the mean
sampling_distributions_n100 <- sample_estimates_n100 |>
  ggplot(aes(x = samples_mean)) +
  geom_histogram(bins = 12) +
  labs(x = "Sample Means", y = "Count")
sampling_distributions_n100

sample_estimates_n100 |>
  summarize(mean = mean(samples_mean))
# this tells us that the mean age is 79.3, which is MUCH closer to the actual population mean



















# The Bootstrap

# Definition: pretending that a large enough sample IS the population, and then taking more samples
# (with replacement) of the same size from it
# -   this gives an APPROXIMATION of the sampling distribution


# Bootstrap Process: (For a sample size n)
#   1) Randomly select an observation from the sample that was drawn from the population
#   2) Record the value of the observation
#   3) Replace that observation
#   4) Repeat 1-3 n times until you have another sample with n observation (bootstrap sample)
#   5) Calculate the bootstrap point estimate
#   6) Repeat 1-5 until you have lots of different bootstrap point estimates for a distribution
#   7) Calculate the plausible range of values around the observed point estimate

# we only work with one sample
one_sample <- can_seniors |>
  rep_sample_n(size = 10)
one_sample

summarize(one_sample, mean = mean(age))
# mean age is 76.1

# view distribution
one_sample_dist <- ggplot(one_sample, aes(age)) +
  geom_histogram(fill = "dodgerblue3", color = "lightgrey") +
  labs(x = "Age", y = "Count") +
  theme(text = element_text(size = 12))
one_sample_dist

# retake a sample with replacements
boot1 <- one_sample |>
  rep_sample_n(size = 10, replace = TRUE, reps = 1)
# NOTE: set replace = TRUE to indicate that we want replacements in the bootstrap sample
boot1

boot1_dist <- ggplot(boot1, aes(age)) +
  geom_histogram(fill = "dodgerblue3", color = "lightgrey") +
  labs(x = "Age", y =  "Count") +
  theme(text = element_text(size = 12))

boot1_dist

summarize(boot1, mean = mean(age))
# mean age is 70.9

# note that the mean ages differ




# if we take 2000 bootstrap samples of the original sample:
boot1000 <- one_sample |>
  rep_sample_n(size = 10, replace = TRUE, reps = 1000)
# NOTE: set replace = TRUE to indicate that we want replacements in the bootstrap sample
boot1000


# taking a look at the first 6 bootstrap samples:
six_bootstrap_samples <- boot1000 |>
  filter(replicate <= 6)

ggplot(six_bootstrap_samples, aes(age)) +
  geom_histogram(fill = "dodgerblue3", color = "lightgrey") +
  labs(x = "Price per night (Canadian dollars)", y = "Count") +
  facet_wrap(~replicate) +
  theme(text = element_text(size = 12))
# we can see that the distributions differ

# we can also take the mean age of each sample
six_bootstrap_samples |>
  group_by(replicate) |>
  summarize(mean = mean(age))
# and we can see that the means differ as well


# taking the mean of each of the 1000 samples:
boot1000_means <- boot1000 |>
  group_by(replicate) |>
  summarize(mean = mean(age))

boot1000_means

# we can view the distribution of all the means
boot_est_dist <- ggplot(boot1000_means, aes(x = mean)) +
  geom_histogram(fill = "dodgerblue3", color = "lightgrey") +
  labs(x = "Sample mean price per night \n (Canadian dollars)", y = "Count") +
  theme(text = element_text(size = 12))
boot_est_dist





# Sampling Distribution vs Bootstrap Distribution:

# -   shape of distributions are very similar which gives us an idea of the point estimate variability
# -   the means of the two distributions are different
#         - sampling distribution is centered at population's mean age
#         - bootstrap distribution is centered at the one sample's mean age
# OVERALL: bootstrap approximate sampling distribution, but not the mean










# Confidence Interval:

# Definition: a range of plausible values for the population parameter
# -   this will be the range of values in the middle 95% of the bootstrap distribution

# To calculate:
#   1) Arrange the values in ascending order
#   2) Find the value such that 2.5% of the values fall below it (lower bound)
#   3) Find the value such that 97.5% of the values fall above it (upper bound)

bounds <- boot1000_means |>
  select(mean) |>
  pull() |>
  quantile(c(0.025, 0.975))

bounds



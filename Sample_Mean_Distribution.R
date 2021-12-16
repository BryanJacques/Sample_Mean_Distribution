# This script is designed to show that by sampling a dataset multiple times and 
# plotting the sample mean, the sample mean distribution will approach normal as
# the number of repitions is increased. It uses mean disc golf hole distance to
# do so.


# load packages and set seed
# ------------------------------------------------------------------------------
library(tidyverse)
library(here)
library(readxl)
library(gridExtra)

set.seed(1)


# import files and select fields
# ------------------------------------------------------------------------------
hole_scores <- read_csv(here("Hole_Scores.csv"))
round_course_map <- read_excel(here("Round_Course_Map.xlsx"))
course_hole_map <- read_excel(here("Course_Hole_Map.xlsx"))

hole_scores <- hole_scores %>% 
  left_join(round_course_map, by = c("tournament_short","round")) %>% 
  left_join(course_hole_map, by = c("course","hole")) %>% 
  mutate(par_score = score_val - par)

hole_scores <- hole_scores %>% 
  select(course, hole, par, distance) %>% 
  distinct()


# get overall mean to plot as vertical line in later plots
# ------------------------------------------------------------------------------
overall_mean_distance <- mean(hole_scores$distance)


# create plot_dist function to take sample size and repetitions args, returns
#   ggplot object of distribution of means
# ------------------------------------------------------------------------------
plot_dist <- function(sample_size, repetitions) {
  
  sample_means <- vector()
  
  for (x in 1:repetitions) {
    sample_hole_scores <- hole_scores %>% 
      slice_sample(n = sample_size)
    
    sample_means <- append(sample_means, mean(sample_hole_scores$distance))
  }
  
  ggplot() +
    geom_histogram(mapping = aes(x = sample_means)) +
    geom_vline(
      xintercept = overall_mean_distance
      ,color = "red"
    ) +
    labs(
      title = str_c("Sample Size ", as.character(sample_size), ", ", as.character(repetitions), " ", "repetitions")
    )
}


# run plot_dist function w/ increasing number of repetitions to show how
#   increasing repetitions causes sample mean to approach normal distribution
# ------------------------------------------------------------------------------
plot_1 <- plot_dist(20,50)
plot_2 <- plot_dist(20,200)
plot_3 <- plot_dist(20,10000)

grid.arrange(plot_1, plot_2, plot_3, ncol=1)

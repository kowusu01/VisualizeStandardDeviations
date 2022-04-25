##################################################################
# code snippet to visualize standard deviations in user ratings
# using movielens dataset and ggplot 
##################################################################

# load libraries
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(RColorBrewer)) install.packages("RColorBrewer")

library(ggplot2)
library(RColorBrewer)

# set set to ensure reproducibility
set.seed(1) 


# url to my github location
remote_base_url <- "https://raw.githubusercontent.com/kowusu01"
remote_path <- "/TransformedDatasets/main/movielens/rda/"


local_folder <- "rda"
file_name <- "movielens_ratings.rda" 
if(!dir.exists(local_folder))
  dir.create(local_folder)

# read dataset from github
# construct the path for the local folder where the downloaded will be saved
local_path <- file.path(local_folder, file_name)

# construct the github full path for the dataset
remote_full_path <-  paste0(remote_base_url, remote_path, file_name)

# download the file, only if it does not exist locally
if(!file.exists(local_path))
  download.file(remote_full_path, local_path,  method="curl", mode = "w")



# load the data into memory
movielens_ratings <- readRDS(local_path)

# we start by calculating standard deviations for each user's ratings
user_rating_sds <- movielens_ratings %>% 
  group_by(userId) %>% 
  summarise(n=n(), sd=sd(rating)) %>%
  arrange(desc(sd))

# see users and the standard deviation in their ratings
user_rating_sds %>% select(userId, sd) %>% arrange(sd)

  
# now lets select a sample of the standard deviations to plot
sample_size <- 50000
sample_index <- sample(nrow(user_rating_sds), sample_size)
user_rating_sds <- user_rating_sds[sample_index, ]

# set chart title and label
chart_title <- "Levels of deviation (standard deviations for user ratings)"
y_axis_label <- "standard deviation \n (in user ratings)"

dev.new()
# base chart
user_rating_sds %>% 
  ggplot(aes(x=factor(userId), y=sd)) + 
  geom_point() + 
  labs(title = chart_title) + ylab(y_axis_label) + xlab("users") +
   theme(axis.text.x=element_blank())


# group the standard deviations into categories and use the 
# categories as color for the scatterplot
user_rating_sds <- user_rating_sds %>% 
  mutate(deviation=ifelse(sd==0, "no_variation", "high_variation")) %>%
  mutate(deviation=ifelse(sd > 0 & sd <=0.5, "low_variation", deviation)) %>% 
  mutate(deviation=ifelse(sd > 0.5 & sd <=1.5, "medium_variation", deviation)) %>% 
  mutate(deviation = factor(deviation, 
                                       levels = c( "high_variation", 
                                                   "medium_variation",
                                                   "low_variation",
                                                   "no_variation" )))

dev.new()
user_rating_sds %>% 
  ggplot(aes(x=factor(userId), y=sd)) + 
  geom_point(aes(color=deviation)) + 
  scale_color_manual(values = c("#e41a1c", "#fed98e", "#31a354", "#de2d26")) +
  labs(title = chart_title) +
  ylab(y_axis_label) + xlab("users") +
  theme(axis.text.x=element_blank())


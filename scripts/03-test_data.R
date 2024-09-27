#### Preamble ####
# Purpose: Tests... 
# Author: Ruiying LI 
# Date: 24 September 2024 
# Contact: ruiying.li@mail.utoronto.ca 
# License: MIT
# Pre-requisites:None
# Any other information needed? 


#### Workspace setup ####
library(tidyverse)



#### Test data ####
data <- read.csv("data/raw_data/simulated.csv")


# Test for negative numbers
data$number_of_death |> min() <= 0

# Test for NAs
all(is.na(data$number_of_death))

#Test abnormalities
ggplot(data, aes(y = number_of_death)) +
  geom_boxplot() +
  labs(title = "boxplot of Death number")


#Test for level of variable
table(data$location)
table(data$civic_centre)


#### Preamble ####
# Purpose: Cleans the raw plane data recorded by two observers.
# Author: Ruiying Li
# Date: 24 September 2024 [...UPDATE THIS...]
# Contact: ruiying.li@mail.utoronto.ca
# License: MIT
# Pre-requisites:None
# Any other information needed?

#### Workspace setup ####

library(tidyverse)
library(dplyr)
library(lubridate)

raw_data <- read.csv("data/raw_data/raw_data.csv")

#transfer str into factor
location<- as.factor(raw_data$PLACE_OF_DEATH)
civic_centre<- as.factor(raw_data$CIVIC_CENTRE)


#change Capital letter to lower version,and delete useless list
cleaned_data <- raw_data %>%rename_with(tolower)%>%select(-x_id)%>%
  mutate(time_period = ym(time_period))

#cleaning data in season order
cleaned_data <- cleaned_data %>%
  mutate(year = year(time_period), 
         season = case_when(
           month(time_period) %in% c(12, 1, 2) ~ "Winter",  
           month(time_period) %in% c(3, 4, 5) ~ "Spring", 
           month(time_period) %in% c(6, 7, 8) ~ "Summer",  
           month(time_period) %in% c(9, 10, 11) ~ "Fall"   
         ))%>%filter(year >= 2020 & year <= 2024)%>%
  rename(number_of_death = death_licenses,location = place_of_death)%>%select(-x)
#remove useless list
cleaned_data <- cleaned_data%>%select(-time_period)

#### Save data ####
write.csv(cleaned_data, "data/analysis_data/analysis_data.csv")


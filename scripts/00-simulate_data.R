#### Preamble ####
# Purpose: Simulates data
# Author: Ruiying li 
# Date: 24 September 2024
# Contact: ruiying.li@mail.utoronto.ca
# License: MIT
# Pre-requisites: None
# Any other information needed? 

#### Simulate data ####
library(tidyverse)
install.packages('tibble')
library(tibble)


#### Simulate data ####
set.seed(123)

# Define the start year and end date
start_date <- as.Date("2020-01-01")
end_date <- as.Date("2024-08-01")

#number of dates to simulate 
number_of_dates <- 100

#death locations to simulate
locations<- c("Toronto","Outside city limits")

#death registry center to simulate
civic_centres<-c('ET','NY','SC','TO')

#Simulated data with date
data<-tibble(dates = as.Date(runif(n= number_of_dates,
                                   min = as.numeric(start_date), 
                                   max = as.numeric(end_date)
),
origin = "1970-01-01"
),
number_of_death = rpois(n = number_of_dates,lambda = 50),
location = sample(locations,size = number_of_dates,replace = TRUE),
civic_centre = sample(civic_centres,size = number_of_dates,replace = TRUE)
)

#write_csv
write.csv(data,file = "data/raw_data/simulated.csv")


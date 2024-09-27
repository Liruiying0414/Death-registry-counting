#### Preamble ####
# Purpose: Downloads and saves the data from
# Author: Ruiying li 
# Date: 24 September 2024
# Contact: ruiying.li@mail.utoront.ca 
# License: MIT
# Pre-requisites:None
# Any other information needed?


#### Workspace setup ####
library(opendatatoronto)
library(tidyverse)


#### Download data ####
# get package
package <- show_package("cba07a90-984b-42d2-9131-701c8c2a9788")

# get all resources for this package
resources <- list_package_resources("cba07a90-984b-42d2-9131-701c8c2a9788")

# identify datastore resources; by default, Toronto Open Data sets datastore resource format to CSV for non-geospatial and GeoJSON for geospatial resources
datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))


# load the first datastore resource as a sample
data <- filter(datastore_resources, row_number()==1) %>% get_resource()



#### Save data ####
write.csv(data, "data/raw_data/raw_data.csv") 


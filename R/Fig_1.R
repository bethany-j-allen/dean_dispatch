# Bethany Allen
# 19.03.2025
# Code to quantify the distribution of dinosaur fossils from different
# continents

# Load packages
library(tidyverse)
library(palaeoverse)

# Read in data
fossils <- read_csv("data/dinosauria.csv", skip = 19)

# Remove absolute duplicates
fossils <- distinct(fossils, collection_no, accepted_name, .keep_all = TRUE)

# Collapse stacked occurrences
fossils <- distinct(fossils, lat, lng, max_ma, min_ma, accepted_name,
                 .keep_all = TRUE)

# Create Phanerozoic time bins
bins <- time_bins()

# Put occurrences into time bins using "majority" method
fossils <- bin_time(occdf = fossils, bins = bins, method = "majority")

# Remove occurrences for which bin is not Late Cretaceous (73 - 78)
fossils <- filter(fossils, bin_assignment == 73 | bin_assignment == 74 |
                           bin_assignment == 75 | bin_assignment == 76 |
                           bin_assignment == 77 | bin_assignment == 78)



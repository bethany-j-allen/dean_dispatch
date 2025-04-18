# Bethany Allen
# 19.03.2025
# Code to quantify the distribution of dinosaur fossils from different
# continents

# Load packages
library(tidyverse)
library(palaeoverse)
library(countrycode)
library(rphylopic)
library(gridExtra)
library(ggpubr)
library(ggthemes)
library(stringr)

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

# Take ISO 2 letter country codes and convert to continents
fossils$continent <- countrycode(fossils$cc, "iso2c", "continent")

# Label country 'AA' as being Antarctica
fossils$continent[which(fossils$cc == "AA")] <- "Antarctica"

# Label country 'UK' as being in Europe
fossils$continent[which(fossils$cc == "UK")] <- "Europe"

# Relabel "Americas" as "South America"
fossils$continent[which(fossils$continent == "Americas")] <-
  "South America"

# Relabel "CA", "US" and "MX" as "North America"
fossils$continent[which(fossils$cc == "CA")] <- "North America"
fossils$continent[which(fossils$cc == "US")] <- "North America"
fossils$continent[which(fossils$cc == "MX")] <- "North America"

## Count loss of North American species
# Subset to North American fossils
NorthAmerican <- filter(fossils, continent == "North America")

# Count Campanian species
Campanian <- filter(NorthAmerican, bin_assignment == 77) %>%
  filter(accepted_rank == "species")
length(unique(Campanian$accepted_name))

# Count Maastrichtian species
Maastrichtian <- filter(NorthAmerican, bin_assignment == 78) %>%
  filter(accepted_rank == "species")
length(unique(Maastrichtian$accepted_name))

## All dinosaurs
# Count and calculate percentages
counts <- count(fossils, continent) %>% mutate(pct = round((n / sum(n) * 100), 1))
counts <- arrange(counts, desc(n))
counts$continent <- factor(counts$continent, levels = counts$continent)

# Generate colourblind palette for seven continents
palette <- colorblind_pal()(8)

# Plot
all_plot <- ggplot(counts, aes(x = continent, y = n, fill = continent,
                            label = paste(pct, "%", sep = ""))) +
         geom_col(fill = palette[2:8], show.legend = FALSE) +
         geom_text(position = position_dodge(width = 0.9),
                   vjust = -0.5,
                   size = 5) +
         scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
         ylim(0, 5000) +
         labs(x = element_blank(), y = "Number of occurrences") +
         theme_classic(base_size = 16)

# Save
ggsave(file = "figures/Figure_A.pdf", plot = all_plot,
       width = 18, height = 12, units = "cm")

## Ankylosaurs
# Filter
ank <- filter(fossils, family == "Ankylosauridae")

# Count and calculate percentages
ank_counts <- count(ank, continent) %>% mutate(pct = round((n / sum(n) * 100), 1))
ank_counts <- arrange(ank_counts, desc(n))
ank_counts$continent <- factor(ank_counts$continent, levels = ank_counts$continent)

# Plot
ank_plot <- ggplot(ank_counts, aes(x = continent, y = n, fill = continent,
                                   label = paste(pct, "%", sep = ""))) +
       geom_col(fill = palette[2:4], show.legend = FALSE) +
       geom_text(position = position_dodge(width = 0.9),
                 vjust = -0.5,
                 size = 5) +
       scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
       ylim(0, 150) +
       labs(x = element_blank(), y = "Number of occurrences") +
       add_phylopic(uuid = "8a8a2525-e97d-4505-8085-7958f8d36137",
                    x = 2.75, y = 120, alpha = 0.2, height = 15) +
       theme_classic(base_size = 16)

# Save
ggsave(file = "figures/Figure_B.pdf", plot = ank_plot,
       width = 9, height = 12, units = "cm")

## Ceratopsids
# Filter
ctp <- filter(fossils, family == "Ceratopsidae")

# Count and calculate percentages
ctp_counts <- count(ctp, continent) %>% mutate(pct = round((n / sum(n) * 100), 1))
ctp_counts <- arrange(ctp_counts, desc(n))
ctp_counts$continent <- factor(ctp_counts$continent, levels = ctp_counts$continent)

# Plot
ctp_plot <- ggplot(ctp_counts, aes(x = continent, y = n, fill = continent,
                                   label = paste(pct, "%", sep = ""))) +
  geom_col(fill = palette[2:3], show.legend = FALSE) +
  geom_text(position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 5) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  ylim(0, 650) +
  labs(x = element_blank(), y = "Number of occurrences") +
  add_phylopic(uuid = "0388ee19-b40e-46fd-92f5-8ef6b9075590",
               x = 2, y = 500, alpha = 0.2, height = 75) +
  theme_classic(base_size = 16)

# Save
ggsave(file = "figures/Figure_C.pdf", plot = ctp_plot,
       width = 9, height = 12, units = "cm")

## Hadrosaurs
# Filter
hdr <- filter(fossils, family == "Hadrosauridae")

# Count and calculate percentages
hdr_counts <- count(hdr, continent) %>% mutate(pct = round((n / sum(n) * 100), 1))
hdr_counts <- arrange(hdr_counts, desc(n))
hdr_counts$continent <- factor(hdr_counts$continent, levels = hdr_counts$continent)

# Plot
hdr_plot <- ggplot(hdr_counts, aes(x = continent, y = n, fill = continent,
                                   label = paste(pct, "%", sep = ""))) +
  geom_col(fill = c(palette[2:6], palette[8], palette[7]), show.legend = FALSE) +
  geom_text(position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 5) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  ylim(0, 1050) +
  labs(x = element_blank(), y = "Number of occurrences") +
  add_phylopic(uuid = "76779b00-0150-406a-a443-23c534ec80fe",
               x = 6.25, y = 800, alpha = 0.2, height = 175) +
  theme_classic(base_size = 16)

# Save
ggsave(file = "figures/Figure_D.pdf", plot = hdr_plot,
       width = 18, height = 12, units = "cm")

## Tyrannosaurs
# Filter
tyr <- filter(fossils, family == "Tyrannosauridae")

# Count and calculate percentages
tyr_counts <- count(tyr, continent) %>% mutate(pct = round((n / sum(n) * 100), 1))
tyr_counts <- arrange(tyr_counts, desc(n))
tyr_counts$continent <- factor(tyr_counts$continent, levels = tyr_counts$continent)

# Plot
tyr_plot <- ggplot(tyr_counts, aes(x = continent, y = n, fill = continent,
                                   label = paste(pct, "%", sep = ""))) +
  geom_col(fill = palette[2:4], show.legend = FALSE) +
  geom_text(position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 5) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  ylim(0, 400) +
  labs(x = element_blank(), y = "Number of occurrences") +
  add_phylopic(uuid = "499fe1d6-a3c5-4219-a60e-d5ae93031bda",
               x = 2.75, y = 275, alpha = 0.2, height = 65) +
  theme_classic(base_size = 16)

# Save
ggsave(file = "figures/Figure_E.pdf", plot = tyr_plot,
       width = 9, height = 12, units = "cm")

# Create and arrange composite plot
#composite_plot <- ggarrange(all_plot, ank_plot, ctp_plot, hdr_plot, tyr_plot,
#          labels = c("A", "B", "C", "D", "E"),
#          ncol = 3, nrow = 2)

# Save
#ggsave(file = "figures/Figure.pdf", plot = composite_plot,
#       width = 40, height = 25, units = "cm")

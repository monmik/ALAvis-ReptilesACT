# Import downloaded csv
df <- read.csv("Data/downloaded_records.csv")

# Clean and Organise Data ----
# Find NAs
na_count <- sapply(df, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)

# Find empty
empty_count <-sapply(df, function(y) sum(length(which(y == ""))))
empty_count <- data.frame(empty_count)


# Remove columns with Empty and NA values
df_full <- df[ , (na_count == 0) & (empty_count == 0)]
  # Left with 14 variables

# Try keep colums that have are at least 80% full?
num_obs <- dim(df)[1]
df_80 <- df[ , (na_count < .2*num_obs) & (empty_count < .2*num_obs)]
  # Left with 31 more interesting variables..

# Try keep colums that have are at least 70% full?
df_70 <- df[ , (na_count < .3*num_obs) & (empty_count < .3*num_obs)]
  # Still 31.. leave it there..

# Remove rows with na's
df_80_complete <- na.omit(df_80)
  # Left with 8623 obs!

# Remove rows with empty cells
df_80_complete <- df_80_complete[!apply(df_80_complete == "", 1, any), ] 
  # Left with 7968 obs! Hopefully representative...

# Keep columns of interest
chosen_data <- df_80_complete[, c("eventDate", 
                                  "decimalLatitude", 
                                  "decimalLongitude", 
                                  "taxonConceptID", 
                                  "scientificName", 
                                  "kingdom", 
                                  "phylum", 
                                  "class", 
                                  "order",
                                  "family",
                                  "genus", 
                                  "vernacularName")]
# geo_code <- df_80_complete$geodeticDatum[1]

chosen_data$eventDate <- as.Date(chosen_data$eventDate)

# Define factors
for(i in 5:length(chosen_data)){ # starting at 5th column (scientificName)
  chosen_data[, i] <- as.factor(chosen_data[, i])
}

# Visualise! ----
library(tidyverse)
library(sf)
library(mapview)

## Create sf object and map ----
chosen_data.sf <-
  st_as_sf(chosen_data, 
           coords = c("decimalLongitude", "decimalLatitude"),  
           crs = 4326)
mapview(chosen_data.sf)

## Uh Oh! some of these are not in the ACT! ----
##   Getting extreme value of longitute visually using the observations
##    on the map
chosen_data_act <- chosen_data[chosen_data$decimalLongitude < 150, ]
  # Now with 7741 obs

## Try again: Create sf object and map ----
 # This is updated below, commented out here.
 # chosen_data_act.sf <- 
 #  st_as_sf(chosen_data_act, 
 #          coords = c("decimalLongitude", "decimalLatitude"),  
 #           crs = 4326)
 # map_reptiles_ACT <-
 #  mapview(chosen_data_act.sf, zcol="kingdom",
 #          alpha.regions = 0.2, alpha = 0.5, label = "vernacularName") +
 #    mapview(chosen_data_act.sf, zcol="phylum", hide = TRUE,
 #            alpha.regions = 0.2, alpha = 0.5, label = "vernacularName") +
 #    mapview(chosen_data_act.sf, zcol="class", hide = TRUE,
 #            alpha.regions = 0.2, alpha = 0.5, label = "vernacularName") +
 #    mapview(chosen_data_act.sf, zcol="order", hide = TRUE,
 #            alpha.regions = 0.2, alpha = 0.5, label = "vernacularName") +
 #  mapview(chosen_data_act.sf, zcol="family", hide = TRUE,
 #          alpha.regions = 0.2, alpha = 0.5, label = "vernacularName") +
 #  mapview(chosen_data_act.sf, zcol="genus", hide = TRUE,
 #          alpha.regions = 0.2, alpha = 0.5, label = "vernacularName") +
 #  mapview(chosen_data_act.sf, zcol="scientificName", hide = TRUE,
 #         alpha.regions = 0.2, alpha = 0.5, label = "vernacularName")

# Plot some charts ----
library(ggplot2)
library(viridis)   

# min(chosen_data_act$eventDate)
# max(chosen_data_act$eventDate)

## Observations over time ----
time_plot <- 
  ggplot(chosen_data_act, aes(x=eventDate))+
    geom_bar(aes(fill=family))+
    scale_fill_viridis(discrete = TRUE)+
    # facet_wrap(~order, scales = "free")+
    labs(x="Event Date", y= "Number of Records", fill="Family")+
    theme(legend.position = "bottom", 
          panel.background = element_rect(fill = "white", colour = "grey50")) +
  ylim(0, 105) 
# time_plot

chosen_data_act_2000s <- chosen_data_act[chosen_data_act$eventDate >= "2000-01-01",]
# 5055 obs remaining.. not bad..

# Obs in 2000s
time_plot_2000s <- 
  ggplot(chosen_data_act_2000s, aes(x=eventDate))+
  geom_bar(aes(fill=family))+
  scale_fill_viridis(discrete = TRUE)+
  # facet_wrap(~order, scales = "free")+
  labs(x="Event Date", y= "Number of Records", fill="Family")+
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "white", colour = "grey50")) 
# time_plot_2000s

# Still not very clear... log scale? 
time_plot_2000s.log2 <- 
  ggplot(chosen_data_act_2000s, aes(x=eventDate))+
  geom_bar(aes(fill=order), width=1)+
  scale_fill_viridis(discrete = TRUE)+
  # facet_wrap(~order, scales = "free")+
  labs(x="Event Date", y= "Number of Records", fill="Order")+
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "white", colour = "grey50"))+ 
  scale_y_continuous(trans = "log2") 
# time_plot_2000s.log2
#   Looks like maximum sightings in summer! Do reptiles hibernate in winter? 

# Update the map with the subset
chosen_data_act_2000s.sf <- 
  st_as_sf(chosen_data_act_2000s, 
           coords = c("decimalLongitude", "decimalLatitude"),  
           crs = 4326)
map_reptiles_ACT_2000s <- 
  mapview(chosen_data_act_2000s.sf, zcol="kingdom", hide = TRUE,
          alpha.regions = 0.2, alpha = 0.5, label = "vernacularName", 
          layer.name = "Kingdom") +
  mapview(chosen_data_act_2000s.sf, zcol="phylum", hide = TRUE,
          alpha.regions = 0.2, alpha = 0.5, label = "vernacularName",  
          layer.name = "Phylum") +
  mapview(chosen_data_act_2000s.sf, zcol="class", hide = TRUE,
          alpha.regions = 0.2, alpha = 0.5, label = "vernacularName",
          layer.name = "Class") +
  mapview(chosen_data_act_2000s.sf, zcol="order", hide = TRUE,
          alpha.regions = 0.2, alpha = 0.5, label = "vernacularName",
          layer.name = "Order") +
  mapview(chosen_data_act_2000s.sf, zcol="family", hide = TRUE,
          alpha.regions = 0.2, alpha = 0.5, label = "vernacularName",
          layer.name = "Family") +
  mapview(chosen_data_act_2000s.sf, zcol="genus", hide = TRUE,
          alpha.regions = 0.2, alpha = 0.5, label = "vernacularName",
          layer.name = "Genus") +
  mapview(chosen_data_act_2000s.sf, zcol="scientificName",
          alpha.regions = 0.2, alpha = 0.5, label = "vernacularName",
          layer.name = "Species")

# Look at family and genus
family_bar_2000s <- 
  ggplot(chosen_data_act_2000s, aes(x=genus))+
  geom_bar(aes(fill=family), width=1)+
  scale_fill_viridis(discrete = TRUE)+
  # facet_wrap(~order, scales = "free")+
  labs(x="Genus", y= "Number of Records", fill="Family")+
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        panel.background = element_rect(fill = "white", colour = "grey50")) 
# family_bar_2000s

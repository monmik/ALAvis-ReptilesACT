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
# Define date class 
chosen_data$eventDate <- as.Date(chosen_data$eventDate)

# Define factors
for(i in 5:length(chosen_data)){ # starting at 5th column (scientificName)
  chosen_data[, i] <- as.factor(chosen_data[, i])
}

## Remove locations outside the ACT (mislabelled?) ----
##   Getting rough cutoff value for longitute visually using the map
chosen_data_act <- chosen_data[chosen_data$decimalLongitude < 150, ]
  # Now with 7741 obs

# Only include records since 2000
chosen_data_act_2000s <- chosen_data_act[chosen_data_act$eventDate >= "2000-01-01",]
  # 5055 obs remaining..

# Visualise! ----

## Map ----
 # Shows spatial distribution of individual reports and dates
library(tidyverse)
library(sf)
library(mapview)

 # Create map object 
chosen_data_act_2000s.sf <- 
  st_as_sf(chosen_data_act_2000s, 
           coords = c("decimalLongitude", "decimalLatitude"),  
           crs = 4326)

# Add layers based on taxonomic rank
  # First layer = species
map_reptiles_ACT_2000s <-  
  mapview(chosen_data_act_2000s.sf, zcol="scientificName",
          alpha.regions = 0.2, alpha = 0.5, label = "vernacularName",
          layer.name = "Species")+
  mapview(chosen_data_act_2000s.sf, zcol="genus", hide = TRUE,
          alpha.regions = 0.2, alpha = 0.5, label = "vernacularName",
          layer.name = "Genus")+
  mapview(chosen_data_act_2000s.sf, zcol="family", hide = TRUE,
          alpha.regions = 0.2, alpha = 0.5, label = "vernacularName",
          layer.name = "Family") +
  mapview(chosen_data_act_2000s.sf, zcol="order", hide = TRUE,
          alpha.regions = 0.2, alpha = 0.5, label = "vernacularName",
          layer.name = "Order")  +
  mapview(chosen_data_act_2000s.sf, zcol="class", hide = TRUE,
          alpha.regions = 0.2, alpha = 0.5, label = "vernacularName",
          layer.name = "Class") +
  mapview(chosen_data_act_2000s.sf, zcol="phylum", hide = TRUE,
          alpha.regions = 0.2, alpha = 0.5, label = "vernacularName",  
          layer.name = "Phylum") +
  mapview(chosen_data_act_2000s.sf, zcol="kingdom", hide = TRUE,
          alpha.regions = 0.2, alpha = 0.5, label = "vernacularName", 
          layer.name = "Kingdom") 

## Charts ----
library(ggplot2)
library(viridis)   

 # Group by dates and get count info
library(dplyr)
chosen_data_act_2000s_byDate <-
  chosen_data_act_2000s %>%
  group_by(eventDate) %>%
  tally()

 # Plot reports over time
time_plot_2000s_byDate <- 
  ggplot(chosen_data_act_2000s_byDate, aes(x=eventDate, y = n))+
  geom_line(linewidth=.25)+
  labs(x="Event Date", y= "Number of Records (Log Scale)", fill="Family")+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"), 
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  scale_y_continuous(trans = "log10") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", 
               limits=c(min(chosen_data_act_2000s_byDate$eventDate), 
                        max(chosen_data_act_2000s_byDate$eventDate)))
  # time_plot_2000s_byDate


# Plot Genus counts in the area, coloured by Family
family_bar_2000s <- 
  ggplot(chosen_data_act_2000s, aes(x=genus))+
  geom_bar(aes(fill=family), width=1)+
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Genus", y= "Number of Records", fill="Family")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
        panel.background = element_rect(fill = "white", colour = "grey50"))
family_bar_2000s


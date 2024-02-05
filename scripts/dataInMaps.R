#data wrangling using ML paper and how to do causal inference

library(tidyverse)
library(viridis)

load("data/MgmtPhenWeather.RData")

#add a PD.yday variable
soydf <- ncsrpII |> 
  mutate(month_numeric = match(tolower(PD.month), tolower(month.abb)),
         PD.ymd = ymd(paste(year, month_numeric, PD.day, sep = "-")),
         PD.yday = yday(PD.ymd)) |> 
  select(-month_numeric, -PD.ymd)


#---- visualize the PD, MG and Yield
ggplot(
  data = soydf,
  mapping = aes(x = PD.yday, y = yield)
) +
  geom_point(aes(color = as.factor(MG.f)), size = 1.5) +
  #geom_smooth(method = lm, se = TRUE) +
  geom_smooth(se = TRUE) +
  facet_wrap(~ state) +
  scale_color_viridis(discrete = TRUE) +
  labs(color = "MG")


#Visualize the data points on a map
library(maps)
library(mapdata)

# remove NA's from MG.f variable and visualize
soydf_cleaned <- soydf[complete.cases(soydf$MG.f), ]

#
us_states <- map_data("state")
ggplot() +
  geom_polygon(data = us_states, aes(x = long, y = lat, group = group), fill = "aliceblue", color = "black") +
  geom_point(data = soydf, aes(x = longitude, y = latitude), color = "firebrick3", size = 1) +
  theme_minimal() +
  theme(panel.grid = element_blank())

#visualize map
ggplot() +
  geom_polygon(data = us_states, aes(x = long, y = lat, group = group), fill = "aliceblue", color = "black") +
  geom_point(data = soydf_cleaned, aes(x = longitude, y = latitude, color = MG.f), size = 1) +
  theme_minimal() +
  theme(panel.grid = element_blank())
##-----------

#plot the state map
library(sf)
library(tigris)

# Load the shapefile for Iowa counties
iowa_counties <- tigris::counties(state = "IA", cb = TRUE)
wisconsin_counties <- tigris::counties(state = "WI", cb = TRUE)

#filter iowa data
IA_soydf_cleaned <- soydf_cleaned |> 
  filter(state == "IA")

# Plot the map
ggplot() +
  geom_sf(data = iowa_counties, fill = "lightblue", color = "white", size = 0.2) +
  geom_point(data = IA_soydf_cleaned, aes(x = longitude, y = latitude), color = "red", size = 1) +
  labs(title = "Iowa soybean data") +
  theme_minimal()

### Use st_write to export the data to a shapefile
#st_write(iowa_counties, "outputs/iowa_counties.shp")
#st_write(wisconsin_counties, "outputs/wisconsin/wisconsin_counties.shp")
## Iowa points and planting date
#write.csv(IA_soydf_cleaned, "outputs/IA_soydf.csv")

ggplot() +
  geom_sf(data = wisconsin_counties, fill = "lightblue", color = "white", size = 0.2) 

#====================== countywise state maps ==================================


library(ggplot2)
library(sf)
library(tigris)

# Get the shapefile for Iowa counties
# Load the shapefile for Iowa counties
iowa_counties <- tigris::counties(state = "IA", cb = TRUE)

# Plot the map
ggplot() +
  geom_sf(data = iowa_counties, fill = "lightblue", color = "white", size = 0.2) +
  labs(title = "Iowa Counties Map") +
  theme_minimal()


# this script is created to subset the data for spatial join in ArcGis
# Needed cols are lat long and PD

library(tidyverse)
library(viridis)

load("data/MgmtPhenWeather.RData")

# add a PD.yday variable
soydf <- ncsrpII |> 
  mutate(month_numeric = match(tolower(PD.month), tolower(month.abb)),
         PD.ymd = ymd(paste(year, month_numeric, PD.day, sep = "-")),
         PD.yday = yday(PD.ymd)) |> 
  select(-month_numeric, -PD.ymd)

# subset only needed vars
soydf_cord <- ncsrpII |> 
  select(order, id, state, latitude, longitude, PD.year, p.date)

# export the csv file
write.csv(soydf_cord, "outputs/soy_pdate.csv", row.names = FALSE)

#data except kansas
soydf_noKS <- soydf_cord |> 
  filter(state != "KS")
  

##
#data only kansas
soydf_KS <- soydf_cord |> 
  filter(state == "KS")


##
ggplot(
  data = soydf_noKS,
  mapping = aes( x= state, y = PD.yday)
) +
  geom_point()

#write.csv(soydf_noKS, "outputs/soydf_noKS.csv", row.names = FALSE)

#------ check the data exported from ArcGIS
noKSjoined <- read_csv("data/noKSjoined.csv")

soyPD_noKS <- noKSjoined |> 
  select(order_, id, state, latitude, longitude, 
         PD_year, PD_month, PD_day, PD_yday, P_Date) |> 
  rename(order = order_)

# for row 2436, 2437, 2438 replace PD with 30-Apr
# for row 4768, replace PD with 30-Apr

#================ Merge noKS data and KS data===================================
# load two data sets with and without KS and concatenate
# first clear the headers

library(tidyverse)
library(readr)
noKS <- read_csv("data/noKSjoined.csv")

noKS_pdate <- noKS |> 
  select(order_, id, state, latitude, longitude, PD_year, PD_month, PD_day, PD_yday, P_Date) |> 
  rename(order = order_)

##
KS_pdate <- read_csv("data/KS_pdateJoined.csv")
onlyKS_pdate <- KS_pdate |> 
  select(order_, id, state, latitude, longitude, PD_year, PD_month, PD_day, PD_yday, P_Date) |> 
  rename(order = order_)

## concatenate the two 

soy_pdate <- rbind(noKS_pdate, onlyKS_pdate) |> 
  arrange(order)


#Merge df1 and df2 based on the "order" column, filling missing values with NA
soydf <- merge(ncsrpII, soy_pdate[, c("order", "PD_yday", "P_Date")], by = "order", all.x = TRUE)

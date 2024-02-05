# Data preparation for caual model
library(tidyverse)

soydf_Pdate <- read_csv("data/soydf_Pdate.csv")

#change the letter date to dayOfYear
# Convert the character dates to Date objects
soydf_Pdate$P_Date <- as.Date(soydf_Pdate$P_Date, format = "%d-%b")

# Add a new column "P_Rdoy" containing day of year values
soydf_Pdate$P_Rdoy <- as.integer(format(soydf_Pdate$P_Date, "%j"))

##-->
#calculate the diff between the P_date and P_Rdoy
soydf_Pdate$diff_pd_rpd <- as.integer(soydf_Pdate$PD_yday - soydf_Pdate$P_Rdoy)
##

##-------------------Stratified by PD recommendation----------------------------
#                        Data exploration  

obsData <- dplyr::select(soydf_Pdate, c(latitude, longitude, yield, MG.f, PD_yday,  
                                        P_Rdoy, diff_pd_rpd, previous.crop))
obsData$Y <- obsData$yield
factors <- c("MG.f", "previous.crop")
obsData[factors] <- lapply(obsData[factors], as.factor)

# convert the treatment A(pd on time vs pd not on time) 
# change the treatment assignment based on P.Date decision 
# T = 1 if Planted on RPD (within ± 10 Recommended PD) = 1
# T= 0 if planted before or after recommended date

#--->
obsData$A <- ifelse((obsData$PD_yday - obsData$P_Rdoy) >= -10 & 
                      (obsData$PD_yday - obsData$P_Rdoy)<= 10, 1, 0)

# relabeling the previous crop ------------------------------------------------|

obsData$prev.crop.type <- ifelse(obsData$previous.crop %in% c("oats","sorghum", "barley", "rye","popcorn"), "other.cereals", 
                                 ifelse (obsData$previous.crop == "corn", "corn", 
                                         ifelse (obsData$previous.crop == "soybean", "soybean", 
                                                 ifelse(obsData$previous.crop == "wheat", "wheat",
                                                        "others"))))
obsData <- obsData |> 
  select(-previous.crop)

save(obsData, file = "data/obsData.RData")

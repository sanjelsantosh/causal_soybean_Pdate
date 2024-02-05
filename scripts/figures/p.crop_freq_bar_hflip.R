# horizontal frequency bar chart 
# previous crop

load("data/obsData.RData")
data <- obsData
rm(obsData)

library(tidyverse)

ggplot(
  data = obsData,
  mapping = aes(x = after_stat(count), y = previous.crop)
) +
  geom_bar(stat = "count") +
  geom_text(aes(label = after_stat(count)), stat = "count", hjust = -0.2) +
  theme_bw()
  

##------------------

ggplot(
  data = data,
  mapping = aes( x= after_stat(count), y = MG.f)
) +
  geom_bar(stat = "count")

##



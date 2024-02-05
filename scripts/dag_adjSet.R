# Find minimum adjustment set 
library(dagitty)

p <- dagitty("dag {
  Crop_growth -> Harvest_date
  Crop_growth -> Yield
  Harvest_date -> Yield
  NPK_application -> Crop_growth
  Planting_Date -> Crop_growth
  crop_rotation -> Crop_growth
  disease_severity -> Crop_growth
  disease_severity -> Yield
  maturity_group -> Crop_growth
  maturity_group -> Yield
  planting_depth -> Crop_growth
  row_width -> Crop_growth
  seeding_rate -> Crop_growth
  soil_moisture_on_sowing -> Crop_growth
  soil_moisture_on_sowing -> Planting_Date
  soil_pH -> Crop_growth
  soil_properties -> Crop_growth
  Variety -> Crop_growth
  Variety -> Planting_Date
  Variety -> maturity_group
  Variety -> Yield
  latitude -> Crop_growth
  latitude -> Planting_Date
  longitude -> Crop_growth
  longitude -> Planting_Date
}")


plot(p)

adjustmentSets(p, "Planting_Date", "Yield")

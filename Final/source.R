### Dependencies
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse); library(tidymodels) # Tidy suites
library(sf); library(geojsonsf) # Spatial packages
library(here); library(patchwork); library(gt); library(rmarkdown) # Utilities

### working directory
setwd(here::here())

### Data
properties <- read_csv("data/FML/Nov9_FML_OwnOccUpdated.csv") # Property ownership data
# gentrification <- read_csv("data/Gentrification/Gentrification_1990-2017.csv") # Gentrification Index data
depaul_ihs <- read_csv("data/DePaul IHS/IHS_HousingStockComposition.csv") # Housing stock composition data
community_areas <- geojson_sf("data/Boundaries/community_areas.geojson") # Shapefiles of Community Areas for mapping

lm_mod <- linear_reg() %>%
  set_engine("lm")

depaul_ihs <- depaul_ihs %>%
  mutate(
    Community.Area = toupper(Geography),
    Community.Area = ifelse(Community.Area == "O'HARE", "OHARE", Community.Area),
    share_black = `Share of Population that is African American`
  ) %>%
  filter(
    Community.Area != "CHICAGO TOTAL"
  )

properties <- properties %>%
  mutate(Owner.Occupancy = factor(Owner.Occupancy))


properties_sf <- st_as_sf(properties, coords = c("Long", "Lat"),
                          crs = 26971, agr = "constant")

community_areas <- st_transform(community_areas, crs = 26971)

property_owners <- properties %>% 
  group_by(Taxpayer.Match.Code) %>%
  summarize(
    properties = n_distinct(Property.Index.Number),
    property_cat = case_when(
      properties == 1 ~ "P 1",
      properties >= 2 & properties <= 5 ~ "P 2 - 5",
      properties >= 6 & properties <= 15 ~ "P 6 - 15",
      properties > 15 ~ "P >15"
    ) %>% factor(levels = c("P 1", "P 2 - 5", "P 6 - 15", "P >15")),
    units_estimate = sum(Relative.Size),
    units_cat = case_when(
      units_estimate == 1 ~ "UC 1",
      units_estimate >= 2 & units_estimate <= 4 ~ "UC 2 - 4",
      units_estimate >= 5 & units_estimate <= 12 ~ "UC 5 - 12",
      units_estimate >= 13 & units_estimate <= 30 ~ "UC 13 - 30",
      units_estimate >= 31 & units_estimate <= 100 ~ "UC 31 - 100",
      units_estimate >= 100 ~ "UC >100"
    ) %>% factor(levels = c("UC 1", "UC 2 - 4", "UC 5 - 12", "UC 13 - 30", "UC 31 - 100", "UC >100")),
    affiliated_owner = Affiliated.With
    ) %>%
  unique()

properties_sf <- property_owners %>%
  select(Taxpayer.Match.Code, 
         "Properties_Owned" = properties,
         "Properties_Owned_Cat" = property_cat,
         "Units_Owned_Estimate" = units_estimate,
         "Units_Owned_Cat" = units_cat) %>%
  right_join(properties_sf, by = "Taxpayer.Match.Code")

properties_cca <- properties_sf %>%
  mutate(Community.Area = toupper(Community.Area)) %>%
  group_by(Community.Area) %>%
  count(
    Properties_Owned_Cat,
    total_unit_est = sum(Relative.Size)
    ) %>%
  pivot_wider(names_from = Properties_Owned_Cat, values_from = n, values_fill = 0) %>%
  right_join(community_areas, by = c("Community.Area" = "community")) %>%
  mutate(Total_Props = `P 1` + `P 2 - 5` + `P 6 - 15` + `P >15`)
  
properties_cca <- properties_cca %>%
  select(Community.Area, area_num_1, `P 1`, `P 2 - 5`, `P 6 - 15`, `P >15`, Total_Props, total_unit_est, shape_area, geometry)

units_cca <- properties_sf %>%
  mutate(Community.Area = toupper(Community.Area)) %>%
  group_by(Community.Area) %>%
  count(
    Units_Owned_Cat,
    total_unit_est = sum(Relative.Size)
    ) %>%
  pivot_wider(names_from = Units_Owned_Cat, values_from = n, values_fill = 0) %>%
  right_join(community_areas, by = c("Community.Area" = "community")) %>%
  mutate(Total_Props = `UC 1` + `UC 2 - 4` + `UC 5 - 12` + `UC 13 - 30` + `UC 31 - 100` + `UC >100`)

units_cca <- units_cca %>%
  select(Community.Area, area_num_1, `UC 1`:`UC >100`, Total_Props, total_unit_est, shape_area, geometry)

prop_cca_full <- properties_cca %>%
  # right_join(gentrification, by = c("Community.Area" = "CCA"))  %>%
  right_join(depaul_ihs) %>%
  mutate(renter_hh = `Share of Households that are Renter-Occupied` * `Total Households`)

units_cca_full <- units_cca %>%
  right_join(depaul_ihs) %>%
  mutate(renter_hh = `Share of Households that are Renter-Occupied` * `Total Households`)

prop_cca_plot <- properties_cca %>%
  ggplot(aes(geometry = geometry)) +
  geom_sf(aes(fill = `P >15`))

units_cca_plot <- units_cca %>%
  ggplot(aes(geometry = geometry)) +
  geom_sf(aes(fill = `UC >100`))

prop_unithh_plot <- prop_cca_full %>%
  ggplot() +
  geom_histogram(aes(x = renter_hh, fill = "blue"), alpha = .5) +  
  geom_histogram(aes(x = total_unit_est, fill = "red"), alpha = .5)


lm_prop_unit <- lm_mod %>%
  fit(units_estimate ~ properties, data = property_owners)

hivol_prop_ownocc <- properties_sf %>%
  filter(
    Properties_Owned > 15, Owner.Occupancy == "Multiple building, owner-occupied"
  )

hivol_unit_ownocc <- properties_sf %>%
  filter(
    Units_Owned_Estimate > 100, Owner.Occupancy == "Multiple building, owner-occupied"
  )

hivol_prop <- properties_sf %>%
  filter(Properties_Owned > 15) 

hivol_unit <- properties_sf %>%
  filter(Units_Owned_Estimate > 100)

conc_hivol_prop_cca <- prop_cca_full %>%
  group_by(Community.Area) %>%
  summarize(
    hivol = `P >15`,
    total_props = Total_Props,
    perc_hivol = `P >15` / Total_Props,
    perc_rent = `Share of Households that are Renter-Occupied`,
    total_households = `Total Households`,
    geometry = geometry
  )

conc_hivol_unit_cca <- units_cca_full %>%
  group_by(Community.Area) %>%
  summarize(
    hivol = `UC >100`,
    total_props = Total_Props,
    perc_hivol = `UC >100` / Total_Props,
    perc_rent = `Share of Households that are Renter-Occupied`,
    total_households = `Total Households`,
    geometry = geometry
  )

hivol_prop_cca_plot <- conc_hivol_prop_cca %>%
  ggplot(aes(x = Community.Area, y = perc_hivol)) +
  geom_col() +
  coord_flip()

hivol_prop_cca_map <- conc_hivol_prop_cca %>%
  ggplot(aes(geometry = geometry)) +
  geom_sf(aes(fill = perc_hivol))

hivol_unit_cca_plot <- conc_hivol_unit_cca %>%
  ggplot(aes(x = Community.Area, y = perc_hivol)) +
  geom_col() +
  coord_flip()

hivol_unit_cca_map <- conc_hivol_unit_cca %>%
  ggplot(aes(geometry = geometry)) +
  geom_sf(aes(fill = perc_hivol))

socecon_hivol_props_cca <-prop_cca_full %>%
  group_by(Community.Area) %>%
  summarize(
    hivol = sum(`P >15`),
    total = sum(Total_Props),
    perc_hivol = hivol / total,
    black = share_black,
    poverty = `Share of Population that is under the Poverty Level`,
    renter = `Share of Households that are Renter-Occupied`
  )

socecon_hivol_unit_cca <- units_cca_full %>%
  group_by(Community.Area) %>%
  summarize(
    hivol = sum(`UC >100`),
    total = sum(Total_Props),
    perc_hivol = hivol / total,
    black = share_black,
    poverty = `Share of Population that is under the Poverty Level`,
    renter = `Share of Households that are Renter-Occupied`
  )

lm_socecon_prop <- lm_mod %>%
  fit(perc_hivol ~ black + poverty + renter, data = socecon_hivol_props_cca)

lm_socecon_unit <- lm_mod %>%
  fit(perc_hivol ~ black + poverty + renter, data = socecon_hivol_unit_cca)

poverty_hivol_props_plot <- socecon_hivol_props_cca %>%
  ggplot(aes(x = poverty, y = perc_hivol)) + 
  geom_point() +
  geom_smooth(method = "lm")

renter_hivol_props_plot <- socecon_hivol_props_cca %>%
  ggplot(aes(x = renter, y = perc_hivol)) + 
  geom_point() +
  geom_smooth(method = "lm")

poverty_hivol_unit_plot <- socecon_hivol_unit_cca %>%
  ggplot(aes(x = poverty, y = perc_hivol)) +
  geom_point() +
  geom_smooth(method = "lm")

renter_hivol_unit_plot <- socecon_hivol_unit_cca %>%
  ggplot(aes(x = renter, y = perc_hivol)) +
  geom_point() +
  geom_smooth(method = "lm")

OwnOcc_cca <- properties_sf %>% # Find counts of single building owner-occupied properties
  group_by(Community.Area) %>%
  count(OwnOcc = Owner.Occupancy == "Single building, owner-occupied") %>%
  mutate(
    Total = sum(n),
    Community.Area = toupper(Community.Area)
  ) %>%
  filter(OwnOcc == TRUE) %>%
  mutate(OwnOcc = n) %>%
  select(Community.Area, OwnOcc, Total)


OwnOcc_cca <- OwnOcc_cca %>% # Joining to spatial and housing stock data
  right_join(depaul_ihs) %>%
  right_join(community_areas, by = c("Community.Area" = "community"))

OwnOcc_cca <- OwnOcc_cca  %>% # Setting up final dataset
  mutate(
    area = st_area(geometry),
    perc_ownocc = OwnOcc / Total,
    HH_per_meter_sq_tenthousands = as.numeric((`Total Households` / area) * 10000)
  ) %>%
  select(Community.Area, OwnOcc, Total, perc_ownocc, `Total Households`, HH_per_meter_sq_tenthousands, area, geometry)

lm_ownocc_density <- lm_mod %>%
  fit(perc_ownocc ~ HH_per_meter_sq_tenthousands, data = OwnOcc_cca)

OwnOcc_cca_plot <- OwnOcc_cca %>%
  ggplot(aes(x = HH_per_meter_sq_tenthousands, y = perc_ownocc)) +
  geom_point() +
  geom_smooth(method = "lm")

sales_prop_cca <- prop_cca_full %>%
  group_by(Community.Area) %>%
  summarize(
    hivol = `P >15`,
    perc_hivol = `P >15` / Total_Props,
    perc_rent = `Share of Households that are Renter-Occupied`,
    sales_5yr_avg = `Average Yearly Property Sales Activity 2015 - 2019 - Sales per 100 Residential Parcels`,
    total_households = `Total Households`,
    geometry = geometry
  )

sales_unit_cca <- units_cca_full %>%
  group_by(Community.Area) %>%
  summarize(
    hivol = `UC >100`,
    perc_hivol = `UC >100` / Total_Props,
    perc_rent = `Share of Households that are Renter-Occupied`,
    sales_5yr_avg = `Average Yearly Property Sales Activity 2015 - 2019 - Sales per 100 Residential Parcels`,
    total_households = `Total Households`,
    geometry = geometry
  )

lm_sales_prop <- lm_mod %>%
  fit(perc_hivol ~ sales_5yr_avg, data = sales_prop_cca)

lm_sales_unit <- lm_mod %>%
  fit(perc_hivol ~ sales_5yr_avg, data = sales_unit_cca)

sales_prop_cca_plot <- sales_prop_cca %>%
  ggplot(aes(x = sales_5yr_avg, y = perc_hivol)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(y = "Share of Properties Owned by High-Volume Landlords",
       x = "5yr Average, Residential Sales",
       title = "Property Ownership and Residential Sales")

sales_unit_cca_plot <- sales_unit_cca %>%
  ggplot(aes(x = sales_5yr_avg, y = perc_hivol)) +
  geom_point() +
  geom_smooth(method = "lm")
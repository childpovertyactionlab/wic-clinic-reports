library(tidyverse)
library(tidycensus)
library(readxl)
library(sf)
library(ggplot2)
library(plotly)
library(leaflet)
library(googlesheets4)

gs4_auth(email = "taylor@childpovertyactionlab.org")
clinic_map <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1pCkVgjP4VLtNDwIHYbCm7wcYOtFqaAZu2znO-N7eXK8/",
  range = "Clinic Attributes") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs = 4269)

df <- read_excel("C:/Users/twcro/Documents/CPAL/Participants Advanced Find View 6-22-2023 4-31-11 PM.xlsx")

df1 <- df %>%
  group_by(`Address 1: ZIP/Postal Code (Family) (Family)`) %>%
  summarize(count = n())

participation_numbers <- df %>%
  filter(`Status Reason` == "Active" | `Status Reason` == "Provisional") %>%
  group_by(`Clinic # (Clinic) (Local Agency and Clinic)`) %>%
  summarize(total = n()) %>%
  mutate(`Clinic # (Clinic) (Local Agency and Clinic)` = as.numeric(`Clinic # (Clinic) (Local Agency and Clinic)`))

pop <- get_acs(geography = "zip code tabulation area",
               variables = "B01003_001",
               year = 2019,
               state = "TX",
               survey = "acs5")
pop$GEOID <- as.character(pop$GEOID)

zips <- tigris::zctas(state="TX", year = 2010)

clinic_zip <- st_join(clinic_map, zips, join = st_within) %>%
  merge(., participation_numbers, by.x = "Site", by.y = "Clinic # (Clinic) (Local Agency and Clinic)")

cutoff = 0.075
joined <- left_join(zips, pop, by = c("ZCTA5CE10" = "GEOID")) %>%
  left_join(df1, by = c("ZCTA5CE10" = "Address 1: ZIP/Postal Code (Family) (Family)")) %>%
  drop_na(count) %>%
  mutate(density = ifelse(count / estimate < cutoff, count / estimate, cutoff))

leaflet(data = joined) %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addPolygons(fillColor = ~colorNumeric(palette = "viridis", domain = joined$density)(density),
              fillOpacity = 0.7,
              color = "#BDBDC3",
              weight = 1,
              popup = ~paste(ZCTA5CE10, ":", count, "participants"),
              label = ~ZCTA5CE10) %>%
  addLegend(pal = colorNumeric(palette = "viridis", domain = joined$density),
            values = ~density,
            opacity = 0.7,
            title = "Participant Density",
            position = "bottomright")


df2 <- df %>%
  group_by(`Clinic # (Clinic) (Local Agency and Clinic)`, `Address 1: ZIP/Postal Code (Family) (Family)`) %>%
  count() %>%
  ungroup() %>%
  mutate(`Clinic # (Clinic) (Local Agency and Clinic)` = as.numeric(`Clinic # (Clinic) (Local Agency and Clinic)`))

merged_df <- merge(clinic_zip, df2, by.x = c("Site", "ZCTA5CE10"), by.y = c("Clinic # (Clinic) (Local Agency and Clinic)", "Address 1: ZIP/Postal Code (Family) (Family)"), all.x = TRUE) %>%
  mutate(pct_inZIP = ifelse(is.na(n/total), 0, n/total))

ggplot(merged_df, aes(x = Clinic, y = pct_inZIP, fill = Clinic)) +
  geom_bar(stat = "identity", width = 0.7) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Clinics", y = "Percentage in same ZIP", title = "Percentage of Patients Served in the Same ZIP Code per Clinic") +
  theme(legend.position = "none")

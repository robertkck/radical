# Data prep

library(tidyverse)
library(rgdal)
library(sp)

# Load patent attributes -----------------------------------------------------------------

ipc <- read_delim("data/REGPAT/EPO/201803_EPO_IPC.txt", delim = "|") %>% 
  janitor::clean_names() 

ipc <- ipc %>% 
  select(appln_id, year = prio_year, year_app = app_year, ipc) %>% 
  mutate(ipc = str_sub(ipc, 1, 4)) %>% distinct() %>% 
  group_by(appln_id, year_app, year) %>% nest(.key = "ipc") 

saveRDS(ipc, file = "data/ipc.RDS")

radical <- read_delim("data/quality/201803_OECD_PATENT_QUALITY_EPO.txt", delim = "|")
radical <- radical %>% select(appln_id, tech_field, many_field, breakthrough, radicalness)

saveRDS(radical, file = "data/radical.RDS")

# Applicants --------------------------------------------------------------

df_app <- read_delim("data/REGPAT/EPO/201803_EPO_App_reg.txt", delim = "|") %>% 
  janitor::clean_names()

df_app <- df_app %>%
  select(appln_id,
         person_id,
         reg = reg_code,
         ctry = ctry_code,
         reg_share,
         app_share)

ppat <- read_csv("data/EEE_PPAT/EEE-PPAT_2017b.csv",
                 col_types = cols_only(
                   person_id = col_integer(),
                   sector =col_character(),
                   hrm_l2_id = col_integer()
                 )
)

df_app <- df_app %>% left_join(ppat, by = "person_id")
df_app <- df_app %>% 
  count(appln_id, sector) %>% 
  group_by(appln_id) %>% 
  mutate(
    s = paste0(sector, collapse = ", "), 
    s = fct_lump(s, 13), # Ad-hoc decision to only keep the top 13 combinations
    n_applicants = sum(n, na.rm = T),
    n_applicant_type = n_distinct(sector)
  ) %>%
  spread(sector, n, fill = 0) %>% 
  janitor::clean_names()

saveRDS(df_app, "data/regpat_epo_app.rds")

# Inventors ---------------------------------------------------------------

# Load inventor data
df_inv <-
  read_delim("data/REGPAT/EPO/201803_EPO_Inv_reg.txt", delim = "|") %>% 
  janitor::clean_names()

df_inv <- df_inv %>% select(
  appln_id, person_id,               # Application and person IDs
  reg = reg_code, ctry = ctry_code,  # Regionalization
  reg_share, inv_share               # For fractional counting
  )

# Merge patent information

# df_inv <- df_inv %>% left_join(ipc, by = "appln_id")
# df_inv <- df_inv %>% left_join(radical, by = "appln_id")

# FIlter year and EU 
df_inv <- df_inv %>% filter(year >= 2000 & year <= 2014) %>% 
  mutate(eu28 = countrycode(ctry, origin = "iso2c", destination= "eu28")) %>% 
  group_by(appln_id) %>% filter(any(eu28 %in% "EU")) %>% ungroup()

# Merge with Applicant information

df_inv <- df_inv %>% left_join(df_app, by = "appln_id")

saveRDS(df_inv, file = "data/regpat_epo_inv.rds")

# Regions and distances -----------------------------------------------------------------
## First load the NUTS3 and world countries to compute region/country centroids

eu <- readOGR(paste0("data/nuts/data/", "NUTS_RG_03M_2013.shp"))
centroids <- coordinates(eu)

cent <- data.frame(coords=centroids, eu@data) %>% as.tibble() %>% 
  select(lat = coords.1, lon = coords.2, reg = NUTS_ID, everything())

world <- readOGR(paste0("data/country centroids/", "country_centroids_az8.shp"))
world <- world@data %>% select(ctry = iso_a2, ctry_name = name, lat = Latitude, lon = Longitude)
cent <- data.frame(coords=centroids, eu@data) %>% as.tibble() %>% 
  select(lat = coords.1, lon = coords.2, reg = NUTS_ID, everything())

saveRDS(cent, file = "../data/centroids_EU.rds")
saveRDS(world, file = "../data/centroids_world.rds")

# ipc concordance ---------------------------------------------------------

conc <- readxl::read_xls("data/ipc_technology.xls", skip = 6)
conc <- conc %>% select(tech_field = Field_number, tech_field_name = Field_en, tech_sector = Sector_en) %>% 
  distinct()

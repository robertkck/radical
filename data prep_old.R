# Data prep

library(tidyverse)

path <- "C:/Users/KalcikR/OneDrive/Arbeit/AIT/1803_Radical Inno/data/"
path <- "C:/Onedrive/Arbeit/AIT/1803_Radical Inno/data/"
region <- read_delim(paste0(path, "REGPAT/201803_EPO_region_full_v2.txt"), delim = ";")
ipc <- read_delim(paste0(path, "REGPAT/201803_EPO_year_full_v2.txt"), n_max = 100, guess_max = 100, delim = ";")
# region_name <- read_delim(paste0(path, "REGPAT/REGPAT_REGIONS.txt"), n_max = 100, guess_max = 100, delim = "|")


# Inventors ---------------------------------------------------------------


inv <- read_delim(paste0(path, "REGPAT/EPO/", "201803_EPO_Inv_reg.txt"), delim = "|")
inv <- inv %>% 
  select(appln_id = Appln_id, person_id = Person_id, reg = Reg_code, ctry = Ctry_code, reg_share = Reg_share, inv_share = Inv_share) 

ipc <- read_delim(paste0(path, "REGPAT/EPO/", "201803_EPO_IPC.txt"), delim = "|")
ipc <- ipc %>% select(appln_id = Appln_id, year = Prio_Year, year_app = App_year, ipc = IPC)
ipc <- ipc %>% group_by(appln_id, year_app, year) %>% nest() 

radical <- read_delim(paste0(path, "quality/", "201803_OECD_PATENT_QUALITY_EPO.txt"), delim = "|")
# radical <- radical %>% select(appln_id, tech_field, many_field, breakthrough, radicalness)

df <- inv %>% left_join(ipc, by = "appln_id")
# df <- df %>% left_join(ppat, by = "person_id")
# df <- df %>% left_join(app, by = "appln_id")
df <- df %>% left_join(radical, by = "appln_id")

df <- df %>% filter(year >= 2000 & year <= 2014)
saveRDS(df, file = "C:/Users/KalcikR/OneDrive/Arbeit/AIT/1803_Radical Inno/radical/data/regpat_epo_inv.rds")


# Applicants --------------------------------------------------------------

app <- read_delim(paste0(path, "REGPAT/EPO/", "201803_EPO_App_reg.txt"), delim = "|")
app <- app %>% select(appln_id = Appln_id, person_id = Person_id, reg = Reg_code, ctry = Ctry_code, reg_share = Reg_share, app_share = App_share)

ppat <- read_csv(paste0(path, "EEE_PPAT/", "EEE-PPAT_2017b.csv"),
                 col_types = cols_only(
                   person_id = col_integer(),
                   sector =col_character(),
                   hrm_l2_id = col_integer()
                 )
)

df <- app %>% left_join(ppat, by = "person_id")
df_app <- df %>% select(appln_id, sector) %>% group_by(appln_id) %>% mutate(n = n()) %>% distinct() %>% 
  group_by(appln_id) %>% arrange(sector) %>% summarise(s = paste0(sector, collapse = ", "), n_actor = first(n))

df_app <- df_app %>% group_by(s) %>% 
  mutate(
    freq = n(),
    sector_cat = case_when(
      freq >= 1000 ~ s,
      TRUE ~ "Other combination"
    )
  ) %>% ungroup()

saveRDS(df_app, "data/regpat_epo_app.rds")


df <- df %>% left_join(ipc, by = "appln_id")
df <- df %>% filter(year >= 2000)


saveRDS(df, file = "C:/Users/KalcikR/OneDrive/Arbeit/AIT/1803_Radical Inno/radical/data/regpat_epo_inv.rds")


# Regions -----------------------------------------------------------------

library(rgdal)
library(sp)
eu <- readOGR(paste0(path, "nuts/data/", "NUTS_RG_03M_2013.shp"))
centroids <- coordinates(eu)

cent <- data.frame(coords=centroids, eu@data) %>% as.tibble() %>% 
  select(lat = coords.1, lon = coords.2, reg = NUTS_ID, everything())


world <- readOGR(paste0(path, "country centroids/", "country_centroids_az8.shp"))
world <- world@data %>% select(ctry = iso_a2, ctry_name = name, lat = Latitude, lon = Longitude)
cent <- data.frame(coords=centroids, eu@data) %>% as.tibble() %>% 
  select(lat = coords.1, lon = coords.2, reg = NUTS_ID, everything())


# ipc concordance ---------------------------------------------------------

conc <- readxl::read_xls(paste0(path, "ipc_technology.xls"), skip = 6)
conc <- conc %>% select(tech_field = Field_number, tech_field_name = Field_en, tech_sector = Sector_en) %>% 
  distinct()

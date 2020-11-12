# Load Packages ----------------------------------------------------------------
Packages <- c("stringr", 
              "maps",
              "reshape2",
              "ggplot2",
              "gridExtra",
              "stringr", 
              "lubridate", 
              "tigris", 
              "SpecsVerification",
              "corrplot",
              "ggExtra",
              "miniUI",
              "gtools",
              "hydroTSM",
              "dplyr",
              "readxl",
              "sf",
              "tmap",
              "tmaptools",
              "tidyr",
              "tidyverse",
              "purrr",
              "ggpubr",
              "usmap",
              "rstudioapi",
              "wesanderson"
              
)
suppressPackageStartupMessages(all(unlist(lapply(Packages, require, character.only = TRUE))))


#### Set Working Directory ####
current.dir <- dirname(getActiveDocumentContext()$path)
setwd(current.dir) 


# Assign file paths ----------------------------------------------------------------
dat.dir <- file.path("../sample_data")
out.dir <- file.path("../proc_outputs")

# Combined county level pop-weighted BlueSky data and AQS monitor data
NAM_2015_Comp <- read.csv(file = file.path(dat.dir, "Comparison_Dataset_NAM_AQS_2015.csv"), 
                          colClasses = c("character", rep("numeric", 4), "character", rep("numeric", 4)), 
                          col.names = c("Date","Year", "CTFIPS","FIPS5", 
                                        "AQS_Site_ID", "Location", "Grid _ID", 
                                        "PM_AQS_Mean", "PM_NAM_Mean","PM_NAM_max")) %>% 
  mutate(YMD = as.Date(Date, format =  "%d%b%Y"),
         Month = month(as.Date(Date, format =  "%d%b%Y"), label = T, abbr = F),
         Season = time2season(as.Date(Date, format =  "%d%b%Y"), out.fmt = "seasons", type="default"),
         wf_szn = if_else(month(as.Date(Date, format =  "%d%b%Y")) %in% c(3,4,5,6), "Early",
                          if_else(month(as.Date(Date, format =  "%d%b%Y")) %in% c(7,8,9,10), "Peak",
                                  "Off")))

# NCHS Urban-Rural Classification Codes
UrbRurCodes <- read_xlsx(path = file.path(dat.dir, "NCHSURCodes2013.xlsx"), 
                         col_types = c("numeric", rep("text", 2), rep("skip", 3), "numeric", rep("skip", 2)), 
                         col_names = c("FIPS5","ST_Abbr", "CTY_NAME", "UR_CODE"), skip = 1)

# Read in population data
cnty_pop_14_18 <- read.csv(file= file.path(dat.dir, "County_pop_yr_1418.csv"),
                           colClasses = rep("numeric", 3), 
                           col.names = c("FIPS5", "Year", "POP"))

# Merged NAM <-> AQS <-> NCHS Classification codes
NAM_2015_Comp_Codes <- NAM_2015_Comp %>% left_join(UrbRurCodes, by="FIPS5") %>%
  left_join(cnty_pop_14_18, by=c("FIPS5", "Year")) %>%
  mutate(Metro_Class = cut(UR_CODE, breaks = c(0, 3, 4, 6), 
                           labels = c("Small", "Meduim", "Large")),
         THold_mean = cut(PM_NAM_Mean, breaks = c(1, 5, 10, 20, Inf), 
                          labels = c("1-5", "5-10", "10-20", ">20")),
         THold_max = cut(PM_NAM_max, breaks = c(1, 5, 10, 20, Inf), 
                         labels = c("1-5", "5-10", "10-20", ">20")))
rm(NAM_2015_Comp)


# County-level smoke data
# Read in and process US county map data ----------------------------------------------------------------
US_Cty <- st_read(dsn=file.path(dat.dir, "us_county_bdry", "gz_2010_us_050_00_20m.shp"))
US_St <- st_read(dsn=file.path(dat.dir, "us_state_bdry", "cb_2018_us_state_20m.shp"))
NCDC_bdry  <-  st_read(dsn=file.path(dat.dir, "ncdc_regions", "county_NCDC_Dissolve.shp")) %>%
  st_make_valid() %>% st_transform("+init=epsg:4269") %>% dplyr::rename("Clim_Reg" = "NCDC_regio")

# leave out AK, HI, and PR (state FIPS: 02, 15, and 72)
US_CON_Cty <- US_Cty[!(US_Cty$STATE %in% c("02","15","72")),]
US_CON_St <- US_St[!(US_St$STATEFP %in% c("02","15","72")),] 
# Create FIPS category
US_CON_Cty$FIPS5 <- as.numeric(paste0(US_CON_Cty$STATE, US_CON_Cty$COUNTY) %>% str_remove("^0+"))
# US_WF_Cty$FIPS5 <- as.numeric(paste0(US_WF_Cty$STATE, US_WF_Cty$COUNTY) %>% str_remove("^0+"))

# Create map of county boundaries for overlay with NCDC region areas areas
centroids  <- sf::st_centroid(US_CON_Cty)
inters <- sf::st_intersection(NCDC_bdry, centroids) %>% sf::st_set_geometry(NULL)
NCDC_Cnty <- dplyr::left_join(US_CON_Cty, inters) %>% sf::st_sf(sf_column_name = "geometry")
rm("inters", "centroids")

########### Calculating and mapping monitor statistics ------

# Determining number of counties in each climate region
NCDC_Cnty %>%
  group_by(Clim_Reg) %>% dplyr::summarise(length(unique(FIPS5)))

# Calculating number of monitors per climate region
NCDC_Cnty %>% 
  left_join(NAM_2015_Comp_Codes) %>%
  filter(!is.na(AQS_Site_ID)) %>% 
  group_by(Clim_Reg) %>% 
  dplyr::summarise(length(unique(AQS_Site_ID)))

# Number of counties with monitors in each climate region
NCDC_Cnty %>% 
  left_join(NAM_2015_Comp_Codes) %>%
  filter(!is.na(AQS_Site_ID)) %>%
  group_by(Clim_Reg) %>%
  dplyr::summarise(length(unique(FIPS5)))

# Population fraction of each climate region
NCDC_Cnty %>% 
  left_join(NAM_2015_Comp_Codes) %>%
  group_by(Clim_Reg) %>% 
  dplyr::summarise(Clim_POP = sum(POP, na.rm = T)) %>% 
  mutate(POP_frac = Clim_POP/sum(Clim_POP)*100)


# fraction of counties in each metro class aggregated by NCDC climate regions
NCDC_Cnty %>% left_join(UrbRurCodes) %>%  
  mutate(Metro_Class = cut(UR_CODE, breaks = c(0, 3, 4, 6), 
                           labels = c("Small", "Meduim", "Large"))) %>%
  group_by(Clim_Reg, Metro_Class) %>% dplyr::summarise(con_class = length(unique(FIPS5))) %>%  
  group_by(Clim_Reg) %>%
  mutate(con_class_frac = con_class/sum(con_class)*100) %>%
  st_set_geometry(NULL) %>% print(n=40)

# fraction of counties in each metro class aggregated by High impact states climate regions
plot_usmap("counties", include = c("AZ", "CA", "CO", "ID", "MT", "NM", "NV", "OR", "UT", "WA", "WY"))
NCDC_Cnty %>% left_join(UrbRurCodes) %>% 
  filter(ST_Abbr %in% c("WA", "OR", "CA", "ID", "NV", "UT", "AZ", "MT", "WY", "CO", "NM")) %>%
  mutate(Metro_Class = cut(UR_CODE, breaks = c(0, 3, 4, 6), 
                           labels = c("Small", "Meduim", "Large"))) %>%
  group_by(Metro_Class) %>%
  count(n = n()) %>% 
  spread(value, n, fill = 0)
  dplyr::summarise(con_class = tally(Metro_Class)) %>%  
  # group_by(Clim_Reg) %>%
  # mutate(con_class_frac = con_class/sum(con_class)*100) %>%
  st_set_geometry(NULL) %>% print(n=40)


# Map number of AQS monitors per county
tmap_mode("plot")
monitor_count <- NAM_2015_Comp_Codes %>% group_by(FIPS5) %>% 
  summarise(MCount = as.numeric(length(unique(AQS_Site_ID)))) %>%
  left_join(NCDC_Cnty) %>%
  st_sf() %>%
  tm_shape(projection="+init=epsg:2163") +
  tm_polygons("MCount", legend.is.portrait = T,
              breaks = c(1,2,4,6,8,10,Inf),
              style = "fixed",
              # palette = "seq",
              border.col = "grey70", lwd=0.1,
              title = "# of Monitors in County") +
  tm_layout(aes.palette = list(seq = "BuPu")) +
  tm_shape(US_CON_St) +
  tm_borders(lwd=0.04, col = "black", alpha = .4) +
  tm_shape(US_CON_Cty) +
  tm_borders(lwd=0.08, col = "black", alpha = .1) +
  tm_shape(NCDC_bdry) +
  tm_fill("Clim_Reg",
          alpha = 0.0, 
          legend.show =F,
          title = "US CLimate Regions") +
  tm_borders(lwd=0.5, col = "black", alpha = .7) +
  tm_text("Clim_Reg", size=0.32, col = "black",
          bg.color="white", bg.alpha = .5,
          legend.size.show = FALSE) +
  tm_layout(main.title= "Number of AQS Monitors per County, 2015 ",
            main.title.position = 0.3,
            main.title.size = 0.5,
            legend.title.size = 0.6,
            legend.text.size=0.5,
            frame = FALSE)
tmap_save(monitor_count, filename=file.path(out.dir, "AQS_monitor_county.png"), width=1080, height=960, asp=0)


# Mapping metro classes of every  county
tmap_mode("plot")
metro_class <- NAM_2015_Comp_Codes %>% 
  group_by(FIPS5) %>%
  summarise(UR_CODE = unique(UR_CODE)) %>%
  left_join(NCDC_Cnty) %>% 
  st_sf() %>%
  tm_shape(projection="+init=epsg:2163") +
  tm_polygons("UR_CODE", legend.is.portrait = T,
              style = "fixed",
              breaks = c(0, 3, 4, 6), 
              labels = c("Large Central or Fringe Metropolitan", 
                         "Medium or Small Metropolitan",
                         "Non-Metropolitan"),
              # palette = "cat",
              border.col = "grey80", lwd=0.1,
              title = "County Metro Class") +
  tm_layout(aes.palette = list(seq = "RdYlGn")) +
  tm_shape(US_CON_St) +
  tm_borders(lwd=0.04, col = "black", alpha = .4) +
  tm_shape(US_CON_Cty) +
  tm_borders(lwd=0.08, col = "black", alpha = .1) +
  tm_shape(NCDC_bdry) +
  tm_fill("Clim_Reg",
          alpha = 0.0, 
          legend.show =F,
          title = "US CLimate Regions") +
  tm_borders(lwd=0.5, col = "black", alpha = .7) +
  tm_text("Clim_Reg", size=0.35, col = "black",
          bg.color="white", bg.alpha = .5,
          legend.size.show = FALSE) +
  tm_layout(main.title= "NCHS County Urban-Rural Classification, 2015", 
            main.title.position = 0.25,
            main.title.size = 0.5,
            legend.title.size = 0.6,
            legend.text.size=0.4,
            legend.width = 1.0,
            frame = FALSE)
tmap_save(metro_class, filename=file.path(out.dir, "county_metro_class.png"), width=1080, height=960, asp=0)



# Calculating amd plotting the total population residing in counties with no monitoring
tmap_mode("plot")
no_monit_cnty <- cnty_pop_14_18 %>% filter(Year==2015) %>%
  left_join(NAM_2015_Comp_Codes) %>%
  filter(is.na(AQS_Site_ID)) %>% select("FIPS5", "Year", "POP") %>%
  left_join(NCDC_Cnty) %>% st_sf() %>%
  tm_shape(projection="+init=epsg:2163") +
  tm_polygons("POP", legend.is.portrait = T,
              style = "log10_pretty",
              # palette = "cat",
              border.col = "grey80", lwd=0.1,
              title = "County Population") +
  tm_layout(aes.palette = list(seq = "BuPu")) +
  tm_shape(US_CON_St) +
  tm_borders(lwd=0.1, col = "black", alpha = .8) +
  tm_shape(US_CON_Cty) +
  tm_borders(lwd=0.07, col = "black", alpha = .2) +
  tm_shape(NCDC_bdry) +
  tm_fill("Clim_Reg",
          alpha = 0.0, 
          legend.show =F,
          title = "US CLimate Regions") +
  tm_borders(lwd=0.5, col = "black", alpha = .7) +
  tm_text("Clim_Reg", size=0.35, col = "black",
          bg.color="white", bg.alpha = .5,
          legend.size.show = FALSE)  +
  tm_layout(main.title= "Population density of counties without AQS monitors, 2015", 
            main.title.position = 0.25,
            main.title.size = 0.5,
            legend.title.size = 0.6,
            legend.text.size=0.4,
            legend.width = 1.0,
            frame = FALSE)
tmap_save(no_monit_cnty, filename=file.path(out.dir, "pop_non_monit.png"), width=1080, height=960, asp=0)



# Calculating and plotting the percentage of active monitoring days per county for 2015
mon_rep_frac <- NAM_2015_Comp_Codes %>% 
  group_by(FIPS5, AQS_Site_ID) %>%
  summarise(mon_frac = (365 - sum(is.na(PM_AQS_Mean)))/365) %>%
  group_by(FIPS5) %>%
  summarise(avr_frac = sum(mon_frac)/as.numeric(length(unique(AQS_Site_ID)))*100, max_frac = max(mon_frac)*100) %>%
  left_join(NCDC_Cnty) %>% 
  st_sf() %>%
  tm_shape(projection="+init=epsg:2163") +
  tm_polygons("avr_frac", legend.is.portrait = T,
              # palette = "cat",
              border.col = "grey80", lwd=0.1,
              title = "% active monitor days, 2015") +
  tm_layout(aes.palette = list(seq = "BuPu")) +
  tm_shape(US_CON_St) +
  tm_borders(lwd=0.1, col = "black", alpha = .8) +
  tm_shape(US_CON_Cty) +
  tm_borders(lwd=0.07, col = "black", alpha = .2) +
  # tm_shape(ClimReg_bdry) +
  # tm_fill("Clim_Reg",
  #         alpha = 0.0, 
  #         legend.show =F,
  #         title = "US CLimate Regions") +
  # tm_borders(lwd=0.5, col = "black", alpha = .7) +
  # tm_text("Clim_Reg", size=0.35, col = "black",
  #         bg.color="white", bg.alpha = .5,
  #         legend.size.show = FALSE)  +
  tm_layout(main.title= "County Average Fraction of Days with Active PM2.5 Monitoring, 2015", 
            main.title.position = 0.15,
            main.title.size = 0.5,
            legend.title.size = 0.6,
            legend.text.size=0.4,
            legend.width = 1.0,
            frame = FALSE)
tmap_save(mon_rep_frac, filename=file.path(out.dir, "county_monit_days.png"), width=1080, height=960, asp=0)


# mapping 2015 smoke day and person days of exposure ----------------

# calculating total smoke days and person days of exposure grouped by wildfire seasons across all counties
wfszn_cnty_smk <- NAM_2015_Comp_Codes %>% 
  mutate(AQS_smk_dy = ifelse(PM_AQS_Mean >= 20, 1, 0),
         AQS_per_dy = POP * ifelse(PM_AQS_Mean >= 20, 1, 0),
         NAM_smk_dy = ifelse(PM_NAM_Mean >= 20, 1, 0),
         NAM_per_dy = POP * ifelse(PM_NAM_Mean >= 20, 1, 0)) %>%
  group_by(FIPS5, wf_szn) %>% 
  summarise(AQS_smk_dy_ct = sum(AQS_smk_dy, na.rm = T),
            AQS_sum_per_dys = sum(AQS_per_dy, na.rm = T),
            NAM_smk_dy_ct = sum(NAM_smk_dy, na.rm = T),
            NAM_sum_per_dys = sum(NAM_per_dy, na.rm = T)) %>%
  group_by(wf_szn) %>%
  summarise(AQS_smk_dy_wf_szn = sum(AQS_smk_dy_ct, na.rm = T),
            AQS_per_dys_wf_szn = sum(AQS_sum_per_dys, na.rm = T),
            NAM_smk_dy_wf_szn = sum(NAM_smk_dy_ct, na.rm = T),
            NAM_per_dys_wf_szn = sum(NAM_sum_per_dys, na.rm = T)) %>%
  select(wf_szn, AQS_smk_dy_wf_szn, NAM_smk_dy_wf_szn) %>%
  melt() %>%
  ggplot(., aes(x = variable, y=value, fill = wf_szn)) +
  geom_bar(stat ="identity", position = position_dodge(), alpha=0.75) +
  theme_bw(base_size = 15) +
  # adjust the x axis breaks
  # scale_x_date(date_breaks = "1 years", date_labels = "%Y") +
  theme(axis.title.x = element_text(colour = "black",  size = 25, margin = margin(20,0,0,0)),
        axis.title.y = element_text(colour = "black",  size = 25, margin = margin(0,10,0,0)),
        axis.text.x  = element_text(colour = "black",  size = 25),
        axis.text.y  = element_text(colour = "black",  size = 25),
        panel.border = element_rect(colour = "grey75", size = 0.85),
        axis.line    = element_line(colour = "grey75", size = 0.85)) 


# calculating total smoke days and person days of exposure grouped by calendar seasons across all counties
szn_cnty_smk <- NAM_2015_Comp_Codes %>% 
  mutate(AQS_smk_dy = ifelse(PM_AQS_Mean >= 20, 1, 0), 
         AQS_per_dy = POP * ifelse(PM_AQS_Mean >= 20, 1, 0),
         NAM_smk_dy = ifelse(PM_NAM_Mean >= 20, 1, 0), 
         NAM_per_dy = POP * ifelse(PM_NAM_Mean >= 20, 1, 0)) %>%
  group_by(FIPS5, Season) %>% 
  dplyr::summarise(AQS_smk_dy_ct = sum(AQS_smk_dy, na.rm = T), 
                   AQS_sum_per_dys = sum(AQS_per_dy, na.rm = T),
                   NAM_smk_dy_ct = sum(NAM_smk_dy, na.rm = T), 
                   NAM_sum_per_dys = sum(NAM_per_dy, na.rm = T)) %>%
  group_by(Season) %>%
  dplyr::summarise(AQS_smk_dy_szn = sum(AQS_smk_dy_ct, na.rm = T), 
                   AQS_per_dys_szn = sum(AQS_sum_per_dys, na.rm = T),
                   NAM_smk_dy_szn = sum(NAM_smk_dy_ct, na.rm = T), 
                   NAM_per_dys_szn = sum(NAM_sum_per_dys, na.rm = T))


# calculating total smoke days and person days of exposure grouped by climate region
clim_cnty_smk <- NAM_2015_Comp_Codes %>% left_join(NCDC_Cnty) %>%
  mutate(NAM_smk_dy = ifelse(PM_NAM_Mean >= 20, 1, 0), 
         NAM_per_dy = POP * ifelse(PM_NAM_Mean >= 20, 1, 0)) %>%
  group_by(Clim_Reg) %>% 
  dplyr::summarise(NAM_smk_dy_ct = sum(NAM_smk_dy, na.rm = T), 
                   NAM_sum_per_dys = sum(NAM_per_dy, na.rm = T)) %>%
  group_by(Clim_Reg) %>%
  dplyr::summarise(NAM_smk_dys = sum(NAM_smk_dy_ct, na.rm = T),
                   NAM_per_dys = sum(NAM_sum_per_dys, na.rm = T)) %>%
  dplyr::mutate("Smoke Days" = NAM_smk_dys/sum(NAM_smk_dys, na.rm = T)* 100,
                "Person-Days" = NAM_per_dys/sum(NAM_per_dys, na.rm = T)*100) %>%
  dplyr::select(1,4:5) %>%
  melt() %>%
  ggplot(., aes(x = variable, y=value, fill = Clim_Reg)) +
  geom_bar(stat ="identity", position = position_dodge(), width=0.8) +
  theme_minimal(base_size = 11,) +
  labs(fill = "Climate Regions") +
  scale_fill_manual(values = wes_palette("FantasticFox1", 9, type = "continuous")) +
  ylab("Percent of county level smoke days and person-days \n (%)") +
  xlab("")+
  theme(legend.title = element_text(colour="black", size=15, face="bold"),
        legend.text = element_text(color="black", size = 14),
        axis.title.x = element_text(colour = "black",  size = 15, margin = margin(20,0,0,0)),
        axis.title.y = element_text(colour = "black",  size = 15, margin = margin(0,10,0,0)),
        axis.text.x  = element_text(colour = "black",  size = 15),
        axis.text.y  = element_text(colour = "black",  size = 15)) 
ggsave(filename="NAM_clim_smk_per_bar.png", plot=clim_cnty_smk, device="png", 
       width = 10, height = 8, dpi = 300, units = "in",
       path = file.path(out.dir))



# calculating total smoke days and person days of exposure grouped by metropolitan classes
met_szn_cnty_smk <- NAM_2015_Comp_Codes %>% 
  filter(Year==2015) %>%
  mutate(AQS_smk_dy = ifelse(PM_AQS_Mean >= 20, 1, 0), 
         AQS_per_dy = POP * ifelse(PM_AQS_Mean >= 20, 1, 0),
         NAM_smk_dy = ifelse(PM_NAM_Mean >= 20, 1, 0), 
         NAM_per_dy = POP * ifelse(PM_NAM_Mean >= 20, 1, 0)) %>%
  group_by(FIPS5, Metro_Class) %>% 
  summarise(AQS_smk_dy_ct = sum(AQS_smk_dy, na.rm = T), 
            AQS_sum_per_dys = sum(AQS_per_dy, na.rm = T),
            NAM_smk_dy_ct = sum(NAM_smk_dy, na.rm = T), 
            NAM_sum_per_dys = sum(NAM_per_dy, na.rm = T)) %>%
  group_by(Metro_Class) %>%
  summarise(AQS_smk_dy_szn = sum(AQS_smk_dy_ct, na.rm = T), 
            AQS_per_dys_szn = sum(AQS_sum_per_dys, na.rm = T),
            NAM_smk_dy_szn = sum(NAM_smk_dy_ct, na.rm = T), 
            NAM_per_dys_szn = sum(NAM_sum_per_dys, na.rm = T))



# mapping yearly average smoke days  across all counties
AQS_ave_cnty_smk_dys <- NAM_2015_Comp_Codes %>% 
  mutate(smk_dy = ifelse(PM_AQS_Mean >= 20, 1, 0), per_dy = POP * ifelse(PM_AQS_Mean >= 20, 1, 0)) %>%
  group_by(FIPS5) %>% 
  summarise(smk_dy_ct = sum(smk_dy, na.rm = T), sum_per_dys = sum(per_dy, na.rm = T)) %>%
  left_join(US_CON_Cty, by = "FIPS5") %>% st_sf() %>% mutate(max=max(smk_dy_ct)) %>%
  ggplot()  +
  geom_sf(aes(fill = smk_dy_ct), size =0.1, color="grey55") +
  geom_sf(data = US_CON_Cty, aes(color=STATES), color="grey90",  alpha = 0.1, fill=NA, size=0.1) +
  geom_sf(data = US_CON_St, aes(color=STATES), color="grey65",  alpha = 0.1, fill=NA, size=0.1) +
  # geom_sf(data = GACC, aes(color=GACCAbbrev), color="grey10",  alpha = 0.7, fill=NA, size=0.3) +
  # geom_sf_text(data = GACC, aes(label = GACCAbbrev), colour = "black") +
  coord_sf(crs ="+init=epsg:2163") +
  scale_fill_gradientn(name = "Smoke Days", na.value="grey50",
                       limits = c(0,  207),
                       space = "Lab",
                       colours = c("#FFFFFC", get_brewer_pal("YlOrRd", n = 45, contrast = c(0.2, 1.0)))) +
  theme_void() +
  labs(title = "2015", subtitle = (expression(paste("AQS Yearly average county-level number of smoke days", ~"[Daily Mean ",PM[2.5], ~"> 20",mu*g*m^{-3},"]")))) +
  guides(fill = guide_colourbar(barwidth = 1.5, barheight = 8, draw.ulim = FALSE, draw.llim = FALSE, frame.colour = "Gray80")) +
  theme(panel.background = element_rect(fill="white",colour ="white",size=0.5,linetype="solid"),
        panel.grid.major = element_line(size=0.5,linetype='solid',colour="white"), 
        panel.grid.minor = element_line(size=0.25,linetype='solid',colour="white"),
        plot.title = element_text(hjust = 0.5, size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.text = element_text(margin = margin(t = 10)),
        legend.justification = c(1.0, 0.5), 
        legend.position = c(1.03, 0.26),
        legend.box.margin=margin(c(50,50,50,50)))
ggsave(plot=AQS_ave_cnty_smk_dys, filename = file.path(out.dir, "BlueSky_eval_pop", "cnty_smk_dys", "AQS_yr_ave_smk_dys.png"),
       width = 10, height = 8, dpi = 300, units = "in", device='png')


#  mapping yearly average person days of exposure  across all counties
AQS_ave_cnty_per_dys <- NAM_2015_Comp_Codes %>% 
  mutate(smk_dy = ifelse(PM_AQS_Mean >= 20, 1, 0), per_dy = POP * ifelse(PM_AQS_Mean >= 20, 1, 0)) %>%
  group_by(FIPS5) %>% 
  summarise(smk_dy_ct = sum(smk_dy, na.rm = T), sum_per_dys = sum(per_dy, na.rm = T)) %>%
  left_join(US_CON_Cty, by = "FIPS5") %>% st_sf() %>% mutate(max=max(sum_per_dys)) %>%
  ggplot()  +
  geom_sf(aes(fill = sum_per_dys), size =0.1, color="grey55") +
  geom_sf(data = US_CON_Cty, aes(color=STATES), color="grey90",  alpha = 0.1, fill=NA, size=0.1) +
  geom_sf(data = US_CON_St, aes(color=STATES), color="grey65",  alpha = 0.1, fill=NA, size=0.1) +
  # geom_sf(data = GACC, aes(color=GACCAbbrev), color="grey10",  alpha = 0.7, fill=NA, size=0.3) +
  # geom_sf_text(data = GACC, aes(label = GACCAbbrev), colour = "black") +
  coord_sf(crs ="+init=epsg:2163") +
  scale_fill_gradientn(name = "Person Days", na.value="grey50",
                       limits = c(0,  1373197032),
                       space = "Lab",
                       colours = c("#FFFFFC", get_brewer_pal("YlOrRd", n = 1e3, contrast = c(0.2, 1.0)))) +
  theme_void() +
  labs(title = "2015", subtitle = (expression(paste("AQS Yearly average county-level person days of exposure", ~"[Daily Mean ",PM[2.5], ~"> 20",mu*g*m^{-3},"]")))) +
  guides(fill = guide_colourbar(barwidth = 1.5, barheight = 8, draw.ulim = FALSE, draw.llim = FALSE, frame.colour = "Gray80")) +
  theme(panel.background = element_rect(fill="white",colour ="white",size=0.5,linetype="solid"),
        panel.grid.major = element_line(size=0.5,linetype='solid',colour="white"), 
        panel.grid.minor = element_line(size=0.25,linetype='solid',colour="white"),
        plot.title = element_text(hjust = 0.5, size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.text = element_text(margin = margin(t = 10)),
        legend.justification = c(1.0, 0.5), 
        legend.position = c(1.03, 0.26),
        legend.box.margin=margin(c(50,50,50,50)))
ggsave(plot=AQS_ave_cnty_per_dys, filename = file.path(out.dir, "BlueSky_eval_pop", "cnty_pers_dys", "AQS_yr_ave_per_dys.png"),
       width = 10, height = 8, dpi = 300, units = "in", device='png')


# Correlation analysis of 2015 smoke data ~ Metro areas ----------------------------------

# Correlation: NAM vs AQS 
corr_NAM_AQS <-  NAM_2015_Comp_Codes %>% 
  group_by(FIPS5, AQS_Site_ID) %>%
  summarise(mon_frac = (365 - sum(is.na(PM_AQS_Mean)))/365) %>%
  group_by(FIPS5) %>%
  summarise(cty_avr_frac = sum(mon_frac)/as.numeric(length(unique(AQS_Site_ID)))*100, cty_max_frac = max(mon_frac)*100) %>%
  left_join(NAM_2015_Comp_Codes) %>% 
  # left_join(GACC_Cnty) %>%
  filter(cty_avr_frac >= 75) %>%
  filter(Month == "August") %>%
  drop_na(PM_AQS_Mean) %>% 
  ggplot(aes(x = PM_AQS_Mean, y = PM_NAM_Mean)) +
  geom_point(alpha = 0.5, size =  1.0) +
  geom_smooth(method="lm", se = T, color = "grey65") +
  scale_color_manual(values = c("#009E73", "#000000", "#FC4E07")) +
  scale_fill_manual(values = c("#009E73", "#000000", "#FC4E07")) +
  # facet_wrap( ~ wf_szn) +
  stat_cor(method = "spearman", 
           label.x.npc = 0, 
           label.y.npc = 0.98, hjust = 0,
           cor.coef.name = "R",  size = 4) +
  stat_cor(method = "kendall", 
           label.x.npc = 0, 
           label.y.npc = 0.92, hjust = 0,
           cor.coef.name = "tau",  size = 4) +
  labs(title = expression(paste(" County-level NAM and AQS-based", ~PM[2.5])), 
       subtitle = "August 1–August 31, 2015",
       y = expression(Mean~NAM~PM[2.5]~paste(mu, "g") %.% m^{-3}), 
       x = expression(Mean~AQS~PM[2.5]~paste(mu, "g") %.% m^{-3})) + 
  theme_bw(base_size = 11) +
  theme(panel.background = element_rect(fill="white",colour ="black",size=0.5,linetype="solid"),
        panel.grid.major = element_line(size=0.5,linetype='solid',colour="white"), 
        panel.grid.minor = element_line(size=0.25,linetype='solid',colour="white"),
        plot.title = element_text(hjust = 0.5, size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.justification = c(1.0, 0.7), 
        legend.position = c(1.0, 0.85),
        legend.box.margin=margin(c(50,50,50,50)))


# Correlation: NAM vs AQS  - Western States _August
corr_West <- NAM_2015_Comp_Codes %>% 
  group_by(FIPS5, AQS_Site_ID) %>%
  summarise(mon_frac = (365 - sum(is.na(PM_AQS_Mean)))/365) %>%
  group_by(FIPS5) %>%
  summarise(cty_avr_frac = sum(mon_frac)/as.numeric(length(unique(AQS_Site_ID)))*100, cty_max_frac = max(mon_frac)*100) %>%
  left_join(NAM_2015_Comp_Codes) %>% 
  # left_join(GACC_Cnty) %>%
  filter(cty_avr_frac >= 75) %>% 
  filter(ST_Abbr %in% c("WA", "OR", "CA", "ID", "NV", "UT", "AZ", "MT", "WY", "CO", "NM")) %>%
  filter(Month == "August") %>%
  drop_na(PM_AQS_Mean) %>% 
  ggplot(aes(x = PM_AQS_Mean, y = PM_NAM_Mean)) +
  geom_point(alpha = 0.5, size =  1.0) +
  geom_smooth(method="lm", se = T, color = "grey75") +
  # scale_color_manual(values = c("#009E73", "#000000", "#FC4E07")) +
  # scale_fill_manual(values = c("#009E73", "#000000", "#FC4E07")) +
  # facet_wrap( ~ wf_szn ) +
  stat_cor(method = "spearman", 
           label.x.npc = 0, 
           label.y.npc = 0.98, hjust = 0,
           cor.coef.name = "R",  size = 4) +
  stat_cor(method = "kendall", 
           label.x.npc = 0, 
           label.y.npc = 0.92, hjust = 0,
           cor.coef.name = "tau",  size = 4) +
  labs(title = expression(paste(" County-level NAM and AQS-based", ~PM[2.5])),
       subtitle = "Western States, August 1–August 31, 2015",
       y = expression(Mean~NAM~PM[2.5]~paste(mu, "g") %.% m^{-3}), 
       x = expression(Mean~AQS~PM[2.5]~paste(mu, "g") %.% m^{-3})) + 
  theme_bw(base_size = 11) +
  theme(panel.background = element_rect(fill="white",colour ="black",size=0.5,linetype="solid"),
        panel.grid.major = element_line(size=0.5,linetype='solid',colour="white"), 
        panel.grid.minor = element_line(size=0.25,linetype='solid',colour="white"),
        plot.title = element_text(hjust = 0.5, size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.justification = c(1.0, 0.7), 
        legend.position = c(1.0, 0.85),
        legend.box.margin=margin(c(50,50,50,50)))


# Correlation: NAM vs AQS by metro area
corr_metro <- NAM_2015_Comp_Codes %>% 
  group_by(FIPS5, AQS_Site_ID) %>%
  summarise(mon_frac = (365 - sum(is.na(PM_AQS_Mean)))/365) %>%
  group_by(FIPS5) %>%
  summarise(cty_avr_frac = sum(mon_frac)/as.numeric(length(unique(AQS_Site_ID)))*100, cty_max_frac = max(mon_frac)*100) %>%
  left_join(NAM_2015_Comp_Codes) %>% filter(cty_avr_frac >= 75) %>% 
  filter(ST_Abbr %in% c("WA", "OR", "CA", "ID", "NV", "UT", "AZ", "MT", "WY", "CO", "NM")) %>%
  filter(Month == "August") %>%
  drop_na(PM_AQS_Mean) %>% 
  ggplot(aes(x = PM_AQS_Mean, y = PM_NAM_Mean)) +
  geom_point(aes(shape = Metro_Class), alpha = 0.7, size =  1.0) +
  geom_smooth(method="lm", se = T, color = "grey75") +
  # scale_color_manual(values = c("#009E73", "#000000", "#FC4E07")) +
  # scale_fill_manual(values = c("#009E73", "#000000", "#FC4E07")) +
  # facet_wrap( ~ wf_szn ) +
  stat_cor(method = "spearman", 
           label.x = 0,
           cor.coef.name = "R",
           aes(shape = Metro_Class), size = 4) +
  labs(
    # title = expression(paste(" County-level NAM and AQS-based", ~PM[2.5])),
    subtitle = "Western States, August 1–August 31, 2015\n Observations disaggregated by metropolitan classifications",
    y = expression(Mean~NAM~PM[2.5]~paste(mu, "g") %.% m^{-3}), 
    x = expression(Mean~AQS~PM[2.5]~paste(mu, "g") %.% m^{-3})) + 
  theme_bw(base_size = 11) +
  theme(panel.background = element_rect(fill="white",colour ="black",size=0.5,linetype="solid"),
        panel.grid.major = element_line(size=0.5,linetype='solid',colour="white"), 
        panel.grid.minor = element_line(size=0.25,linetype='solid',colour="white"),
        plot.title = element_text(hjust = 0.5, size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title=element_text(size=10), 
        legend.text=element_text(size=10),
        legend.justification = c(-0.3, 0.3), 
        legend.position = c(-0.3, 0.3),
        legend.box.margin=margin(c(50,50,50,50)))



# Correlation: NAM vs AQS by concentration thresholds
corr_THold <- NAM_2015_Comp_Codes %>% 
  group_by(FIPS5, AQS_Site_ID) %>%
  summarise(mon_frac = (365 - sum(is.na(PM_AQS_Mean)))/365) %>%
  group_by(FIPS5) %>%
  summarise(cty_avr_frac = sum(mon_frac)/as.numeric(length(unique(AQS_Site_ID)))*100, cty_max_frac = max(mon_frac)*100) %>%
  left_join(NAM_2015_Comp_Codes) %>% 
  filter(cty_avr_frac >= 75) %>% 
  filter(ST_Abbr %in% c("WA", "OR", "CA", "ID", "NV", "UT", "AZ", "MT", "WY", "CO", "NM")) %>%
  filter(Month == "August") %>%
  drop_na(PM_AQS_Mean) %>% 
  ggplot(aes(x = PM_AQS_Mean, y = PM_NAM_Mean)) +
  geom_point(aes(shape = THold_mean), alpha = 0.7, size =  1.0) +
  geom_smooth(method="lm", se = T, color = "grey75") +
  # scale_color_manual(values = c("#009E73", "#000000", "#FC4E07")) +
  # scale_fill_manual(values = c("#009E73", "#000000", "#FC4E07")) +
  # facet_wrap( ~ wf_szn ) +
  stat_cor(method = "spearman", 
           label.x = 0,
           cor.coef.name = "R",
           aes(shape = THold_mean), size = 4) +
  labs(
    # title = expression(paste(" County-level NAM and AQS-based", ~PM[2.5])),
    subtitle = "Western States, August 1–August 31, 2015\n Observations disaggregated by concentration thresholds",
    y = expression(Mean~NAM~PM[2.5]~paste(mu, "g") %.% m^{-3}), 
    x = expression(Mean~AQS~PM[2.5]~paste(mu, "g") %.% m^{-3})) + 
  theme_bw(base_size = 11) +
  theme(panel.background = element_rect(fill="white",colour ="black",size=0.5,linetype="solid"),
        panel.grid.major = element_line(size=0.5,linetype='solid',colour="white"), 
        panel.grid.minor = element_line(size=0.25,linetype='solid',colour="white"),
        plot.title = element_text(hjust = 0.5, size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title=element_text(size=10), 
        legend.text=element_text(size=10),
        legend.justification = c(-0.3, 0.3),
        legend.position = c(-0.3, 0.3),
        legend.box.margin=margin(c(50,50,50,50)))


corr_grid  <- grid.arrange(corr_NAM_AQS, corr_West, corr_metro, corr_THold, nrow = 2)
ggsave(filename="corr_AQS_NAM_grid.png", plot=corr_grid,
       path = file.path(out.dir, "BlueSky_eval_pop"),
       width = 10, height = 10, 
       dpi = 300, units = "in", device='png')



######### Combined Plots ##########

AQS_wfszn_cnty_per_dys_grid <- NAM_2015_Comp_Codes %>% 
  mutate(smk_dy = ifelse(PM_AQS_Mean >= 20, 1, 0), per_dy = POP * ifelse(PM_AQS_Mean >= 20, 1, 0)) %>%
  group_by(FIPS5, wf_szn) %>% 
  summarise(smk_dy_ct = sum(smk_dy, na.rm = T), sum_per_dys = sum(per_dy, na.rm = T)) %>%
  left_join(US_CON_Cty, by = "FIPS5") %>% st_sf() %>%
  group_by(wf_szn) %>% mutate(max=max(smk_dy_ct)) %>%
  nest() %>%
  mutate(plot= map2(data, wf_szn, ~ggplot(data=.x) +
                      geom_sf(aes(fill = smk_dy_ct), size =0.1, color="grey55") +
                      geom_sf(data = US_CON_Cty, aes(color=STATES), color="grey90",  alpha = 0.1, fill=NA, size=0.1) +
                      geom_sf(data = US_CON_St, aes(color=STATES), color="grey65",  alpha = 0.1, fill=NA, size=0.1) +
                      geom_sf(data = GACC, aes(color=GACCAbbrev), color="black",  alpha = 0.9, fill=NA, size=0.15) +
                      geom_sf_text(data = GACC, aes(label = GACCAbbrev), colour = "black", size=2.3) +
                      coord_sf(crs ="+init=epsg:2163") +
                      scale_fill_gradientn(name = "Smoke Days", na.value="grey50",
                                           limits = c(0,  210),
                                           colours = c("#FFFFFC", get_brewer_pal("YlOrRd", n =40, contrast = c(0.2, 1.0)))) +
                      theme_void() +
                      labs(title = paste("AQS", wf_szn, "Wildfire Season")) +
                      guides(fill = guide_colourbar(barwidth = 12.0, barheight = 0.5, draw.ulim = FALSE, draw.llim = FALSE, frame.colour = "Gray80", direction = "horizontal")) +
                      theme(panel.background = element_rect(fill="white",colour ="white",size=0.5,linetype="solid"),
                            panel.grid.major = element_line(size=0.5,linetype='solid',colour="white"), 
                            panel.grid.minor = element_line(size=0.25,linetype='solid',colour="white"),
                            plot.title = element_text(hjust = 0.5, size = 10),
                            plot.subtitle = element_text(hjust = 0.5, size = 12),
                            legend.text = element_text(margin = margin(t = 5)),
                            # legend.justification = c(1.0, 0.5), 
                            legend.position = "bottom",
                            legend.box.margin=margin(c(5,5,5,5)))
  ))
AQS_plot_names <- paste("AQS", c("Early", "Off", "Peak"), sep="_")
assign(AQS_plot_names, AQS_wfszn_cnty_per_dys_grid$plot)



NAM_wfszn_cnty_smk_dys_grid <- cnty_smk_pop %>%
  filter(Year==2015) %>%
  mutate(smk_dy = ifelse(DY_Max >= 20, 1, 0), per_dy = POP * ifelse(DY_Max >= 20, 1, 0)) %>%
  group_by(FIPS5, wf_szn) %>% 
  summarise(smk_dy_ct = sum(smk_dy, na.rm = T), sum_per_dys = sum(per_dy, na.rm = T)) %>%
  left_join(US_CON_Cty, by = "FIPS5") %>% st_sf() %>%
  group_by(wf_szn) %>% mutate(max=max(smk_dy_ct)) %>% 
  nest() %>%
  mutate(plot= map2(data, wf_szn, ~ggplot(data=.x) +
                      geom_sf(aes(fill = smk_dy_ct), size =0.1, color="grey55") +
                      geom_sf(data = US_CON_Cty, aes(color=STATES), color="grey90",  alpha = 0.1, fill=NA, size=0.1) +
                      geom_sf(data = US_CON_St, aes(color=STATES), color="grey65",  alpha = 0.2, fill=NA, size=0.1) +
                      geom_sf(data = GACC, aes(color=GACCAbbrev), color="black",  alpha = 0.9, fill=NA, size=0.15) +
                      geom_sf_text(data = GACC, aes(label = GACCAbbrev), colour = "black", size=2.3) +
                      coord_sf(crs ="+init=epsg:2163") +
                      scale_fill_gradientn(name = "Smoke Days", na.value="grey50",
                                           limits = c(0, 210),
                                           colours = c("#FFFFFC", get_brewer_pal("YlOrRd", n = 40, contrast = c(0.2, 1.0)))) +
                      theme_void() +
                      labs(title = paste("NAM", wf_szn, "Wildfire Season")) +
                      guides(fill = guide_colourbar(barwidth = 12.0, barheight = 0.5, draw.ulim = FALSE, draw.llim = FALSE, frame.colour = "Gray80", direction = "horizontal")) +
                      theme(panel.background = element_rect(fill="white",colour ="white",size=0.5,linetype="solid"),
                            plot.title = element_text(hjust = 0.5, size = 10),
                            plot.subtitle = element_text(hjust = 0.5, size = 12),
                            legend.text = element_text(margin = margin(t = 5)),
                            # legend.justification = c(1.0, 0.5), 
                            legend.position = "bottom",
                            legend.box.margin=margin(c(5,5,5,5)))
  ))
NAM_plot_names <- paste("NAM", c("Early", "Off", "Peak"), sep="_")
assign(NAM_plot_names, NAM_wfszn_cnty_smk_dys_grid$plot)



wf_szn_smkdy_grid  <- grid.arrange(grobs= c(get(AQS_plot_names), get(NAM_plot_names)), ncol = 3)
ggsave(filename="AQS_NAM_wfszn_grid.png", plot=wf_szn_smkdy_grid,
       path = file.path(out.dir, "BlueSky_eval_pop"),
       width = 15, height = 10, 
       dpi = 300, units = "in", device='png')








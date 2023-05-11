############# FW6 INDICATOR ############
required.packages <- c("dplyr","ggplot2","sf","lubridate","stringr", "svglite","ggrepel","tidyverse")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only = TRUE)

rm(new.packages,required.packages)
# 1. Importing
setwd(dir = "G:/Arnaud/Contrats/CNRS - Plankton indicators/Donnees/FW6 Indicator/")
dat <- read.csv(choose.files(),sep = ";", dec = ",")

# 2. Subsetting zooplankton within plankton dataset
dat <- 
  dat %>%
  filter(Type == "Zooplankton")

# 3. Converting a the Date column into as.Date object
dat$Date <- dmy(dat$Date)

class(dat$Date)

# 4. Remove NA entries
dat <- na.omit(dat[,c(1,3,4,11)])

# 5. Group by Latitude and Longitude
dat <-
  dat %>% 
  group_by(Latitude,Longitude) %>% 
  dplyr::mutate(station=cur_group_id()) 

# 6. Subsetting OSPAR from non-OSPAR data
dat <- 
  dat %>% 
  filter(Longitude < 10)

# 7. Plotting sampling effort per station
freq <-
  dat %>%
  group_by(station = station, month = lubridate::floor_date(Date, 'month')) %>%
  tally()

freq$station <- str_replace(freq$station, "10$", "AMRU2")
freq$station <- str_replace(freq$station, "11$", "SWWBA")
freq$station <- str_replace(freq$station, "12$", "NSB3")
freq$station <- str_replace(freq$station, "15$", "NSGR2")
freq$station <- str_replace(freq$station, "16$", "STYL2")
freq$station <- str_replace(freq$station, "18$", "URST3")
freq$station <- str_replace(freq$station, "20$", "DTEND")
freq$station <- str_replace(freq$station, "4$", "HELGO")
freq$station <- str_replace(freq$station, "7$", "NGW8")

freq$station[c(1:4)] <- "ES1"
freq$station[c(5:10)] <- "UFSDB"
freq$station[c(17:22)] <- "NEFB"

setwd("G:/Arnaud/Contrats/CNRS - Plankton indicators/Figures/FW6/")

img <-
  ggplot(freq, aes(x= month, y = n)) +
  geom_bar(stat = 'identity', position = "dodge2") +
  facet_wrap(~station) +
  theme_bw() +
  ylab("Sampling frequency") +
  xlab("Year") +
  theme(axis.text=element_text(angle = 45, size=8, vjust = 0.5, hjust=1)) 

ggsave(file="Sampling effort.svg", plot=img, width=15, height=10,units = "cm")

# 8. Group by month for each station 
temp <- 
  dat %>% 
  group_by(station,Latitude, Longitude,year=floor_date(Date, "year")) %>%
  summarize(across(Biomass, list(sum)))

# 9. Replace station by the True name
temp$station <- str_replace(temp$station, "10$", "AMRU2")
temp$station <- str_replace(temp$station, "11$", "SWWBA")
temp$station <- str_replace(temp$station, "12$", "NSB3")
temp$station <- str_replace(temp$station, "15$", "NSGR2")
temp$station <- str_replace(temp$station, "16$", "STYL2")
temp$station <- str_replace(temp$station, "18$", "URST3")
temp$station <- str_replace(temp$station, "20$", "DTEND")
temp$station <- str_replace(temp$station, "4$", "HELGO")
temp$station <- str_replace(temp$station, "7$", "NGW8")

temp$station[1] <- "ES1"
temp$station[c(2,3,4)] <- "UFSDB"
temp$station[c(8,9,10)] <- "NEFB"

# 10. Create a map of unique station

st <- unique(temp[c("Latitude", "Longitude","station")])

setwd("G:/Arnaud/Script/Maps_R/ne_10m_land/")
land <- st_read(dsn = ".", layer = "ne_10m_land")
land <- st_zm(land[which(st_geometry_type(land) == "MULTIPOLYGON"),],drop = TRUE)

xlabs = seq(3, 10, by = 1)
ylabs = seq(53, 56, by = 0.5)

img <- 
  ggplot() +
  geom_sf(data = land) +
  geom_point(dat = st, mapping = aes(x = Longitude, y = Latitude)) +
  geom_label_repel(dat = st, aes(x = Longitude, y = Latitude, label = station),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  scale_x_continuous(breaks = xlabs, labels = paste0(xlabs,'°W')) +
  scale_y_continuous(breaks = ylabs, labels = paste0(ylabs,'°N')) +
  xlab("") +
  ylab("") +
  coord_sf(xlim = c(3,10), ylim = c(53,56)) + 
  theme_bw() +
  theme(axis.text=element_text(angle = 45, size=8, vjust = 0.5, hjust=1))

img

setwd("G:/Arnaud/Contrats/CNRS - Plankton indicators/Figures/FW6/")

ggsave(file="Sample point.svg", plot=img, width=15, height=10,units = "cm")

# 11. Produce the yearly zooplankton biomass
options(scipen = 999)
temp$station <- as.character(temp$station)

p <- bquote(atop(Zooplankton~biomass~(10^5~µg~m^-3)))

img <-
  ggplot(temp) +
  geom_point(aes(x = year, y = Biomass_1/100000)) +
  geom_line(aes(x = year, y = Biomass_1/100000)) +
  facet_wrap(~station) +
  ylab(p) +
  theme_bw() +
  theme(axis.text=element_text(angle = 45, size=8, vjust = 0.5, hjust=1)) 
  
ggsave(file="Time_series BSH.svg", plot=img, width=15, height=10,units = "cm")


# 12. Display only the year rather than %Y-%M-%d
temp$year <- str_sub(temp$year, start = 1, end = 4)


p <- bquote(atop(Zooplankton~biomass~phantom(),
                 (10^5~µg~m^-3)))

# 13. Plot the yearl zooplankton biomass
img <-
  ggplot() +
  geom_sf(data = land) +
  geom_point(data = temp, mapping = aes(x = Longitude, y = Latitude, size = Biomass_1/100000)) +
  facet_wrap(~year) +
  scale_x_continuous(breaks = xlabs, labels = paste0(xlabs,'°W')) +
  scale_y_continuous(breaks = ylabs, labels = paste0(ylabs,'°N')) +
  xlab("") +
  ylab("") +
  coord_sf(xlim = c(3,10), ylim = c(53,56)) + 
  theme_bw() +
  theme(axis.text=element_text(angle = 45, size=8, vjust = 0.5, hjust=1)) +
  guides(size=guide_legend(title=p))

ggsave(file="Biomass map.svg", plot=img, width=20, height=14,units = "cm")


# 14. Building biomass from zooplankton abundance and carbon content masterlist
setwd("F:/PANACEA T1.5/D1.5a")

dat <- read.csv(choose.files(),sep = ";", dec = ",")

masterlist <- readxl::read_xlsx(choose.files())

## Average value of Carbon Content per taxa
carb_cont <- masterlist %>%
  filter(Measure == "C") %>%
  group_by(Taxa_Name,WoRMS_AphiaID) %>%
  summarise(avg = mean(Average)) %>%
  ungroup()

## join the data table and the masterlist
dat <- dat %>% 
  left_join(carb_cont, by = join_by(AphiaID == WoRMS_AphiaID))

## Estimate biomass from abundance and the average carbon content value
dat$Biomass <- dat$Abundance*dat$avg

## Format the date into ISO date
dat$Date <- as.Date(dat$Date, format = "%d/%m/%Y")
class(dat$Date)

## Sum the data per date
dat <- 
  dat %>% 
  group_by(Station,Date) %>%
  summarise(Biomass = sum(Biomass,na.rm = TRUE))

## Mean the data for each month
dat <-
  dat %>% 
  group_by(month = lubridate::floor_date(Date, 'month')) %>%
  summarise(total_biomass = mean(Biomass))

p <- bquote(atop(Zooplankton~biomass~(10^5~µg~m^-3)))


require(ggformula)
img <- 
  ggplot(data = dat, aes (x = month, y = total_biomass / 10^4)) +
  geom_point(size = 0.5) +
  geom_line(linewidth = 0.2) +
  geom_lm(fill = "blue", linetype = "dashed" ) +
  xlab("Date") +
  ylab(p) +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 10),
    axis.text.x = element_text(size = 8),
    axis.title.y = element_text(size = 10),
    axis.text.y = element_text(size = 8))

setwd("G:/Arnaud/Contrats/CNRS - Plankton indicators/Figures/FW6/")
ggsave(file="Estimated biomass L4.svg", plot=img, width=15, height=10,units = "cm")


## Check if the annual biomass is the same magnitude as the one observed from direct measure in the field (DE-BSH)
temp <- 
  dat %>% 
  group_by(year=floor_date(month, "year")) %>%
  summarize(across(total_biomass, list(sum)))

img <- 
  ggplot(data = temp, aes (x = year, y = total_biomass_1 / 10^5)) +
  geom_point(size = 0.5) +
  geom_line(linewidth = 0.2) +
  geom_lm(fill = "blue", linetype = "dashed" ) +
  xlab("Date") +
  ylab(p) +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 10),
    axis.text.x = element_text(size = 8),
    axis.title.y = element_text(size = 10),
    axis.text.y = element_text(size = 8))

setwd("G:/Arnaud/Contrats/CNRS - Plankton indicators/Figures/FW6/")
ggsave(file="annual biomass budget L4.svg", plot=img, width=15, height=10,units = "cm")

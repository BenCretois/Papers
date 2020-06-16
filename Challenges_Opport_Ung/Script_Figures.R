# --------------------------------------------------------------------------------
# --------------------FIGURES FOR CHALLENGES AND OPPORTUNITIES -------------------
# --------------------------------------------------------------------------------

# Libraries -------------------------------------------------------------------

library(tidyverse)
library(sf)
library(reshape2)
library(cowplot)
library(raster)

# -----------------------------------------------------------------------------

# Open dataset with the native ungulates - already there for another analysis
data <- st_read('/Data/mammals_sp.shp') %>% 
  dplyr::select(id, PA, Chamois, IbrnIbx, Ibex, Isard, Moose, 
                RedDeer, RoeDeer, WildBor,WldRndr)

# Make a sf with only IDs
ids <- data %>% dplyr::select(id)

# Open the other species that I need to merge -----------------------------

BarbarySheep <- read_sf("//home.ansatt.ntnu.no/benjamcr/Desktop/Research/Project_DistributionUngulates/Distribution_Data/Layer_Species/BarbarySheep.shp") %>% 
  mutate(BarbarySheep = 1) %>% 
  dplyr::select(BarbarySheep) %>% 
  st_centroid()

Bison <- read_sf("//home.ansatt.ntnu.no/benjamcr/Desktop/Research/Project_DistributionUngulates/Distribution_Data/Layer_Species/Bison.shp") %>% 
  mutate(Bison = 1) %>% 
  dplyr::select(Bison) %>% 
  st_centroid()

ChineeseWaterDeer <- read_sf("//home.ansatt.ntnu.no/benjamcr/Desktop/Research/Project_DistributionUngulates/Distribution_Data/Layer_Species/ChineeseWaterDeer.shp") %>% 
  mutate(ChineeseWaterDeer = 1) %>% 
  dplyr::select(ChineeseWaterDeer) %>% 
  st_centroid()

FallowDeer <- read_sf("//home.ansatt.ntnu.no/benjamcr/Desktop/Research/Project_DistributionUngulates/Distribution_Data/Layer_Species/FallowDeer.shp") %>% 
  mutate(FallowDeer = 1) %>% 
  dplyr::select(FallowDeer) %>% 
  st_centroid()

Mouflon <- read_sf("//home.ansatt.ntnu.no/benjamcr/Desktop/Research/Project_DistributionUngulates/Distribution_Data/Layer_Species/Mouflon.shp") %>% 
  mutate(Mouflon = 1) %>% 
  dplyr::select(Mouflon) %>% 
  st_centroid()

Muntjac <- read_sf("//home.ansatt.ntnu.no/benjamcr/Desktop/Research/Project_DistributionUngulates/Distribution_Data/Layer_Species/Muntjac.shp") %>% 
  mutate(Muntjac = 1) %>% 
  dplyr::select(Muntjac) %>% 
  st_centroid()

SikaDeer <- read_sf("//home.ansatt.ntnu.no/benjamcr/Desktop/Research/Project_DistributionUngulates/Distribution_Data/Layer_Species/SikaDeer.shp") %>% 
  mutate(SikaDeer = 1) %>% 
  dplyr::select(SikaDeer) %>% 
  st_centroid()

WhiteTailedDeer <- read_sf("//home.ansatt.ntnu.no/benjamcr/Desktop/Research/Project_DistributionUngulates/Distribution_Data/Layer_Species/WhiteTailedDeer.shp") %>% 
  mutate(WhiteTailedDeer = 1) %>% 
  dplyr::select(WhiteTailedDeer) %>% 
  st_centroid()

Muskox <- read_sf("//home.ansatt.ntnu.no/benjamcr/Desktop/Research/Project_DistributionUngulates/Distribution_Data/Layer_Species/Muskox.shp") %>% 
  mutate(Muskox = 1) %>% 
  dplyr::select(Muskox) %>% 
  st_centroid()

# Intercept the geometries ----------------------------------------------------
ids_barbarysheep <- st_intersection(ids, BarbarySheep) %>% st_drop_geometry()
ids_bison <- st_intersection(ids, Bison) %>% st_drop_geometry()
ids_chineesewd <- st_intersection(ids, ChineeseWaterDeer) %>% st_drop_geometry()
ids_fallowdeer <- st_intersection(ids, FallowDeer) %>% st_drop_geometry()
ids_mouflon <- st_intersection(ids, Mouflon) %>% st_drop_geometry()
ids_muntjac <- st_intersection(ids, Muntjac) %>% st_drop_geometry()
ids_sikadeer <- st_intersection(ids, SikaDeer) %>% st_drop_geometry()
ids_whitetd <- st_intersection(ids, WhiteTailedDeer) %>% st_drop_geometry()
ids_muskox <- st_intersection(ids, Muskox) %>% st_drop_geometry()

# Join to the main dataset ------------------------------------------------------
data <- left_join(data, ids_barbarysheep, by='id') 
data <- left_join(data, ids_bison, by='id') 
data <- left_join(data, ids_chineesewd, by='id') 
data <- left_join(data, ids_fallowdeer, by='id') 
data <- left_join(data, ids_mouflon, by='id') 
data <- left_join(data, ids_muntjac, by='id') 
data <- left_join(data, ids_sikadeer, by='id') 
data <- left_join(data, ids_whitetd, by='id') 
data <- left_join(data, ids_muskox, by='id') 

# Set the absences to 0 instead of NAs -----------------------------------------
data <- data %>% replace_na(list(BarbarySheep = 0,
                                       Bison = 0, 
                                       ChineeseWaterDeer = 0, 
                                       FallowDeer = 0,
                                       Mouflon = 0,
                                       Muntjac = 0,
                                       SikaDeer = 0,
                                       WhiteTailedDeer = 0,
                                       Muskox = 0)) 


# Some columns are factors, transform everything into numeric ---------------
data[, 2:20] <- apply(data[, 2:20],2, function(x) as.numeric(as.character(x)))

# Melt data for the lollipop charts -----------------------------------------

meltdata <- data %>% dplyr::select("PA", "Chamois", "Ibex", "Isard", "IbrnIbx",
                                          "RoeDeer", "RedDeer", "Moose", "WldRndr",
                                          "WildBor", "Bison", "BarbarySheep", "ChineeseWaterDeer", "FallowDeer",
                                          "Muntjac", "SikaDeer", "WhiteTailedDeer", "Muskox", "Mouflon") %>% 
  st_drop_geometry() %>% 
  melt(., id.vars = "PA") %>% 
  filter(value == 1) %>% 
  mutate(cell = 100) %>% 
  drop_na()

# --------------------------------------------------------------------------------
# Build dataset for the lollipop chart -> FOR PROTECTED AREA ---------------------
# --------------------------------------------------------------------------------

PAsp <- meltdata %>%  group_by(variable) %>%  summarise(n = (sum(PA) / sum(cell)*100))

PAsp$Family <- c("Bovidae","Bovidae","Bovidae","Bovidae","Cervidae","Cervidae","Cervidae","Cervidae",
                 "Suidae", "Bovidae", "Bovidae", 
                 "Cervidae", "Cervidae", "Cervidae", "Cervidae", "Cervidae", "Bovidae", "Bovidae")
PAsp$sp <- c("Alpine chamois", "Alpine ibex", "Pyrenean chamois", "Iberian ibex",
             "Roe deer", "Red deer", "Moose", "Wild reindeer", "Wild boar",
             "European bison", "Barbary sheep", "Chinese water deer", "Fallow deer", "Reeves' muntjac",
             "Sika deer", "White tailed deer", "Muskox", "Mouflon")
PAsp$text <- round(PAsp$n, 0)

Europe <- data.frame(variable = "z", n = sum(data$PA) / (length(data$PA)),
                     Family="Europe", sp ="Europe", text=25)

PAsp <- rbind(PAsp, Europe)

# Order for the plot --------------------------------------------------
nameord <- PAsp$sp[order(PAsp$Family, PAsp$n)]
PAsp$sp <- factor(PAsp$sp, levels=nameord)
PAsp$Family_fixed <- factor(PAsp$Family, levels= c("Bovidae", "Cervidae", "Suidae", "Europe"))

# And plot ! ----------------------------------------------------------
b <- PAsp %>% ggplot(., aes(x=sp, y=n, col=Family)) +
  geom_segment(aes(x=sp, xend=sp, y=0, yend=n)) +
  geom_point(size=7) +
  #Colors
  scale_color_manual(values=c("#00AFBB", "#E7B800", "#003399","#FC4E07")) +
  #Custom theme
  theme_classic() +
  theme(axis.title.y = element_blank(),
        strip.text.y = element_blank()) +
  coord_flip() +
  facet_grid(Family_fixed ~ ., scales = "free_y", space="free_y") +
  geom_text(aes(y=n, label=text), colour = "white", size=3.5) +
  ylab("Percentage of species' distribution within protected areas") 

# --------------------------------------------------------------------------------
# Build dataset for the lollipop chart -> FOR EUROPEAN COVERAGE ------------------
# --------------------------------------------------------------------------------

dfarea <- meltforPA %>%  group_by(variable) %>% summarise(n = (n() / 48511)*100) %>% 
  mutate(Family = c("Bovidae","Bovidae","Bovidae","Bovidae","Cervidae","Cervidae","Cervidae","Cervidae",
                    "Suidae", "Bovidae", "Bovidae", 
                    "Cervidae", "Cervidae", "Cervidae", "Cervidae", "Cervidae", "Bovidae", "Bovidae")) %>% 
  mutate(sp = c("Alpine chamois", "Alpine ibex", "Pyrenean chamois", "Iberian ibex",
                "Roe deer", "Red deer", "Moose", "Wild reindeer", "Wild boar",
                "European bison", "Barbary sheep", "Chinese water deer", "Fallow deer", "Reeves' muntjac",
                "Sika deer", "White tailed deer", "Muskox", "Mouflon")) %>% 
  mutate(text = round(n, 0)) %>% 
  mutate(Status = c("Native", "Native", "Native", "Native", "Native", "Native", "Native", "Native",
                    "Native", "Native", "Exotic", "Exotic","Exotic","Exotic","Exotic","Exotic",
                    "Exotic","Exotic"))
dfarea$text <- as.character(dfarea$text)  
for(i in 1:18){if(dfarea[i,5] == 0) { dfarea[i,5] = "<1"}}

nameord <- dfarea$sp[order(dfarea$Family, dfarea$n)]
dfarea$sp <- factor(dfarea$sp, levels=nameord)
dfarea$Family_fixed <- factor(dfarea$Family, levels=c("Cervidae", "Suidae", "Bovidae"))

a <- dfarea %>% ggplot(., aes(x=sp, y=n, col=Family_fixed)) +
  geom_segment(aes(x=sp, xend=sp, y=0, yend=n)) +
  geom_point(size=7) +
  #Colors
  scale_color_manual(values=c("#E7B800","#FC4E07","#00AFBB")) +
  #Custom theme
  theme_classic() +
  theme(axis.title.y = element_blank(),
        strip.text.y = element_blank()) +
  coord_flip() +
  facet_grid(Family_fixed ~ ., scales = "free_y", space="free_y") +
  geom_text(aes(y=n, label=text), colour = "white") +
  ylab("Percentage of species' distribution covering Europe") +
  labs(color = 'Family')

a
b
# ----------------------------------------------------------------------
# Figure for ungulate diversity ----------------------------------------
# ----------------------------------------------------------------------

# Isolate species and remove geometry -> provoque trouble when summing
for_tot <- data %>% dplyr::select(1:20) %>% st_drop_geometry()

for_tot$PreyNative = rowSums(for_tot[, c("Chamois", "Ibex", "Isard", "IbrnIbx",
                                               "RoeDeer", "RedDeer", "Moose", "WldRndr",
                                               "WildBor", "Bison")]) 

for_tot$PreyExotic = rowSums(for_tot[, c("BarbarySheep", "ChineeseWaterDeer", "FallowDeer",
                                               "Muntjac", "SikaDeer", "WhiteTailedDeer", "Muskox", "Mouflon")]) 

# Re-make the dataset spatial
for_tot_sp <- left_join(for_tot, ids, by='id') %>% 
  st_as_sf()


# Histograms -----------------------------------------------------------------

native_df <- for_tot_sp %>% group_by(PreyNative) %>% summarise(n=n())
native_df$PreyNative <- as.numeric(as.character(native_df$PreyNative))

native_hist <- ggplot(native_df, aes(x=PreyNative, y=(n/48499)*100, fill=PreyNative)) + 
  geom_histogram(stat="identity") + 
  theme_classic() +
  xlab("Number of native ungulates") + ylab("Percentage of Europe covered ") +
  scale_fill_viridis(option="inferno", direction=-1) +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5)) +
  theme(legend.position = "none",
        text = element_text(size = 12)) 

exotic_df <- for_tot_sp %>% group_by(PreyExotic) %>% summarise(n=n()) 
exotic_df$PreyExotic <- as.numeric(as.character(exotic_df$PreyExotic))

exotic_hist <- ggplot(exotic_df, aes(x=PreyExotic, y=(n/48499)*100, fill=PreyExotic)) + 
  geom_histogram(stat="identity") + 
  theme_classic() +
  xlab("Number of introduced ungulates") + ylab(' ') +
  scale_fill_viridis(option="inferno", direction=-1) +
  scale_x_discrete(breaks = c(0, 1, 2, 3, 4)) +
  theme(legend.position = "none",
        text = element_text(size = 12)) 

plot_grid(native_hist, exotic_hist)

# Maps --------------------------------------------------------------------

# Open shapefile of europe, transform coordinate system and select area of interest
europe <- read_sf('//home.ansatt.ntnu.no/benjamcr/Desktop/Data_ProjUng/Working_shp/Europe_Full.shp')
europe3035 <- st_transform(europe, crs=3035)
box <- st_bbox(data) 
box_sf <- st_as_sfc(box)
europe3035_box <- st_intersection(europe3035, box_sf) %>% 
  dplyr::select(NUTS_ID)

# Make data as factor
for_tot_sp$PreyExotic <- as.factor(for_tot_sp$PreyExotic)
for_tot_sp$PreyNative <- as.factor(for_tot_sp$PreyNative)

# Make native and exotic maps
exotic_map <- tm_shape(europe3035_box) +
  tm_polygons(col = "white") +
  tm_shape(for_tot_sp) +
  tm_polygons('PreyExotic', border.alpha = 0, title = "Introduced ungulates", palette = "-inferno", breaks = c(0,1,2,3,4,5)) +
  tm_layout(frame = F)

native_map <- tm_shape(europe3035_box) +
  tm_polygons(col = "white") +
  tm_shape(for_tot_sp) +
  tm_polygons('PreyNative', border.alpha = 0, title = "Native ungulates", palette = "-inferno", breaks = c(0,1,2,3,4,5)) +
  tm_layout(frame = F,
            title.size = 2)

tmap_arrange(native_map, exotic_map)
# Plot the figure total!

plot_grid(native_map, exotic_map, native_hist, exotic_hist,
          nrow = 2,ncol=2)
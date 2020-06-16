# ------------------------------------------------------------------------------------------------------------------
# ------------------------------ SCRIPT FOR HUNTERS AS CITIZEN SCIENTISTS ------------------------------------------
# ------------------------------------------------------------------------------------------------------------------

# Libraries ----------------------------------------------------------------------------------------------------

library(readxl)
library(xlsx)
library(tidyselect)
library(tidyverse)
library(ggsci)
library(viridis)
library(tmap)
library(tidyverse)
library(foreign)
library(sf)
library(tmap)
library(cowplot)
library(rphylopic)
library(ggimage)


# Functions ---------------------------------------------------------------------------------------------------------

# Species groups built

Ungulates <- tibble(sp = c('ungulates','moose', 'red deer', 'roe deer', 'reindeer', 'white tailed deer',
                           'fallow deer', 'wild boar','sika deer', 'white-tailed deer', 'wild reindeer', 'forest reindeer',
                           'chamois', 'mouflon', 'isard', 'ibex', 'bison',
                           'iberian ibex', 'deer', 'ungulates', 'barbary sheep'), group = 'Ungulates')

Large_carn <- tibble(sp=c('large carnivores','wolf', 'bear', 'brown bear','wolverine', 'lynx', 'carnivore'),
                     group ='Large carnivores')

Waterfowls <- tibble(sp=c('waterfowl', 'goosander','wigeon', 'mallard', 'geese', 'goose', 'teal', 'common teal',
                          'eider', 'pintail', 'shoveler',
                          'pochard', 'greylag', 'greylag goose', 'goldeneye', 'duck',
                          'swans', 'swan', 'merganser', 'scoter', 'common scoter', 
                          'grebe', 'garganey', 'bean goose', 'tufted duck', 'white-fronted goose',
                          'gadwall', 'greater white-fronted goose', 'canada goose', 'barnacle goose',
                          'greater white-fronted goose', 'greater white fronted goose', 'greater scaup',
                          'long tailed duck', 'black scoter', 'velvet scoter', 'red crested pochard', 'common coot', 'coot', 'red breasted merganser',
                          'waterfowl'), group='Waterfowl')

Other_birds <- tibble(sp=c('grey partridge', 'partridge', 'red legged partridge', 'rock partridge', 'grouse', 'ptarmigan', 'pheasant', 'perdrix',
                           'capercaillie', 'black grouse',
                           'forest grouse', 'hazel grouse', 'willow grouse', 'willow ptarmigan', 'rock ptarmigan',
                           'red grouse', 'quail', 'hazel hen','dove', 'turtle dove','pigeon', 'rock pigeon',
                           'woddcock', 'woodcock', 'turnix', 'tern', 'snipe',
                           'redshank', 'red knot', 'grey plover',
                           'golden plover', 'gulls', 'kestrel','heron', 'magpie', 'song thrush',
                           'owl', 'carrion crow', 'hooded crow', 'rook', 'jackdaw', 'jay',
                           'curlew grouse', 'black headed gull', 'mew gull', 'european herring gull', 'herring gull', 'common gull',
                           'great black-backed gull', 'black-headed gull', 'wood pigeon', 'gull', 'great cormorant', 'ring necked pheasant',
                           'northern bobwhite', 'california quail', 'common starling', 'common pheasant', 'silver pheasant',
                           'golden pheasant', 'reeves pheasant', 'bar tailed godwit', 'black tailed godwit', 'jacksnipe',
                           'common snipe', 'ruff', 'common greenshank', 'common redshank', 'spotted redshank',
                           'common curlew', 'whimbrel', 'moorhen', 'eurasian oystercatcher', 'european golden plover',
                           'lapwing', 'water rail', 'skylark', 'eurasian woodcock', 'common quail', 'mistle thrush',
                           'redwing', 'fieldfare', 'blackbird', 'common woodpigeon', 'stock dove', 'european turtle dove',
                           'collared dove', 'goshawk', 'raven', 'other game birds'), group='Other game birds')

Small_game <- tibble(sp=c('hare', 'mountain hare', 'rabbit','beaver', 'marmot', 'coypus', 'beaver', 'squirell', 'red squirell',
                          'fox', 'red fox', 'mink', 'american mink',
                          'badger', 'racoon', 'polecat', 
                          'otter', 'golden jackal','dogs',
                          'seal', 'mustelid', 'raccoon dog', 'marten',
                          'egyptian mangoose', 'pine marten', 'stone marten', 'stoat',
                          'weasel', 'brown hare', 'coypu', 'musk rat', 'raccoon', 'blue hare', 'european hare', 
                          'flying squirrel', 'arctic fox', 'least weasel', 'red squirrel', 
                          'squirrel', 'common jackal', 'small game'), group='Small game')

undif_sp <- tibble(sp=c('undifined species', 'game'))

group_tot <- bind_rows(Ungulates, Large_carn, Waterfowls, Other_birds, Small_game, undif_sp)


# Function for spread with multiple keys
spread_with_multiple_values <- function(data, key, value, fill = NA,
                                        convert = FALSE, drop = TRUE,
                                        sep = NULL, aggfunc = NULL, ...) {
  args <- list(...)
  key_var <- vars_pull(names(data), !!enquo(key))
  value_var <- vars_pull(names(data), !!enquo(value))
  by <- colnames(data)[which(!colnames(data) %in% c(key_var, value_var))]
  col <- data %>%
    pull(key_var) %>%
    unique() %>%
    as.character()
  
  data <- map(
    col,
    function(x) data %>%
      filter(!!sym(key_var) == x)
  ) %>%
    map2(col, ~change_colname(.x, .y, value_var, key_var)) %>%
    map2(col, ~apply_aggfunc(.x, .y,
                             group_by_col = by,
                             aggfunc = aggfunc,
                             args
    )) %>%
    map2(col, ~apply_convert(.x, .y, convert)) %>%
    map2(col, ~apply_sep(.x, .y, key_var, sep)) %>%
    reduce(full_join, by = by)
  
  if (!drop) {
    data <- data %>% complete_(by)
  }
  
  if (!is.na(fill)) {
    key_value_cols <- colnames(data)[which(!colnames(data) %in% by)]
    data <- data %>% mutate_at(
      vars(one_of(key_value_cols)),
      funs(replace(., is.na(.), fill))
    )
  }
  
  return(data)
}

change_colname <- function(data, new_col, value, old_col) {
  data %>%
    rename(!!as.character(new_col) := !!value) %>%
    dplyr::select(-one_of(old_col))
}


apply_aggfunc <- function(data, col_name, group_by_col, aggfunc, args) {
  if (is.function(aggfunc)) {
    data <- data %>%
      group_by(!!!syms(group_by_col)) %>%
      summarize(
        !!col_name := do.call(
          aggfunc,
          args = c(list(!!sym(col_name)), args) %>% compact()
        )
      ) %>%
      ungroup()
  } else {
    data
  }
}

apply_convert <- function(data, col_name, convert) {
  values <- data[[col_name]]
  if (convert & !is_character(values)) {
    values <- as.character(values)
    values <- type.convert(values, as.is = TRUE)
  }
  data <- data %>% mutate(!!col_name := values)
}

apply_sep <- function(data, new_col, old_col, sep) {
  if (!is.null(sep)) {
    data %>%
      rename(!!str_c(as.character(old_col), as.character(new_col),
                     sep = sep
      ) := !!as.character(new_col))
  } else {
    data
  }
}
# Function to replace species group by the name

rename_group <- function(df){
  for(i in 1:nrow(df[ , which(colnames(df) == 'SpName')])){
    for(j in 1:length(group_tot$sp)){
      if(df[i, which(colnames(df) == 'SpName')] == group_tot[j,1]) df[i, which(colnames(df) == 'SpName')] <- group_tot[j,2]
    }}
  return(df)
}




# --------------------------------------------------------------------------------------------------------------------
# Vizualisation of the results ------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------------

# Open the dataset ----
review <- read_excel('Data/Data_review.xlsx') 


# Tidying the dataset ----

# Tidy the previous dataset and select only the columns we are interested in 
# Countries / Species / EBV1 / EBV2 / EBV3 / EBV4 / all the methods (Measurements) 
# and the source of the litterature

EBV <- review %>% dplyr::select(starts_with('Country'), starts_with('Species'), 
                                starts_with('EBVcandidate'), starts_with('Measurement'), Litt, Ref...114) %>%  
  gather(., 'Species', 'SpName', starts_with('Species')) %>%
  drop_na(SpName) %>% 
  rename_group() %>% 
  gather(., 'Country', 'CountryName', starts_with('Country')) %>% 
  drop_na(CountryName) %>%
  gather(., 'EBV', 'EBVName', starts_with('EBV')) %>% 
  drop_na(EBVName) %>% 
  gather(., 'Measurement', 'MeasurementName', starts_with('Measurement')) %>% 
  drop_na(MeasurementName) %>% 
  dplyr::select(CountryName=CountryName, SpName, EBVName, MeasurementName, Litt, Ref...114) %>% 
  unique() %>% 
  drop_na() %>% 
  filter(CountryName != 'Central Europe' & CountryName != 'Central europe') %>% 
  filter(SpName != 'undefined species')




# Summary tables -----------------------------------------------------------------------------

# ... for countries 
# In the first table we make the difference between whether it was found in the systematic litt
# search or in other sources (respectively 1 and 0)

country_sp <- EBV %>%  dplyr::select(CountryName, SpName, Litt) %>% 
  unique() %>% 
  spread_with_multiple_values(key = SpName, value=Litt, aggfunc = str_c, collapse = '-') 

# Secund table only for found / not found irrespectively of the litterature source
country_sp_bin <- sapply(country_sp[, 2:6], function(x) ifelse(is.na(x), '0', '1')) %>% 
  as.data.frame() %>% 
  mutate(CountryName = country_sp$CountryName)

# ... for species
EBV_sp <- EBV %>%  dplyr::select(EBVName, SpName, Litt) %>% 
  unique() %>% 
  spread_with_multiple_values(key = SpName, value = Litt, aggfunc = str_c, collapse = '-')

# ... for methods
# More tricky, I need to separate the EBV and their respective methods
# Otherwise measurement for EBV2 is on the same line as EBV1, building a wrong table ...

# First, isolate the EBV and their respective methods
EBV1 <- review %>% dplyr::select(EBVcandidate1, starts_with('Measurement1')) %>% gather(., 'Method', 'MethodName', 2:5) %>% drop_na() %>% dplyr::select(EBV = 1, MethodName = MethodName)
EBV2 <- review %>% dplyr::select(EBVcandidate2, starts_with('Measurement2')) %>% gather(., 'Method', 'MethodName', 2:3) %>% drop_na() %>% dplyr::select(EBV = 1, MethodName = MethodName)
EBV3 <- review %>% dplyr::select(EBVcandidate3, starts_with('Measurement3')) %>% drop_na() %>% dplyr::select(EBV = 1, MethodName = Measurement31)
EBV4 <- review %>% dplyr::select(EBVcandidate4, starts_with('Measurement4')) %>% drop_na() %>% dplyr::select(EBV = 1, MethodName = Measurement4)

# Then gather and build the table
# We then do a proper table in excel

EBV_met <- rbind(EBV1,EBV2,EBV3,EBV4) %>%   
  dplyr::select(EBV, MethodName) %>% 
  unique() %>% 
  spread(key = MethodName, value = MethodName)


# Geographic extent of hunter based monitoring - Building the map ----

# First, replace the numbers by proper names for the legend
country_sp[is.na(country_sp)] = 'Not found'
country_sp[country_sp == '0-1'] = '1-0'
EBV_sp[is.na(EBV_sp)] = 'Not found'
EBV_sp[EBV_sp == '0-1'] = '1-0'

# Open the shapefile
europe <- read_sf('Data/Europe.shp')
plot(europe)
# Open my dataset
ds <- country_sp_bin %>% arrange(., CountryName) %>% 
  mutate(NUTS_ID = c('AT', 'BE', 'BIH', 'BG', 'HR', 'CZ', 'DK', 'EE', 'FI', 'FR', 'DE', 'EL', 'HU',
                     'IS', 'IE', 'IT', 'LV', 'LT','LUX', 'MNE','NL', 'NO', 'PL', 'PT', 'RO', 'SRB',
                     'SK', 'SI', 'ES', 'SE', 'CH','UK' ))

# Merge NUTS_ID dataset and spatial data
europe_ds <- full_join(europe,ds, by = 'NUTS_ID')

# Specify columns as character - I experienced some problems later otherwise
europe_ds$`Large carnivores` <- as.character(europe_ds$`Large carnivores`)
europe_ds$`Other game birds` <- as.character(europe_ds$`Other game birds`)
europe_ds$`Small game` <- as.character(europe_ds$`Small game`)
europe_ds$`Ungulates` <- as.character(europe_ds$`Ungulates`)
europe_ds$`Waterfowl` <- as.character(europe_ds$`Waterfowl`)

# Correct dataset 
# - For countries in which there is no species of the species group we code NA
# - For countries in which there is the species but in which we didn't find any evidence 
# of hunter based monitoring we code by 0 (for Albania, Montenegro, Serbia and Bosnia)

europe_ds <- europe_ds %>% replace(., is.na(.), 0)
europe_ds <- europe_ds %>% arrange(., NUTS_ID)

# No large carnivores in Iceland, Netherlands, UK and Ireland
europe_ds[c(17,18,28,37), 'Large carnivores'] <- NA 

# No ungulates in Iceland
europe_ds[18, 'Ungulates'] <- NA

# Make the maps
p1 <- ggplot(europe_ds) +
  geom_sf(aes(fill = `Large carnivores`)) +
  theme_void() +
  scale_fill_viridis(discrete = T, option='D',
                     labels=c('No monitoring involving hunters found',
                              'Monitoring involving hunters not found',
                              'Species group not present ')) +
  theme(legend.title = element_blank())

p2 <- ggplot(europe_ds) +
  geom_sf(aes(fill = `Ungulates`)) +
  theme_void() +
  scale_fill_viridis(discrete = T, option='D') + theme(legend.position = 'none')

p3 <- ggplot(europe_ds) +
  geom_sf(aes(fill = `Small game`)) +
  theme_void() +
  scale_fill_viridis(discrete = T, option='D')+ theme(legend.position = 'none')

p4 <- ggplot(europe_ds) +
  geom_sf(aes(fill = `Other game birds`)) +
  theme_void() +
  scale_fill_viridis(discrete = T, option='D')+ theme(legend.position = 'none')

p5 <- ggplot(europe_ds) +
  geom_sf(aes(fill = `Waterfowl`)) +
  theme_void() +
  scale_fill_viridis(discrete = T, option='D')+ theme(legend.position = 'none')

legend <- get_legend(p1)

plot_grid(p1+ theme(legend.position = 'none'),
          p2,p3,p4,p5,legend)


# Make the figure 2: Diversity of biodiversity characteristics recorded by hunter-based monitoring
# Here is a generic version of it, the full figure is made through the help of photoshop ----

# Create dataset

# Create dataset
data <- data.frame(
  individual=c('Allelic div.', 'Co-ancestry', 'Pop. genetic. diff.', 'Breed and variety div.',
               'Sp. distribution', 'Pop. abundance', 'Pop. structure',
               'Morphology', 'Movement', 'Phenology', 'Physiology', 'Reproduction',
               'Taxonomic div.', 'Sp. interaction'),
  group=c( rep('A', 4), rep('B', 3), rep('C', 5), rep('D', 2)) ,
  value=rep(40,14))


# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 4
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(group)
data$id <- seq(1, nrow(data))

# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# Make the plot
p <- ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(stat="identity", alpha=0.5) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  scale_fill_viridis(discrete = T, option='D') +
  geom_text(data=label_data, 
            aes(x=id, y=value+10, label=individual, hjust=hjust), 
            color="black", fontface="bold", 
            alpha=0.6, size=4.0, angle= label_data$angle, 
            inherit.aes = FALSE ) 

p

# Make figure 3: proportion of information yielded by the overall search ----

# First part of the figure: sources for the differet EBVs
EBV_Litt <- EBV_sp %>% 
  gather(Species, Source, 2:6) %>% 
  filter(Source != 'Not found') %>% 
  dplyr::select(Species, Source) %>% 
  group_by(Species, Source) %>%
  summarize(n = n()) %>% 
  mutate(prop = n / sum(n)) 

EBV_Litt$Source <- factor(EBV_Litt$Source, 
                          levels = c('1', 
                                     '0', '1-0'))

ebvSP <- ggplot(EBV_Litt, aes(x=Species, y=prop, fill=Source)) +
  geom_bar(position='stack', stat='identity') +
  theme_minimal() +
  xlab('') + ylab('Proportion') +
  scale_fill_viridis(discrete=T, direction=-1, 
                     labels = c('Information found in structured search only',
                                'Information found in unstructured search only',
                                'Information found in Both searches')) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        text = element_text(size=15))
ebvSP

# Secund part of the figure: sources concerning the different countries
Country_Sp_Litt <- country_sp %>% 
  gather(Species, Source, 2:6) %>% 
  filter(Source != 'Not found') %>% 
  dplyr::select(Species, Source) %>% 
  group_by(Species, Source) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  group_by(Species) %>% 
  mutate(prop = n / sum(n)) 

Country_Sp_Litt$Source <- factor(Country_Sp_Litt$Source, 
                                 levels = c('1', 
                                            '0', '1-0'))

cntry_sp <- ggplot(Country_Sp_Litt, aes(x=Species, y=prop, fill=Source)) +
  geom_bar(position='stack', stat='identity') +
  theme_minimal() +
  xlab('') + ylab('Proportion')+
  scale_fill_simpsons() +
  scale_fill_viridis(discrete=T, direction=-1) +
  theme_classic() +
  theme(legend.position = 'none',
    axis.text.x = element_text(angle=45, hjust=1),
    text = element_text(size = 15)) 
cntry_sp

legend <- get_legend(ebvSP)

plot_grid(ebvSP + theme(legend.position = 'none'), cntry_sp)
plot_grid(legend)

# Finally make table for the appendix ----

all <- read_excel('Data/Data_review.xlsx') %>% 
  dplyr::select(starts_with('Country'), starts_with('Species'), RefLit, Contact) %>% 
  gather(., 'Species', 'SpName', starts_with('Species')) %>%
  drop_na(SpName) %>% 
  rename_group() %>% 
  gather(., 'Country', 'CountryName', starts_with('Country')) %>% 
  drop_na(CountryName) %>% 
  gather(., 'Ref', 'RefName', RefLit, Contact) %>% 
  drop_na()

table <- all %>% dplyr::select(CountryName, SpName, RefName) %>% 
  group_by(CountryName, SpName) %>% 
  arrange(CountryName, SpName) %>% 
  distinct()
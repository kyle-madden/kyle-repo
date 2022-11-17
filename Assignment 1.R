########
#### Report
'
Introduction
Acipenseridae are a family of long-lived fish commonly known as sturgeon, the vast majority of which are threatened or endangered (IUCN, 2022). Though many sturgeon species face extinction, their biology is understudied and their exact distribution is often unclear due to their lack of abundance (Zholdasova, 1997). I examined the exploratory research question, how does sturgeon sampling effort and species richness change across different geographical regions? Additionally, I explored how sturgeon sampling completeness varies across continents. It is interesting to investigate sampling effort and completeness across continents for sturgeon since there are know incongruencies in sturgeon distribution between present day and known historical distribution.
Results
A prudent example of sampling incompleteness I discovered in my analysis was present with Pseudoscaphirhynchus kaufmanni (Amu Darya sturgeon); although this species is endangered, it is known to inhabit Eurasia, yet no samples were present on BOLD (Zholdasova, 1997). The overall sturgeon species richness was highest in Asia with 12 species, followed by 10 in Europe and 7 species sampled in North America. Comparatively finer scale analyses were hindered by the lack of coordinate data availability for most sturgeon species (Fig. 1). Global sturgeon longitudinal distribution is known to be much more diverse than the available coordinate data suggests. Though there is a lack of coordinate data, sturgeon species sampling appears to be complete for North America based on the relationship between BIN richness and samples barcoded (Fig. 2c). Conversely, sampling appears to be slightly less complete in Europe and Asia since the BIN richness continued to increase as the number of samples barcoded increased (Fig. 2a, Fig. 2b). 

Discussion
It is logical that BIN richness continued to increase for the same number of samples barcoded in Europe and Asia relative to North America since there are less species in North America compared to Europe and Asia. Interestingly, since the total BIN richness does not seem to have stabilized as markedly for the eastern continents and we therefore expect more BINs to eventually be added, figure 2 supports the notion that species are missing from the BOLD database which are known to exist in these locations. Since species like sturgeon are not well sampled, it is difficult to draw conclusions about their distribution, or formulate hypotheses for why some species exist across such large longitudinal distances. Longitude and latitude data is occasionally withheld from public databases like BOLD to prevent highly vulnerable species like Amu Darya sturgeon from being at an increased risk of poaching. Even after considering precautions taken to prevent further decline of this species, general data indicating that this species exists anywhere in the European and Asian continent is absent from BOLD. The absence of samples from a species which is currently known to exist in the continent further highlights the incompleteness in sampling for less common, or difficult to sample species. Given my prior experience working with sturgeon bioinformatics data, I am keenly aware of the difficulty in obtaining samples from species in this family. These results therefore align with my personal expectation that data on any sturgeon species is usually difficult to obtain and incomplete. To enhance this analysis in the future, it would be possible to use collector identities, institutions of origin, and literature searches to fill in gaps in country data. Using collector identity data, I would reach out to specific research groups for more specific sturgeon coordinate data. I would then use coordinate data to investigate how sturgeon species richness changes in relation to the human population adjacent to its habitat, to determine whether anthropogenic activity is related to the number of sturgeon species in each area. 

Acknowledgements
I would like to acknowledge the two cited R guides, as well as the course material form which I adapted and created the code to complete this assignment. I did not consult with any peers or others in my analysis.

References
BOLD API for Public Data Portal. https://v3.boldsystems.org/index.php/resources/api?type=webservices. Accessed on [07-10-2022]
Gastner, M. T. 2020, November 1. Chapter 21 Multi-panel Plots. https://michaelgastner.com/R_for_QR/multi-panel-plots.html.
IUCN. 2022. The IUCN Red List of Threatened Species. Version 2022-1. https://www.iucnredlist.org. Accessed on [07-10-2022].
Moreno, M., and M. Basille. 2018, October 25. Drawing beautiful maps programmatically with R, sf and ggplot2 — Part 1: Basics. https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html.
Moreno, M., and M. Basille. 2018, October 25. Drawing beautiful maps programmatically with R, sf and ggplot2 — Part 2: Layers. https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html.
Zholdasova, I. 1997. Sturgeons and the Aral Sea Ecological catastrophe. Sturgeon Biodiversity and Conservation 48:373–380.'

########
#### Set working directory and get packages/libraries

#set wd each time you start a R session
#setwd("C:/Users/Kyle Madden/OneDrive - University of Guelph/!MSc/Courses/BINF 6210/R files/Assignment 1")

#### Created a list and loop to install packages if user does not have
packages = (c("cowplot", "googleway", "ggplot2", "ggrepel", "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata", "tidyverse", "vegan"))
for(element in packages){
  if (!requireNamespace(element, quietly = TRUE))
    install.packages(element)
}

library("tidyverse")
library("vegan")
library("ggplot2")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("ggspatial")
####
library("tmap")
library("cowplot")
data(rivers)

########

#### Get Sturgeon data:
#downloaded all Acipenseridae (Sturgeon) data on October 6 2022
Acipenseridae <- read_tsv("http://www.boldsystems.org/index.php/API_Public/combined?taxon=Acipenseridae&format=tsv")

#write to Assignment 1 folder
write_tsv(Acipenseridae, "Acipenseridae_BOLD_data.tsv")

#create Acipenser variable from hard disk
Acipenser <- read_tsv("Acipenseridae_BOLD_data.tsv")
########
#DATA EXPLORATION

#checking to make sure data types make sense for each variable
summary (Acipenseridae)

#indexing the data for bin_uri, genus_name, species_name, lat, lon, country, province_state, region, genbank_accession, nucleotides
Acip <- Acipenseridae[, c(8, 20, 22, 47, 48, 55, 56, 57, 71, 72)]

#how many samples have been assigned a BIN?
sum(!is.na(Acip$bin_uri))

#filter out those that do not have a BIN  or species name assigned
Acip.bin <- Acip %>%
  filter(!is.na(bin_uri)) %>%
  filter(!is.na(species_name))

#prints T/F whether bin is present to check that previous code worked
!is.na(Acip.bin$bin_uri)

#how many samples have lat data?
sum(!is.na(Acip$lat))

#how many samples have lon data?
sum(!is.na(Acip$lon))

#filter for fish with lat and lon data, and have a BIN assigned
Acip.bin.lat.lon <- Acip.bin %>%
  filter(!is.na(lon)) %>%
  filter(!is.na(lat))

#How does sturgeon sample abundance vary by latitude? by longitude?
hist(Acip.bin.lat.lon$lat)
hist(Acip.bin.lat.lon$lon)

#how many samples have country data? -> this can be used in addition to lon/lat data to examine fish sampling distribution by continent
sum(!is.na(Acip$country))

#how many unique sample countries of origin are there for fish with a BIN assigned?
length(unique(Acip.bin$country))

#check if all fish with lon/lat data have a country listed, so that no fish with this data are excluded when we filter for samples with a country listed
!is.na(Acip.bin.lat.lon$country)

#All good :), now filter out samples that have no country of origin listed from original set of samples where each fish has been assigned a BIN
Acip.bin.country <- Acip.bin %>%
  filter(!is.na(country))

#View all countries
unique(Acip.bin.country$country)

#To determine which samples from Russia go into the Asian vs Europe sturgeon group, I subset all sturgeon samples from Russia into a separate variable and look at the "region" characteristic to see how many unique regions they come from, and display the regions
Acip.russia <- Acip.bin.country %>%
  filter (country == 'Russia') %>%
  filter (!is.na(region))

unique(Acip.russia$region) 

########
#### DATA ANALYSIS:

#### Created a function to simplify the process of separating the data by continent. A list of the countries and regions are formal arguments as well as a set of data that will be filtered based on the inputted lists. region_list is set to a default value of NULL because region will only be used for Russian regions, which can be either in Asia or Europe. For all other continents, only country data will be used. An if statement will be used to determine if regions are included in the filtering.

Regional_filtering_analysis <- function(whole_set, country_list, region_list = NULL){
  lst <- whole_set
  
  if (is.null(region_list) == F){
    lst <- whole_set %>%
      filter(country %in% country_list | region %in% region_list)}
  else{
    lst <- whole_set %>%
      filter(country %in% country_list)}
  # Print the number of unique species and the unique species names in each group to compare species richness of each group. 
  print(length(lst$species_name))
  print(unique(lst$species_name))
  
  # Determine the number if BINs in each group
  View(lst_bin <- lst %>%
    group_by(bin_uri) %>%
    count(bin_uri))
  
  # Transpose the bin data of each continental data set so the BIN IDs become column titles 
  lst_transpose <- pivot_wider(data = lst_bin, names_from = bin_uri, values_from = n)
  
  # Return the transposed product to be used for plot generation
  return(lst_transpose)
}

#### Create county and region lists to be called as arguments for the Regional_filtering_analysis function
NA_countries <- c('Canada', 'United States')
AS_countries <- c('China','Uzbekistan', 'Kazakhstan', 'Turkmenistan', 'Iran')
AS_regions <- c('Ob River', 'Selenga River','Lena River', 'Yenisei River' , 'Amur River')
EU_countries <- c('Austria', 'Azerbaijan', 'Italy', 'Turkey', 'Germany' , 'Czech Republic', 'Hungary', 'France', 'Ukraine', 'Romania' , 'Slovakia' , 'United Kingdom')
EU_regions <- c('Volga River', 'Astrakhan')
AS_EU_countries <- c('China','Uzbekistan', 'Kazakhstan', 'Turkmenistan', 'Iran', 'Austria', 'Azerbaijan', 'Italy', 'Turkey', 'Germany' , 'Czech Republic', 'Hungary', 'France', 'Ukraine', 'Romania' , 'Slovakia' , 'United Kingdom')


#### Call the Regional_filtering_analysis function with created lists
Acip.bin.NA.transpose <- Regional_filtering_analysis(Acip.bin.country, NA_countries)
Acip.bin.AS.transpose <- Regional_filtering_analysis(Acip.bin.country, AS_countries, AS_regions)
Acip.bin.EU.transpose <- Regional_filtering_analysis(Acip.bin.country, EU_countries, EU_regions)
Acip.bin.EUAS.transpose <- Regional_filtering_analysis(Acip.bin.country, AS_EU_countries)

#These species exist in both the EU and Asia groups: "Huso huso", "Huso huso", "Acipenser ruthenus", "Acipenser gueldenstaedtii", "Acipenser nudiventris", "Acipenser stellatus", "Acipenser baerii".

#"Acipenser oxyrinchus" was the only species in common between NA and EU. No species in common between NA and Asia groups.

########
#### PLOTS:

#4 panel Figure comparing the BIN richness - #of samples across the different regions
#https://michaelgastner.com/R_for_QR/multi-panel-plots.html is the tutorial I followed and code adapted to make multi panel figure
par(mfrow = c(3, 1))
ylim <- c(0, 12)
xlim <- c(0, 200)


#### When attempting to plot the bin richness vs bar-coded sample plots, the following error occurred: "Error in plot.new() : figure margins too large"
# To fix this error, the following line of code was added:
par(mar=c(1,1,1,1))


#### RARECURVE PLOTS:
#Plot of BIN richness vs # of individuals barcoded from Asia
bin.rich_vs_barcoded.samples <- rarecurve(Acip.bin.AS.transpose, xlab = "Samples Barcoded", ylab = "BIN Richness", main = "Sturgeon Samples from Asia", ylim = ylim, xlim = xlim)

#Plot of BIN richness vs # of individuals barcoded from Europe
bin.rich_vs_barcoded.samples <- rarecurve(Acip.bin.EU.transpose, xlab = "Samples Barcoded", ylab = "BIN Richness", main = "Sturgeon Samples from Europe", ylim = ylim, xlim = xlim)

#Plot of BIN richness vs # of individuals barcoded from North America
bin.rich_vs_barcoded.samples <- rarecurve(Acip.bin.NA.transpose, xlab = "Samples Barcoded", ylab = "BIN Richness", main = "Sturgeon Samples from North America", ylim = ylim, xlim = xlim)


#Plot of BIN richness vs # of individuals barcoded from Eurasia for comparison
par(mfrow = c(1, 1))
bin.rich_vs_barcoded.samples <- rarecurve(Acip.bin.EUAS.transpose, xlab = "Samples Barcoded", ylab = "BIN Richness", main = "Sturgeon Samples from Eurasia", ylim = ylim, xlim = c(0, 400))


#### Deleted repeated latitude and longitude histograms


#### SAMPLING SITE MAPS:
#MAP CODE adapted from https://r-spatial.org/r/2018/10/25/ggplot2-sf.html
# and from https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
theme_set(theme_light())

#obtain worldmap template
world.map <- ne_countries(scale = "medium", returnclass = "sf")
class(world.map)

#Create sampling site variable with all samples with lon/lat data available
sampling.sites <- data.frame(longitude = Acip.bin.lat.lon$lon, latitude = Acip.bin.lat.lon$lat)


#### Adding a Russian River map figure
# Load Russian map data from file
russia <- st_read("gadm41_RUS.gpkg", "ADM_ADM_3") %>%
  filter(NAME_1 != "Chukot")

# Use 'rivers' data set and filter to include Russian rivers used in Sturgeon data set
river_names <- c("Ob", "Amur", "Lena" , "Yenisey", "Volga", "Selenga")

russian_rivers <- rivers %>%
  filter(name %in% river_names)

# Create river labels using points from russian_rivers
point_s = c(4, 28, 59, 15, 69, 47)
name = russian_rivers$name[point_s]
russian_river_point <- as.data.frame(name) %>%
  add_column(point = (russian_rivers$geometry[point_s]))

# create Russian map with the Russia shape object, river object, and point labels
russian_map <- tm_shape(russia)+
  tm_polygons(col = "grey95", border.col = "grey70")+
  tm_layout(inner.margins = c(0.1), title = "Russian Rivers Sampled", frame = F, title.position = c("left", "top"))+
  tm_shape(russian_rivers) +
  tm_lines("darkblue")+
  tm_shape(st_as_sf(russian_river_point))+
  tm_dots()+
  tm_text("name")

#### turn Russian map into a grob object so it can be plotted next to sampling data plot
r_map <- tmap_grob(russian_map)


#Create map with sampling sites
sampling_site_map <- ggplot(data = world.map) +
  geom_sf() +
  coord_sf(xlim = c(-120, 110), ylim = c(20, 80), expand = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Sturgeon Sampling Locations", ) + 
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_point(data = sampling.sites, aes(x = sampling.sites$lon, y = sampling.sites$lat), size = 3, shape = 21, fill = "darkred")


#### Plotting the sampling site data with the Russian river data
plot_grid(sampling_site_map, r_map)


#Now save the map as jpg in working directory
ggsave("Sturgeon Sampling Locations.jpg", width = 14, height = 7, dpi = "screen")


#######################################################
######              Country Network              ######
#######################################################
  
#######################################################
######           SCOPUS references           ######
######              Figure 5                     ######
######################################################
#############################################################
#####               Countries Contribution              #####
#############################################################
  
# Get city/country data
world_map <- map_data("world")
data(world.cities)
  
# create list of cities
world.cities <- world.cities %>%
  select(name) %>%
  distinct()
  
# create lists for cities containing spaces
world.cities.Narrow <- world.cities %>%
  filter(str_detect(name, " "))
  
world.cities.Narrow$NameEdit <- world.cities.Narrow$name
  
# replace " " in city name with "_"
world.cities.Narrow$NameEdit  <- gsub(" ", "_", world.cities.Narrow$NameEdit)
world.cities$NameEdit <- gsub(" ", "_", world.cities$name)

world.cities <- full_join(world.cities,world.cities.Narrow)

world.cities <- world.cities %>%
  mutate(NameEdit = coalesce(NameEdit,name))

#repeat the same approach but for country names by creating a list of country without space
  
# create list of countries
world.country <- world_map %>%
  select(region) %>%
  distinct()
  
# create lists for countries containing spaces
world.country.Narrow <- world.country %>%
  filter(str_detect(region, " "))
  
world.country.Narrow$regionEdit <- world.country.Narrow$region
# replace " " in country name with "_"
world.country.Narrow$regionEdit  <- gsub(" ", "_", world.country.Narrow$regionEdit)

world.country <- full_join(world.country,world.country.Narrow)

world.country <- world.country %>%
  mutate(regionEdit = coalesce(regionEdit,region))

#replace city name containing space with equivalent
ScopusAffiliationsFig5 <- Scopus_data %>%
  select(Affiliations,Title)
  
for(d in 1:nrow(ScopusAffiliationsFig5)){
  ScopusAffiliationsFig5$Affiliations[d] <- mgsub(ScopusAffiliationsFig5$Affiliations[d],(world.cities.Narrow$name),(world.cities.Narrow$NameEdit))
  print(d)
}

##replace country name containing space with equivalent
for(d in 1:nrow(ScopusAffiliationsFig5)){
  ScopusAffiliationsFig5$Affiliations[d] <- mgsub((ScopusAffiliationsFig5$Affiliations[d]),(world.country.Narrow$region),(world.country.Narrow$regionEdit))
  print(d)
}

# keeping Title as reference for grouping (i.e. relationship between cities), select Affiliations 
AffiliationsNarrow <- ScopusAffiliationsFig5 %>%
  select(Title,Affiliations)
  
# expand using ";" as a delimiter to separate the different institutions found in the same output 
AffiliationsNarrowExtended <- AffiliationsNarrow %>% 
  mutate(Institution = strsplit(as.character(Affiliations), ";"))%>% 
  unnest(Institution) %>%
  mutate_if(is.character, str_trim) %>%
  distinct()
  
# expand again Institution to get further breakdown
AffiliationsNarrowExtDetails <- AffiliationsNarrowExtended %>% 
  mutate(Inst.details = strsplit(as.character(Institution), ","))%>% 
  unnest(Inst.details) %>%
  mutate_if(is.character, str_trim) %>%
  distinct()
  
# subset for country using world.country$regionEdit
AffiliationsCountry <- subset(AffiliationsNarrowExtDetails, (Inst.details %in% world.country$regionEdit))

# expand using space to allow city to be selected
AffiliationsNarrowExtCityDetails <- AffiliationsNarrowExtDetails %>% 
  mutate(Inst.exp = strsplit(as.character(Inst.details), " "))%>% 
  unnest(Inst.exp) %>%
  mutate_if(is.character, str_trim) %>%
  distinct()
  
# subset for cities using world.cities.Narrow$NameEdit
AffiliationsCities <- subset(AffiliationsNarrowExtCityDetails, (AffiliationsNarrowExtCityDetails$Inst.exp %in% world.cities$NameEdit))

# There will be more city names than expected, caused by city names available elsewhere.
# For example "Police" is a city in Poland, "Institut" in Azerbaijan, or "Street" in UK
  
# reassociate cities and country using the title and affiliations
  
# select cities
AffiliationsCitiesNarrow <- AffiliationsCities %>%
  select(Title, Affiliations, Inst.exp) %>%
  distinct(Inst.exp,Title) %>%
  group_by(Title) %>%
  summarise(Inst.exp=paste0(Inst.exp, collapse = ", "))

# select countries
AffiliationsCountryNarrow <- AffiliationsCountry %>%
  select(Title, Affiliations, Inst.details) %>%
  distinct(Inst.details,Title) %>%
  group_by(Title) %>%
  summarise(Inst.details=paste0(Inst.details, collapse = ", "))

# combined the cities and country dataframes
CombinedCityCountry <- full_join(AffiliationsCitiesNarrow,AffiliationsCountryNarrow)
  
# remove the "_" in City 
CombinedCityCountry$Inst.exp <- gsub("_", " ", CombinedCityCountry$Inst.exp)
# and Country columns
CombinedCityCountry$Inst.details <- gsub("_", " ", CombinedCityCountry$Inst.details)

# merge the city and country columns for the dataset
CombinedCityCountry$Label <- paste(CombinedCityCountry$Inst.exp,CombinedCityCountry$Inst.details)
  
# merge the city and country columns fo world city
world.cities$Label <- paste(world.cities$name,world.cities$country.etc)
# world.cities$Label <- toupper(world.cities$Label)
  
# Import longitude and latitude entries from world.cities in the dataset, for the nodes in gephi 
CombinedCityCountry$Lngx <- gsr(as.character(CombinedCityCountry$Label),as.character(world.cities$Label),as.character(world.cities$long))
CombinedCityCountry$Laty <- gsr(as.character(CombinedCityCountry$Label),as.character(world.cities$Label),as.character(world.cities$lat))
  
#exclude all no numerical rows in dataset
# one method is to convert longitude and latitude columns to numeric, introducing NA by coersion
  
CombinedCityCountry$Lngx <- as.numeric(CombinedCityCountry$Lngx)
CombinedCityCountry$Laty <- as.numeric(CombinedCityCountry$Laty)

# remove rows with NA in latitude and longitude
Nodes <- CombinedCityCountry[complete.cases(CombinedCityCountry[ , 5:6]),]
Nodes <- filter(Nodes, Label != "Street UK")

Nodes <- as.data.frame(Nodes)
  
# define continent for each country, useful for colour coding in Gephi
Nodes$Continent <- countrycode(sourcevar = Nodes$Inst.details,
                               origin = "country.name",
                               destination = "continent")
  
GephiNodes <- Nodes %>%
  select(Label, Lngx, Laty, Continent) %>%
  distinct()
GephiNodes$ID <- GephiNodes$Label
  
# export the Nodes table for Gephi import
write.table(GephiNodes,paste0(Results.dir,"GephiNodes.csv"),sep = ",", row.names = F, quote = F)

# institutions regrouped by titles
recombinedDataset <- Nodes %>%
  select(Title, Label) %>%
  dplyr::group_by(Title) %>%
  dplyr::summarise(Affiliations=paste(Label, collapse = ","))
  
#create the adjacency list for Gephi
AdjacencyListGephi <- recombinedDataset %>%
  select(Affiliations)
  
# export the adjacency list for Gephi import
write.table(AdjacencyListGephi,paste0(Results.dir,"AdjacencyList.csv"), row.names = F, quote=F,col.names=FALSE)
  
print("Processing complete. Please check 'Gephi' folders for output and instructions")
  
# For Gephi, import the Adjacency list first, selecting comma instead of space separator.
# In Data Laboratory, in Nodes, Import Spreadsheet
# Select the Nodes files (i.e. GephiNodes.csv), importing as a nodes table, click Next and Finish
# On the next Screen, make sure to select "Append to existing workspace"
# if all went well, all the nodes should then have longitude and latitude
# first include the Geo Layout (install Plugins from Tools if necessary):
#               Scale: 1000.0 (Default)
#               Latitude: laty
#               Longitude: lngx
#               Projection: Equirectangular
#               Center and Looping: unchecked
# Then add Map of Countries (install Plugins from Tools if necessary):
#               Country: World (Default)
#               Subregion: No subregion (Default)
#               Region: No region (Default)
#               District: No district (Default)
#               Scale: 1000.0 (Default)
#               Weight: 1.0 (Default)
#               Projection: Equirectangular
#               Center: unchecked
#######################################################
###### Percentage international collaboration    ######
######              Figure 6                     ######
#######################################################
SE_data <- read.csv(file = "ScienceAndEngineering/ScienceAndEngineering2020.csv")

ScopusCountryCountTotalIndividual <- data.frame(Country = removeDiacritics(unlist(ScopusCountryListUnique)), stringsAsFactors = FALSE)

# define continent for each country
ScopusCountryCountTotalIndividual$Continent <- countrycode(sourcevar = ScopusCountryCountTotalIndividual[, "Country"],
                                      origin = "country.name",
                                      destination = "continent")

# Collate counts for countries over threshold
ScopusCountryCount.Total <- ScopusCountryCountTotalIndividual %>% 
  group_by(Country, Continent) %>% 
  dplyr::summarise(Count = dplyr::n())

names(ScopusCountryCount.Total)[3] <- "TotalCountryCount"

############################################################
#####       repeat for external collaboration         ######
############################################################

ScopusCountryCount <- ScopusCountryListbyPaperUnique

ScopusCountryCount$CountryCount <- str_count(ScopusCountryListbyPaperUnique$Country, ',')
ScopusCountryCount$CountryCount <- ScopusCountryCount$CountryCount + 1

ScopusCountryCount_Ext.Collab. <- ScopusCountryCount %>%
  filter(CountryCount > 1)

ScopusCountryCount_Ext.Collab. <- ScopusCountryCount_Ext.Collab.%>%
  select(Country)

ScopusCountryCount_Ext.Collab.Count <- sapply(ScopusCountryCount_Ext.Collab., strsplit, split = ",", USE.NAMES = FALSE)
ScopusCountryCount_Ext.Collab.Count <- lapply(ScopusCountryCount_Ext.Collab.Count, function(x)unique(x[which(x %in% world.cities$country.etc)]))
ScopusCountryCount_Ext.Collab.Count <- data.frame(Country = removeDiacritics(unlist(ScopusCountryCount_Ext.Collab.Count)), stringsAsFactors = FALSE)

# define continent for each country
ScopusCountryCount_Ext.Collab.Count$Continent <- countrycode(sourcevar = ScopusCountryCount_Ext.Collab.Count[, "Country"],
                                      origin = "country.name",
                                      destination = "continent")

# Collate counts for countries over threshold
ScopusCountryCount_Ext.Collab.Count <- ScopusCountryCount_Ext.Collab.Count %>% 
  group_by(Country, Continent) %>% 
  dplyr::summarise(Count = dplyr::n())

names(ScopusCountryCount_Ext.Collab.Count)[3] <- "ExternalCountryCount"

Country_data.Summary <- full_join(ScopusCountryCount.Total, ScopusCountryCount_Ext.Collab.Count)

Country_data.Summary$ScopusPrecentage <- (Country_data.Summary$ExternalCountryCount/Country_data.Summary$TotalCountryCount)*100

Country_data.Summary <- full_join(Country_data.Summary, SE_data)

## generate plot per country
threshold <- 25 

# Collate counts for countries over threshold
Country_data.Summary <- Country_data.Summary %>% 
  filter(TotalCountryCount > threshold)
# order by count

Country_data.Summary$Country <-  gsub("Serbia","Serbia *",Country_data.Summary$Country)
Country_data.Summary$Country <-  gsub("Romania","Romania *",Country_data.Summary$Country)
Country_data.Summary$Country <-  gsub("Nigeria","Nigeria *",Country_data.Summary$Country)
Country_data.Summary$Country <-  gsub("Hungary","Hungary *",Country_data.Summary$Country)
Country_data.Summary$Country <-  gsub("Czech Republic","Czech Republic *",Country_data.Summary$Country)
Country_data.Summary$Country <-  gsub("Croatia","Croatia *",Country_data.Summary$Country)
Country_data.Summary$Country <-  gsub("Puerto Rico","Puerto Rico *",Country_data.Summary$Country)


Country_data.Summary$Country <- reorder(Country_data.Summary$Country, Country_data.Summary$ScopusPrecentage)
Country_data.Summary$Country <- as.factor(Country_data.Summary$Country)

Country_data.Summary$Science_Eng <- Country_data.Summary$SE_Percentage - Country_data.Summary$ScopusPrecentage

Country_data.Summary.SE <- Country_data.Summary %>%
  select(Country, ScopusPrecentage, Science_Eng) %>%
  gather(source, count, -Country)

# plot
Fig6.1Plot <-  ggplot(Country_data.Summary, (aes(x=Country, y=ScopusPrecentage, fill=Continent))) + 
  geom_col() +
  scale_fill_manual(values = c("gray", brewer.pal(5, "Set1")), breaks = c("Africa", "Americas", "Asia", "Europe", "Oceania")) +
  xlab('Country Affiliation') +
  ylab('Percentage of outputs with international collaboration') +
  coord_flip() +
  theme_minimal() +
  theme(text = element_text(family = "Arial"))

show(Fig6.1Plot)

ggsave(paste0(Figure.dir,"Fig_6.1_ScopusPrecentage_Country_International_Collaboration.png"), Fig6.1Plot, width = 8, height = 6, units = "in", dpi=1200)

# plot as stacked barplot
Fig6.2Plot = ggplot(Country_data.Summary.SE, aes(x = as.factor(Country), y = count, fill = (source))) + 
  geom_col() + 
  coord_flip() +
  #scale_fill_manual(values = c("gray", brewer.pal(5, "Set1")), breaks = c("Africa", "Americas", "Asia", "Europe", "Oceania")) +
  scale_fill_manual(labels = c('Science &\nEngineering [26]','Scopus Search'), values = brewer.pal(3, 'Blues')[1:2]) + 
  #ggtitle('IFSMS Report') +
  xlab('Country Affiliation') +
  ylab('Percentage of outputs with international collaboration') +
  theme_minimal() +
  theme(legend.title=element_blank()) +
  theme(panel.grid.major.y = element_blank(),legend.key.size = unit(1.0, "cm"))

show(Fig6.2Plot)

ggsave(paste0(Figure.dir,"Fig_6.2_ScopusPrecentage_Country_International_Collaboration_S_And_E.tiff"), Fig6.2Plot, width = 8, height = 6, units = "in", dpi=300)

print("Processing complete. Please check 'Results/' and 'Figures/' folders for output")
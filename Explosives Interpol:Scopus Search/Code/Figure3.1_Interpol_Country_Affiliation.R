#######################################################
###### INTERPOL Country Affiliation             ######
######              Figure 3.1                   ######
#######################################################
# Using INTERPOL data, apply corrections to countries

Interpol_data$Affiliations <- gsub("\\.", "", Interpol_data$Affiliations)
Interpol_data$Affiliations <- gsub("USA\\, United States", "USA", Interpol_data$Affiliations)
Interpol_data$Affiliations <- gsub("Russian Federation", "Russia", Interpol_data$Affiliations)
Interpol_data$Affiliations <- gsub("Cairo Egypt", "Cairo\\, Egypt", Interpol_data$Affiliations)
Interpol_data$Affiliations <- gsub("Hong Kong", "Hong Kong\\, China", Interpol_data$Affiliations)
Interpol_data$Affiliations <- gsub("United States", "USA", Interpol_data$Affiliations, perl = TRUE)
Interpol_data$Affiliations <- gsub("United Kingdom", "UK", Interpol_data$Affiliations, perl = TRUE)
Interpol_data$Affiliations <- gsub("South Korea", "Korea South", Interpol_data$Affiliations, perl = TRUE)

# replace ';' with ',' as multiple affiliations are separated with ';'
Interpol_data$Affiliations <- gsub(";", ",", Interpol_data$Affiliations)

# split fields by ", "
InterpolAffiliations <- sapply(Interpol_data$Affiliations, strsplit, split = ", ", USE.NAMES = FALSE)

# extract fields which match a known city making sure that diacritics aren't a problem...
CityList <- lapply(InterpolAffiliations, function(x)x[which(removeDiacritics(x) %in% world.cities$name)])

# ... or country
CountryList <- lapply(InterpolAffiliations, function(x)x[which(removeDiacritics(x) %in% world.cities$country.etc)])

# this version only returns unique instances of countries per publication
InterpolCountryList <- lapply(InterpolAffiliations, function(x)unique(x[which(x %in% world.cities$country.etc)]))

## generate plot of papers per country
#threshold for Figure 
threshold <- 10

InterpolCountryList <- data.frame(Country = removeDiacritics(unlist(InterpolCountryList)), stringsAsFactors = FALSE)

# define continent for each country
InterpolCountryList$Continent <- countrycode(sourcevar = InterpolCountryList[, "Country"],
                                   origin = "country.name",
                                   destination = "continent")

# get countries under threshold
ThresholdCountries <- InterpolCountryList %>% 
  group_by(Country, Continent) %>% 
  dplyr::summarise(Count = dplyr::n()) %>% 
  filter(Count <= threshold)

# aggregate counts as 'Others'
ThresholdCountries <- data.frame(Country = "Others", Continent = "Other", Count = sum(ThresholdCountries$Count))

# order by count
ThresholdCountries$Country <- reorder(ThresholdCountries$Country, ThresholdCountries$Count)
ThresholdCountries <- as.data.frame(ThresholdCountries)

# Collate counts for countries over threshold
InterpolCountryList <- InterpolCountryList %>% 
  group_by(Country, Continent) %>% 
  dplyr::summarise(Count = dplyr::n()) %>%  
  filter(Count > threshold)

# order by count
InterpolCountryList$Country <- reorder(InterpolCountryList$Country, InterpolCountryList$Count)
InterpolCountryList <- as.data.frame(InterpolCountryList)
# add in 'Others'
InterpolCountryList <- rbind(ThresholdCountries, InterpolCountryList)
# plot
InterpolCountryAffiliations <-  ggplot(InterpolCountryList, (aes(x=Country, y=Count, fill=Continent))) + 
  geom_col() +
  scale_fill_manual(values = c("gray", brewer.pal(5, "Set1")), breaks = c("Africa", "Americas", "Asia", "Europe", "Oceania", "Other")) +
  xlab('Country Affiliation') +
  ylab('Total Papers') +
  coord_flip() +
  theme_minimal() +
  theme(text = element_text(family = "Arial"),
        legend.position="right",legend.direction="vertical",
        legend.title=element_text(colour=textcol, size = 8),
        legend.margin=margin(grid::unit(0,"cm")),
        legend.text=element_text(colour=textcol,size=7),
        legend.key.height=grid::unit(0.8,"cm"),
        legend.key.width=grid::unit(0.2,"cm"),
        axis.text.x=element_text(size=6,colour=textcol),
        axis.title = element_text(size = 8),
        axis.text.y=element_text(size=6,vjust=0.2,colour=textcol),
        axis.ticks=element_line(size=0.4),
        plot.background=element_blank(),  # element_rect(fill, colour, size, linetype, color))
        panel.border=element_blank(),
        plot.margin=margin(0.2,0.4,0.1,0.2,"cm"),
        plot.title=element_text(colour=textcol,hjust=0,size=6))+ labs(fill = "Region") # change Continent to region
InterpolCountryAffiliations <- InterpolCountryAffiliations+theme(legend.position = c(0.9, 0.5))

show(InterpolCountryAffiliations)

#save figure
Var1 <- paste0("Fig_3.1_INTERPOL_Country_Affiliation")

ggsave(paste0(Figure.dir,sprintf("%s.tiff",Var1)), InterpolCountryAffiliations, width = 3.3, height = 4.5, units = "in", dpi=300)

print("Processing complete. Please check 'Figures/' folder for output")

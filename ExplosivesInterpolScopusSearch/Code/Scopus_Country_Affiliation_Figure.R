#######################################################
######     Scopus Country Affiliation            ######
######              Figure 3.2                   ######
#######################################################
## generate plot of papers per country

#threshold for Figure 
threshold <- 100

ScopusCountryListUnique <- data.frame(Country = removeDiacritics(unlist(ScopusCountryListUnique)), stringsAsFactors = FALSE)

ScopusCountryListUnique <- ScopusCountryListUnique %>% 
  mutate(Country = strsplit(as.character(Country), ",")) %>% 
  unnest(Country)

# define continent for each country
ScopusCountryListUnique$Continent <- countrycode(sourcevar = ScopusCountryListUnique$Country,
                                                 origin = "country.name",
                                                 destination = "continent")

# get countries under threshold
ThresholdCountries <- ScopusCountryListUnique %>% 
  group_by(Country, Continent) %>% 
  dplyr::summarise(Count = dplyr::n()) %>% 
  filter(Count <= threshold)

# aggregate counts as 'Others'
ThresholdCountries <- data.frame(Country = "Others", Continent = "Other", Count = sum(ThresholdCountries$Count))

# order by count
ThresholdCountries$Country <- reorder(ThresholdCountries$Country, ThresholdCountries$Count)
ThresholdCountries <- as.data.frame(ThresholdCountries)

# Collate counts for countries over threshold
ScopusCountryListUnique <- ScopusCountryListUnique %>% 
  group_by(Country, Continent) %>% 
  dplyr::summarise(Count = dplyr::n()) %>%  
  filter(Count > threshold)

# order by count
ScopusCountryListUnique$Country <- reorder(ScopusCountryListUnique$Country, ScopusCountryListUnique$Count)

# add in 'Others'
ScopusCountryListUnique <- rbind(ThresholdCountries, ScopusCountryListUnique)
ScopusCountryListUnique <- ScopusCountryListUnique %>%
  filter(!is.na(Country))

# plot
ScopusCountryAffiliationsPlot <-  ggplot(ScopusCountryListUnique, (aes(x=Country, y=Count, fill=Continent))) + 
  geom_col() +
  scale_fill_manual(values = c("gray", brewer.pal(5, "Set1")), breaks = c("Africa", "Americas", "Asia", "Europe", "Oceania", "Other")) +
  xlab('Country Affiliation') +
  ylab('Total Papers') +
  coord_flip() +
  theme_minimal() +
  theme(text = element_text(family = "sans"),
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
ScopusCountryAffiliations <- ScopusCountryAffiliationsPlot+theme(legend.position = c(0.9, 0.5))

show(ScopusCountryAffiliations)

#save figure
Var1 <- paste0("Scopus_Country_Affiliation_Figure")

ggsave(paste0(Figure.dir,sprintf("%s.tiff",Var1)), ScopusCountryAffiliationsPlot, width = 3.3, height = 4.5, units = "in", dpi=300)

print("Processing complete. Please check 'Figures/' folder for output")

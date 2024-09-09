#######################################################
#####       Scopus Explosive Keywords           #####
#####              vs Country Plot                #####
#######################################################
# count the number of records with a match to the explosives reference list 
ScopusExplosivesCountryMatches <- ScopusExplosives

# replace blank with NA and remove
ScopusExplosivesCountryMatches[ScopusExplosivesCountryMatches==""] <- NA
ScopusExplosivesCountryMatches <- ScopusExplosivesCountryMatches[complete.cases(ScopusExplosivesCountryMatches[ ,2]),]

############
#Split Column "Country" in row by the separator ",", remove leading white space to generate list
ScopusKeywordIndividualCountries <- ScopusExplosivesCountryMatches %>% 
  mutate(Country = strsplit(as.character(Country), ",")) %>% 
  unnest(Country) %>%
  mutate_if(is.character, str_trim)

#Split Column "Explosive List Corrected" in row by the separator ";", remove leading white space to generate list
ScopusKeywordCountryPairs <- ScopusKeywordIndividualCountries %>% 
  mutate(Explosive_List_Corrected = strsplit(as.character(Explosive_List_Corrected), ";")) %>% 
  unnest(Explosive_List_Corrected) %>%
  mutate_if(is.character, str_trim)

# replace blank with NA and remove
ScopusKeywordCountryPairs[ScopusKeywordCountryPairs==""] <- NA
ScopusKeywordCountryPairs[ScopusKeywordCountryPairs=="NA"] <- NA
ScopusKeywordCountryPairs <- ScopusKeywordCountryPairs[complete.cases(ScopusKeywordCountryPairs[ ,2]),]

ScopusKeywordCountryPairsReduced <- ScopusKeywordCountryPairs %>%
  select(Explosive_List_Corrected,Country)

ScopusKeywordCountryPairCount <- aggregate(ScopusKeywordCountryPairsReduced$Country, by=list(Country=ScopusKeywordCountryPairsReduced$Country, Explosive_List=ScopusKeywordCountryPairsReduced$Explosive_List_Corrected), FUN=length)

# count the number of match to the Explosive_List list per country
ScopusKeywordCountryTotal <- aggregate(ScopusKeywordCountryPairsReduced$Country, by=list(Country=ScopusKeywordCountryPairsReduced$Country), FUN=length)

#select the top 10 countries with the most matching keywords
ScopusKeywordCountryTotal <- top_n(ScopusKeywordCountryTotal,30)

# subset against the top list
ScopusKeywordCountryPairCount <- subset(ScopusKeywordCountryPairCount, Country %in% ScopusKeywordCountryTotal$Country)

##################################################################
# select one of the two possible display for match to Explosive_List #
##################################################################

##### Either combined across all countries together

# This will include the top n countries with the highest number of Explosive_List match. This is different from the country map output as this would include all records regardless of match.
# This part is however bias toward country with the highest number of records as they may focus on specific entries

# count the number of match to the Explosive_List
ScopusKeywordCount <- aggregate(ScopusKeywordCountryPairsReduced$Explosive_List_Corrected, by=list(Explosive_List_Corrected = ScopusKeywordCountryPairsReduced$Explosive_List_Corrected), FUN=length)

#select the top 100 Explosive_List appearing in list
#ScopusKeywordCount <- top_n(ScopusKeywordCount,100)
#write.csv(ScopusKeywordCount, file = "ScopusKeywordCount.csv", row.names = FALSE)

# subset against the top list
ScopusKeywordCountryPairCountExplosiveSubset <- subset(ScopusKeywordCountryPairCount, Explosive_List %in% ScopusKeywordCount$Explosive_List_Corrected)


##### and by considering the top match of each country separately before merging together

# This part instead consider the top n countries and return their individual top n list of explosives reference list. They are then merged together in a list. 
ScopusCountrySubset <- ScopusKeywordCountryTotal$Country
# remove existing dataframe that may have been run on previous option selection
rm(ScopusKeywordCount)

for (i in 1:length(ScopusCountrySubset)){
  
  ScopusKeywordCountryPairsCount_temp <- ScopusKeywordCountryPairsReduced %>%
    filter(Country == ScopusCountrySubset[i])
  
  # count the number of match to the Explosive_List
  ScopusKeywordCountryPairsCount_temp <- aggregate(ScopusKeywordCountryPairsCount_temp$Explosive_List_Corrected, by=list(Explosive_List = ScopusKeywordCountryPairsCount_temp$Explosive_List_Corrected), FUN=length)
  
  #select the top 100 Explosive_List appearing in list
  ScopusKeywordCountryPairsCount_temp <- top_n(ScopusKeywordCountryPairsCount_temp,3)
  ScopusKeywordCountryPairsCount_temp$Country <- ScopusCountrySubset[i]
  ScopusKeywordCountryPairsCount_temp <- ScopusKeywordCountryPairsCount_temp %>%
    select(Country,Explosive_List,x)
  # if the merged dataset doesn't exist, create it
  if (!exists("ScopusKeywordCountryPairsCountSubset")){
    ScopusKeywordCountryPairsCountSubset <- ScopusKeywordCountryPairsCount_temp}
  # if the merged dataset does exist, append to it
  else {ScopusKeywordCountryPairsCountSubset <-rbind(ScopusKeywordCountryPairsCountSubset, ScopusKeywordCountryPairsCount_temp)
  }
}

############################
##### For top keywords #####
############################

names(ScopusKeywordCountryPairCountExplosiveSubset)[3] <- "Frequency"

ScopusKeywordCountryPairsCountSubset <- full_join(ScopusKeywordCountryPairsCountSubset, ScopusKeywordCountryPairCountExplosiveSubset)

ScopusKeywordCountryPairsCountSubset$x[!is.na(ScopusKeywordCountryPairsCountSubset$x)] <- "black"
ScopusKeywordCountryPairsCountSubset$x[is.na(ScopusKeywordCountryPairsCountSubset$x)] <- "white"
names(ScopusKeywordCountryPairsCountSubset)[3] <- "ColourCode"

ScopusKeywordCountryPairsCountSubset$Country <- as.factor(ScopusKeywordCountryPairsCountSubset$Country)
ScopusKeywordCountryPairsCountSubset$Explosive_List <- as.factor(ScopusKeywordCountryPairsCountSubset$Explosive_List)

# ordering keywords in plot by most frequent across all countries
ScopusKeywordOrder <- aggregate(ScopusKeywordCountryPairsCountSubset$Explosive_List, by=list(ScopusKeywordCountryPairsCountSubset$Explosive_List), FUN=length)

ScopusKeywordCountryPairsCountSubset$ScopusKeywordOrder <-  gsr(as.character(ScopusKeywordCountryPairsCountSubset$Explosive_List),as.character(ScopusKeywordOrder$Group.1),ScopusKeywordOrder$x)
ScopusKeywordCountryPairsCountSubset$ScopusKeywordOrder <- as.numeric(ScopusKeywordCountryPairsCountSubset$ScopusKeywordOrder)

# ordering countries by highest number of keywords
CountryOrder <- aggregate(ScopusKeywordCountryPairsCountSubset$Country, by=list(ScopusKeywordCountryPairsCountSubset$Country), FUN=length)

ScopusKeywordCountryPairsCountSubset$CountryOrder <-  gsr(as.character(ScopusKeywordCountryPairsCountSubset$Country),as.character(CountryOrder$Group.1),CountryOrder$x)
ScopusKeywordCountryPairsCountSubset$CountryOrder <- as.numeric(ScopusKeywordCountryPairsCountSubset$CountryOrder)

#############################################################
#####                      GRAPH                        #####
#############################################################

# this is to set the range for the keyword figure
ScopusKeywordCountryPairsCountSubset$groups <- cut(ScopusKeywordCountryPairsCountSubset$Frequency,               # Add group column
                                                     breaks = c(-1, 0, 1, 2, 5, 10, 50, 100, max(ScopusKeywordCountryPairsCountSubset$Frequency,na.rm = T)),
                                                     labels=c("0","1","2","3-5","6-10","11-50","51-100",">100"))

ScopusExplosivesbyCountry_Graph <- ScopusKeywordCountryPairsCountSubset %>%
  # convert state to factor and reverse order of levels
  mutate(KeywordsCorrected=factor(Explosive_List,levels=rev(sort(unique(Explosive_List))))) %>%
  # create a new variable from count
  mutate(countfactor=cut(Frequency,breaks=c(-1, 0, 1, 2, 5, 10, 50, 100, max(Frequency,na.rm = T)),
                         labels=c("0","1","2","3-5","6-10","11-50","51-100",">100")))  %>%
  # change level order
  mutate(countfactor=factor(as.character(countfactor),levels=rev(levels(countfactor))))
# ScopusScopusKeywordCountryPairs$WYear <- gsr(ScopusScopusKeywordCountryPairs$Year,year$Var1,1/year$Freq)

# Change the name back to the their format in ExplosiveProperName
ScopusExplosivesbyCountry_Graph$ExplosiveProperName <- ScopusExplosivesbyCountry_Graph$Explosive_List

ScopusExplosivesbyCountry_Graph$ExplosiveProperName <- gsr(as.character(ScopusExplosivesbyCountry_Graph$ExplosiveProperName),as.character(ExplosiveList$Uncorrected.Explosive),as.character(ExplosiveList$Corrected.Explosive))

ScopusKeywordOrder <- ScopusKeywordOrder %>%
  arrange(desc(x),(Group.1))

ScopusKeywordOrder$Colour <- ScopusKeywordOrder$Group.1
ScopusKeywordOrder$Colour <- gsr(as.character(ScopusKeywordOrder$Colour),as.character(ExplosiveList$Corrected.Explosive),as.character(ExplosiveList$Colour))


# write.csv(ScopusExplosivesbyCountry_Graph,file = "temp.csv",row.names = FALSE)
textcol <- "black"

ScopusExplosivesbyCountryPlot <- ggplot(ScopusExplosivesbyCountry_Graph,aes(x=reorder(Country, CountryOrder),y=reorder(ExplosiveProperName,desc(ScopusKeywordOrder)),fill=countfactor))+
  geom_tile(colour=ScopusExplosivesbyCountry_Graph$ColourCode,width=0.85, height=0.85, size=0.1) + 
  guides(fill=guide_legend(title="Count")) +
  scale_fill_manual(values=c("#d53e4f","#f46d43","#fdae61","#fee08b","#d5ee52","#77c86c","#66afc6","#ddf1da", "#8968CD"),na.value = "grey90") +
  #                           c("#990000","#FF5D00","#FFB900","#FFFF00","#ACFF00","#00CC00","#33FFFF","#008BFF","#0000FF","#8968CD","#551A8B")
  # scale_fill_manual(values=c(pal),na.value = "grey90")+
  theme(text = element_text(family = "Palatino"),
        legend.position="right",legend.direction="vertical",
        legend.title=element_text(colour=textcol),
        legend.margin=margin(grid::unit(0,"cm")),
        legend.text=element_text(colour=textcol,size=8),
        legend.key.height=grid::unit(0.8,"cm"),
        legend.key.width=grid::unit(0.2,"cm"),
        axis.text.y=element_text(size=5, vjust=0.2, colour=ScopusKeywordOrder$Colour),
        axis.ticks=element_line(size=0.4),
        plot.background=element_blank(),  # element_rect(fill, colour, size, linetype, color))
        panel.border=element_blank(),
        plot.margin=margin(0.7,0.4,0.1,0.2,"cm"),
        plot.title=element_text(colour=textcol,hjust=0,size=6),
        axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_text(size=6, angle = 60, hjust=1))

show(ScopusExplosivesbyCountryPlot)

#save figure
Var1 <- paste0("Fig_4.2_Scopus_Explosive_Country")

ggsave(paste0(Figure.dir,sprintf("%s.tiff",Var1)), ScopusExplosivesbyCountryPlot, width = 7.5, height = 8.5, units = "in", dpi=300)

print("Processing complete. Please check 'Figures/' folder for output")

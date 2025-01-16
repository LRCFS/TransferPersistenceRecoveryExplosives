#######################################################
#####       Interpol Explosive Keywords           #####
#####              vs Country Plot                #####
#######################################################
# count the number of records with a match to the explosives reference list 
InterpolExplosivesCountryMatches <- InterpolExplosives

# replace blank with NA and remove
InterpolExplosivesCountryMatches[InterpolExplosivesCountryMatches==""] <- NA
InterpolExplosivesCountryMatches <- InterpolExplosivesCountryMatches[complete.cases(InterpolExplosivesCountryMatches[ ,2]),]

############
#Split Column "Country" in row by the separator ",", remove leading white space to generate list
InterpolKeywordIndividualCountries <- InterpolExplosivesCountryMatches %>% 
  mutate(Country = strsplit(as.character(Country), ",")) %>% 
  unnest(Country) %>%
  mutate_if(is.character, str_trim)

#Split Column "Explosive List Corrected" in row by the separator ";", remove leading white space to generate list
InterpolKeywordCountryPairs <- InterpolKeywordIndividualCountries %>% 
  mutate(Explosive_List_Corrected = strsplit(as.character(Explosive_List_Corrected), ";")) %>% 
  unnest(Explosive_List_Corrected) %>%
  mutate_if(is.character, str_trim)

# replace blank with NA and remove
InterpolKeywordCountryPairs[InterpolKeywordCountryPairs==""] <- NA
InterpolKeywordCountryPairs[InterpolKeywordCountryPairs=="NA"] <- NA
InterpolKeywordCountryPairs <- InterpolKeywordCountryPairs[complete.cases(InterpolKeywordCountryPairs[ ,2]),]

InterpolKeywordCountryPairsReduced <- InterpolKeywordCountryPairs %>%
  select(Explosive_List_Corrected,Country)

InterpolKeywordCountryPairCount <- aggregate(InterpolKeywordCountryPairsReduced$Country, by=list(Country=InterpolKeywordCountryPairsReduced$Country, Explosive_List=InterpolKeywordCountryPairsReduced$Explosive_List_Corrected), FUN=length)

# count the number of match to the Explosive_List list per country
InterpolKeywordCountryTotal <- aggregate(InterpolKeywordCountryPairsReduced$Country, by=list(Country=InterpolKeywordCountryPairsReduced$Country), FUN=length)

#select the top 10 countries with the most matching keywords
InterpolKeywordCountryTotal <- top_n(InterpolKeywordCountryTotal,30)

# subset against the top list
InterpolKeywordCountryPairCount <- subset(InterpolKeywordCountryPairCount, Country %in% InterpolKeywordCountryTotal$Country)

##################################################################
# select one of the two possible display for match to Explosive_List #
##################################################################

##### Either combined across all countries together

# This will include the top n countries with the highest number of Explosive_List match. This is different from the country map output as this would include all records regardless of match.
# This part is however bias toward country with the highest number of records as they may focus on specific entries

# count the number of match to the Explosive_List
InterpolKeywordCount <- aggregate(InterpolKeywordCountryPairsReduced$Explosive_List_Corrected, by=list(Explosive_List_Corrected = InterpolKeywordCountryPairsReduced$Explosive_List_Corrected), FUN=length)

#select the top 100 Explosive_List appearing in list
InterpolKeywordCount <- top_n(InterpolKeywordCount,90)
#write.csv(InterpolKeywordCount, file = "InterpolKeywordCount.csv", row.names = FALSE)

# subset against the top list
InterpolKeywordCountryPairCountExplosiveSubset <- subset(InterpolKeywordCountryPairCount, Explosive_List %in% InterpolKeywordCount$Explosive_List_Corrected)


##### and by considering the top match of each country separately before merging together

# This part instead consider the top n countries and return their individual top n list of explosives reference list. They are then merged together in a list. 
InterpolCountrySubset <- InterpolKeywordCountryTotal$Country
# remove existing dataframe that may have been run on previous option selection
if (exists("InterpolKeywordCountryPairsCountSubset")){
  rm(InterpolKeywordCountryPairsCountSubset)
}
  
for (i in 1:length(InterpolCountrySubset)){
  
  InterpolKeywordCountryPairsCount_temp <- InterpolKeywordCountryPairsReduced %>%
    filter(Country == InterpolCountrySubset[i])
  
  # count the number of match to the Explosive_List
  InterpolKeywordCountryPairsCount_temp <- aggregate(InterpolKeywordCountryPairsCount_temp$Explosive_List_Corrected, by=list(Explosive_List = InterpolKeywordCountryPairsCount_temp$Explosive_List_Corrected), FUN=length)
  
  #select the top 100 Explosive_List appearing in list
  InterpolKeywordCountryPairsCount_temp <- top_n(InterpolKeywordCountryPairsCount_temp,3)
  InterpolKeywordCountryPairsCount_temp$Country <- InterpolCountrySubset[i]
  InterpolKeywordCountryPairsCount_temp <- InterpolKeywordCountryPairsCount_temp %>%
    select(Country,Explosive_List,x)
  # if the merged dataset doesn't exist, create it
  if (!exists("InterpolKeywordCountryPairsCountSubset")){
    InterpolKeywordCountryPairsCountSubset <- InterpolKeywordCountryPairsCount_temp
  # if the merged dataset does exist, append to it
  }else {InterpolKeywordCountryPairsCountSubset <-rbind(InterpolKeywordCountryPairsCountSubset, InterpolKeywordCountryPairsCount_temp)
    }
}
  
############################
##### For top keywords #####
############################

names(InterpolKeywordCountryPairCountExplosiveSubset)[3] <- "Frequency"

InterpolKeywordCountryPairsCountSubset <- full_join(InterpolKeywordCountryPairsCountSubset, InterpolKeywordCountryPairCountExplosiveSubset)

InterpolKeywordCountryPairsCountSubset$x[!is.na(InterpolKeywordCountryPairsCountSubset$x)] <- "black"
InterpolKeywordCountryPairsCountSubset$x[is.na(InterpolKeywordCountryPairsCountSubset$x)] <- "white"
names(InterpolKeywordCountryPairsCountSubset)[3] <- "ColourCode"

InterpolKeywordCountryPairsCountSubset$Country <- as.factor(InterpolKeywordCountryPairsCountSubset$Country)
InterpolKeywordCountryPairsCountSubset$Explosive_List <- as.factor(InterpolKeywordCountryPairsCountSubset$Explosive_List)

# ordering keywords in plot by most frequent across all countries
InterpolKeywordOrder <- aggregate(InterpolKeywordCountryPairsCountSubset$Explosive_List, by=list(InterpolKeywordCountryPairsCountSubset$Explosive_List), FUN=length)

InterpolKeywordCountryPairsCountSubset$InterpolKeywordOrder <-  gsr(as.character(InterpolKeywordCountryPairsCountSubset$Explosive_List),as.character(InterpolKeywordOrder$Group.1),InterpolKeywordOrder$x)
InterpolKeywordCountryPairsCountSubset$InterpolKeywordOrder <- as.numeric(InterpolKeywordCountryPairsCountSubset$InterpolKeywordOrder)

# ordering countries by highest number of keywords
CountryOrder <- aggregate(InterpolKeywordCountryPairsCountSubset$Country, by=list(InterpolKeywordCountryPairsCountSubset$Country), FUN=length)

InterpolKeywordCountryPairsCountSubset$CountryOrder <-  gsr(as.character(InterpolKeywordCountryPairsCountSubset$Country),as.character(CountryOrder$Group.1),CountryOrder$x)
InterpolKeywordCountryPairsCountSubset$CountryOrder <- as.numeric(InterpolKeywordCountryPairsCountSubset$CountryOrder)

#############################################################
#####                      GRAPH                        #####
#############################################################

# this is to set the range for the keyword figure
InterpolKeywordCountryPairsCountSubset$groups <- cut(InterpolKeywordCountryPairsCountSubset$Frequency,               # Add group column
                                                    breaks = c(-1, 0, 1, 2, 5, 10, 50, 100, max(InterpolKeywordCountryPairsCountSubset$Frequency,na.rm = T)),
                                                    labels=c("0","1","2","3-5","6-10","11-50","51-100",">100"))

InterpolExplosivesbyCountry_Graph <- InterpolKeywordCountryPairsCountSubset %>%
  # convert state to factor and reverse order of levels
  mutate(KeywordsCorrected=factor(Explosive_List,levels=rev(sort(unique(Explosive_List))))) %>%
  # create a new variable from count
  mutate(countfactor=cut(Frequency,breaks=c(-1, 0, 1, 2, 5, 10, 50, 100, max(Frequency,na.rm = T)),
                         labels=c("0","1","2","3-5","6-10","11-50","51-100",">100")))  %>%
  # change level order
  mutate(countfactor=factor(as.character(countfactor),levels=rev(levels(countfactor))))
# InterpolInterpolKeywordCountryPairs$WYear <- gsr(InterpolInterpolKeywordCountryPairs$Year,year$Var1,1/year$Freq)

# Change the name back to the their format in ExplosiveProperName
InterpolExplosivesbyCountry_Graph$ExplosiveProperName <- InterpolExplosivesbyCountry_Graph$Explosive_List

InterpolExplosivesbyCountry_Graph$ExplosiveProperName <- gsr(as.character(InterpolExplosivesbyCountry_Graph$ExplosiveProperName),as.character(ExplosiveList$Uncorrected.Explosive),as.character(ExplosiveList$Corrected.Explosive))

InterpolKeywordOrder <- InterpolKeywordOrder %>%
  arrange(desc(x),(Group.1))

InterpolKeywordOrder$Colour <- InterpolKeywordOrder$Group.1
InterpolKeywordOrder$Colour <- gsr(as.character(InterpolKeywordOrder$Colour),as.character(ExplosiveList$Corrected.Explosive),as.character(ExplosiveList$Colour))

# write.csv(InterpolExplosivesbyCountry_Graph,file = "temp.csv",row.names = FALSE)
textcol <- "black"

InterpolExplosivesbyCountryPlot <- ggplot(InterpolExplosivesbyCountry_Graph,aes(x=reorder(Country, CountryOrder),y=reorder(ExplosiveProperName,desc(InterpolKeywordOrder)),fill=countfactor))+
  geom_tile(colour=InterpolExplosivesbyCountry_Graph$ColourCode,width=0.85, height=0.85, size=0.1) + 
  guides(fill=guide_legend(title="Count")) +
  scale_fill_manual(values=c("#d53e4f","#f46d43","#fdae61","#fee08b","#d5ee52","#77c86c","#66afc6","#ddf1da", "#8968CD"),na.value = "grey90") +
  #                           c("#990000","#FF5D00","#FFB900","#FFFF00","#ACFF00","#00CC00","#33FFFF","#008BFF","#0000FF","#8968CD","#551A8B")
  # scale_fill_manual(values=c(pal),na.value = "grey90")+
  theme(text = element_text(family = "sans"),
        legend.position="right",legend.direction="vertical",
        legend.title=element_text(colour=textcol),
        legend.margin=margin(grid::unit(0,"cm")),
        legend.text=element_text(colour=textcol,size=8),
        legend.key.height=grid::unit(0.8,"cm"),
        legend.key.width=grid::unit(0.2,"cm"),
        axis.text.y=element_text(size=6, vjust=0.2, colour=InterpolKeywordOrder$Colour),
        axis.ticks=element_line(size=0.4),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.margin=margin(0.7,0.4,0.1,0.2,"cm"),
        plot.title=element_text(colour=textcol,hjust=0,size=6),
        axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_text(size=6, angle = 60, hjust=1))

show(InterpolExplosivesbyCountryPlot)

#save figure
Var1 <- paste0("Fig_3_Interpol_Explosive_Country")

ggsave(paste0(Figure.dir,sprintf("%s.tiff",Var1)), InterpolExplosivesbyCountryPlot, width = 8, height = 9.5, units = "in", dpi=300)

print("Processing complete. Please check 'Figures/' folder for output")

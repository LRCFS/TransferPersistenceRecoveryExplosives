#######################################################
#####       Interpol Sampling Keywords            #####
#####              vs Country Plot                #####
#######################################################
# count the number of records with a match to the explosives reference list 
InterpolSamplingCountryMatches <- InterpolSampling


# replace blank with NA and remove
InterpolSamplingCountryMatches[InterpolSamplingCountryMatches==""] <- NA
InterpolSamplingCountryMatches <- InterpolSamplingCountryMatches[complete.cases(InterpolSamplingCountryMatches[ ,2]),]

############
#Split Column "Country" in row by the separator ",", remove leading white space to generate list
InterpolSamplingIndividualCountries <- InterpolSamplingCountryMatches %>% 
  mutate(Country = strsplit(as.character(Country), ",")) %>% 
  unnest(Country) %>%
  mutate_if(is.character, str_trim)

#Split Column "Explosive List Corrected" in row by the separator ";", remove leading white space to generate list
InterpolSamplingCountryPairs <- InterpolSamplingIndividualCountries %>% 
  mutate(Sampling_List_Corrected = strsplit(as.character(Sampling_List_Corrected), ";")) %>% 
  unnest(Sampling_List_Corrected) %>%
  mutate_if(is.character, str_trim)

# replace blank with NA and remove
InterpolSamplingCountryPairs[InterpolSamplingCountryPairs==""] <- NA
InterpolSamplingCountryPairs[InterpolSamplingCountryPairs=="NA"] <- NA
InterpolSamplingCountryPairs <- InterpolSamplingCountryPairs[complete.cases(InterpolSamplingCountryPairs[ ,2]),]

InterpolSamplingCountryPairsReduced <- InterpolSamplingCountryPairs %>%
  select(Sampling_List_Corrected,Country)

InterpolSamplingCountryPairCount <- aggregate(InterpolSamplingCountryPairsReduced$Country, by=list(Country=InterpolSamplingCountryPairsReduced$Country, Sampling_List_Corrected=InterpolSamplingCountryPairsReduced$Sampling_List_Corrected), FUN=length)

# count the number of match to the Sampling_List_Corrected list per country
InterpolSamplingCountryTotal <- aggregate(InterpolSamplingCountryPairsReduced$Country, by=list(Country=InterpolSamplingCountryPairsReduced$Country), FUN=length)

#select the top 30 countries with the most matching keywords
InterpolSamplingCountryTotal <- top_n(InterpolSamplingCountryTotal,30)

# subset against the top list
InterpolSamplingCountryPairCount <- subset(InterpolSamplingCountryPairCount, Country %in% InterpolSamplingCountryTotal$Country)

##################################################################
# select one of the two possible display for match to Sampling_List_Corrected #
##################################################################

##### Either combined across all countries together

# This will include the top n countries with the highest number of Sampling_List_Corrected match. This is different from the country map output as this would include all records regardless of match.
# This part is however bias toward country with the highest number of records as they may focus on specific entries

# count the number of match to the Sampling_List_Corrected
InterpolSamplingCount <- aggregate(InterpolSamplingCountryPairsReduced$Sampling_List_Corrected, by=list(Sampling_List_Corrected = InterpolSamplingCountryPairsReduced$Sampling_List_Corrected), FUN=length)

#select the top 100 Sampling_List_Corrected appearing in list
#InterpolSamplingCount <- top_n(InterpolSamplingCount,90)
#write.csv(InterpolSamplingCount, file = "InterpolSamplingCount.csv", row.names = FALSE)

# subset against the top list
InterpolSamplingCountryPairCountKeywordSubset <- subset(InterpolSamplingCountryPairCount, Sampling_List_Corrected %in% InterpolSamplingCount$Sampling_List_Corrected)


##### and by considering the top match of each country separately before merging together

# This part instead consider the top n countries and return their individual top n list of explosives reference list. They are then merged together in a list. 
InterpolSamplingCountrySubset <- InterpolSamplingCountryTotal$Country
# remove existing dataframe that may have been run on previous option selection
if (exists("InterpolSamplingCountryPairsCountSubset")){
  rm(InterpolSamplingCountryPairsCountSubset)
}

for (i in 1:length(InterpolSamplingCountrySubset)){
  
  InterpolSamplingCountryPairsCount_temp <- InterpolSamplingCountryPairsReduced %>%
    filter(Country == InterpolSamplingCountrySubset[i])
  
  # count the number of match to the Sampling_List_Corrected
  InterpolSamplingCountryPairsCount_temp <- aggregate(InterpolSamplingCountryPairsCount_temp$Sampling_List_Corrected, by=list(Sampling_List_Corrected = InterpolSamplingCountryPairsCount_temp$Sampling_List_Corrected), FUN=length)
  
  #select the top 100 Sampling_List_Corrected appearing in list
  InterpolSamplingCountryPairsCount_temp <- top_n(InterpolSamplingCountryPairsCount_temp,3)
  InterpolSamplingCountryPairsCount_temp$Country <- InterpolSamplingCountrySubset[i]
  InterpolSamplingCountryPairsCount_temp <- InterpolSamplingCountryPairsCount_temp %>%
    select(Country,Sampling_List_Corrected,x)
  # if the merged dataset doesn't exist, create it
  if (!exists("InterpolSamplingCountryPairsCountSubset")){
    InterpolSamplingCountryPairsCountSubset <- InterpolSamplingCountryPairsCount_temp
    # if the merged dataset does exist, append to it
  }else {InterpolSamplingCountryPairsCountSubset <-rbind(InterpolSamplingCountryPairsCountSubset, InterpolSamplingCountryPairsCount_temp)
  }
}

############################
##### For top keywords #####
############################

names(InterpolSamplingCountryPairCountKeywordSubset)[3] <- "Frequency"

InterpolSamplingCountryPairsCountSubset <- full_join(InterpolSamplingCountryPairsCountSubset, InterpolSamplingCountryPairCountKeywordSubset)

InterpolSamplingCountryPairsCountSubset$x[!is.na(InterpolSamplingCountryPairsCountSubset$x)] <- "black"
InterpolSamplingCountryPairsCountSubset$x[is.na(InterpolSamplingCountryPairsCountSubset$x)] <- "white"
names(InterpolSamplingCountryPairsCountSubset)[3] <- "ColourCode"

InterpolSamplingCountryPairsCountSubset$Country <- as.factor(InterpolSamplingCountryPairsCountSubset$Country)
InterpolSamplingCountryPairsCountSubset$Sampling_List_Corrected <- as.factor(InterpolSamplingCountryPairsCountSubset$Sampling_List_Corrected)

# ordering keywords in plot by most frequent across all countries
InterpolKeywordOrder <- aggregate(InterpolSamplingCountryPairsCountSubset$Sampling_List_Corrected, by=list(InterpolSamplingCountryPairsCountSubset$Sampling_List_Corrected), FUN=length)

InterpolSamplingCountryPairsCountSubset$InterpolKeywordOrder <-  gsr(as.character(InterpolSamplingCountryPairsCountSubset$Sampling_List_Corrected),as.character(InterpolKeywordOrder$Group.1),InterpolKeywordOrder$x)
InterpolSamplingCountryPairsCountSubset$InterpolKeywordOrder <- as.numeric(InterpolSamplingCountryPairsCountSubset$InterpolKeywordOrder)

# ordering countries by highest number of keywords
CountryOrder <- aggregate(InterpolSamplingCountryPairsCountSubset$Country, by=list(InterpolSamplingCountryPairsCountSubset$Country), FUN=length)

InterpolSamplingCountryPairsCountSubset$CountryOrder <-  gsr(as.character(InterpolSamplingCountryPairsCountSubset$Country),as.character(CountryOrder$Group.1),CountryOrder$x)
InterpolSamplingCountryPairsCountSubset$CountryOrder <- as.numeric(InterpolSamplingCountryPairsCountSubset$CountryOrder)

#############################################################
#####                      GRAPH                        #####
#############################################################

# this is to set the range for the keyword figure
InterpolSamplingCountryPairsCountSubset$groups <- cut(InterpolSamplingCountryPairsCountSubset$Frequency,               # Add group column
                                                     breaks = c(-1, 0, 1, 2, 5, 10, 50, 100, max(InterpolSamplingCountryPairsCountSubset$Frequency,na.rm = T)),
                                                     labels=c("0","1","2","3-5","6-10","11-50","51-100",">100"))

InterpolSamplingbyCountry_Graph <- InterpolSamplingCountryPairsCountSubset %>%
  # convert state to factor and reverse order of levels
  mutate(KeywordsCorrected=factor(Sampling_List_Corrected,levels=rev(sort(unique(Sampling_List_Corrected))))) %>%
  # create a new variable from count
  mutate(countfactor=cut(Frequency,breaks=c(-1, 0, 1, 2, 5, 10, 50, 100, max(Frequency,na.rm = T)),
                         labels=c("0","1","2","3-5","6-10","11-50","51-100",">100")))  %>%
  # change level order
  mutate(countfactor=factor(as.character(countfactor),levels=rev(levels(countfactor))))
# InterpolInterpolSamplingCountryPairs$WYear <- gsr(InterpolInterpolSamplingCountryPairs$Year,year$Var1,1/year$Freq)

InterpolKeywordOrder <- InterpolKeywordOrder %>%
  arrange(desc(x),(Group.1))

InterpolKeywordOrder$Colour <- InterpolKeywordOrder$Group.1
InterpolKeywordOrder$Colour <- gsr(as.character(InterpolKeywordOrder$Colour),as.character(ExplosiveList$Corrected.Explosive),as.character(ExplosiveList$Colour))

# write.csv(InterpolSamplingbyCountry_Graph,file = "temp.csv",row.names = FALSE)
textcol <- "black"

InterpolSamplingbyCountryPlot <- ggplot(InterpolSamplingbyCountry_Graph,aes(x=reorder(Country, CountryOrder),y=reorder(Sampling_List_Corrected,desc(InterpolKeywordOrder)),fill=countfactor))+
  geom_tile(colour=InterpolSamplingbyCountry_Graph$ColourCode,width=0.85, height=0.85, size=0.1) + 
  guides(fill=guide_legend(title="Count")) +
  scale_fill_manual(values=c("#d53e4f","#f46d43","#fdae61","#fee08b","#d5ee52","#77c86c","#66afc6","#ddf1da", "#8968CD"),na.value = "grey90") +
  theme(text = element_text(family = "sans"),
        legend.position="right",legend.direction="vertical",
        legend.title=element_text(colour=textcol),
        legend.margin=margin(grid::unit(0,"cm")),
        legend.text=element_text(colour=textcol,size=8),
        legend.key.height=grid::unit(0.8,"cm"),
        legend.key.width=grid::unit(0.2,"cm"),
        axis.text.y=element_text(size=6, vjust=0.2, colour="black"),
        axis.ticks=element_line(size=0.4),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.margin=margin(0.7,0.4,0.1,0.2,"cm"),
        plot.title=element_text(colour=textcol,hjust=0,size=6),
        axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_text(size=6, angle = 60, hjust=1))

show(InterpolSamplingbyCountryPlot)

#######################################################
#####       Interpol Explosive Keywords           #####
#####              vs Year    Plot                #####
#######################################################
#Split Column "Country" in row by the separator ",", remove leading white space to generate list
InterpolSamplingbyYear <- InterpolSampling %>% 
  mutate(Sampling_List_Corrected = strsplit(as.character(Sampling_List_Corrected), ";")) %>% 
  unnest(Sampling_List_Corrected) %>%
  mutate_if(is.character, str_trim)

InterpolSamplingbyYear <- InterpolSamplingbyYear %>%
  filter(between(Year,2001,2022)) 

# replace blank with NA
InterpolSamplingbyYear[InterpolSamplingbyYear=="NA"] <- NA
InterpolSamplingbyYear [InterpolSamplingbyYear==""] <- NA
# remove NA in Sampling_List_Corrected
InterpolSamplingbyYear <- InterpolSamplingbyYear[complete.cases(InterpolSamplingbyYear[ ,2]),]

InterpolSamplingbyYearCount <- aggregate(InterpolSamplingbyYear$Year, by=list(Year=InterpolSamplingbyYear$Year, Sampling_List_Corrected=InterpolSamplingbyYear$Sampling_List_Corrected), FUN=length)

# count the number of match to the Sampling_List_Corrected list
InterpolSamplingTotal <- InterpolSamplingbyYearCount %>%
  group_by(Sampling_List_Corrected) %>%
  summarise(x = sum(x))

# count the total number of match to the Sampling_List_Corrected list per year
InterpolSamplingYearTotal <- aggregate(InterpolSamplingbyYear$Year, by=list(InterpolSamplingbyYear$Year), FUN=length)
names(InterpolSamplingYearTotal)[1] <- c("Year")
names(InterpolSamplingYearTotal)[2] <- c("Total")

##### Count total number of papers per year ####
InterpolPapersbyYear <- Interpol_data %>%
  select(Year) %>%
  filter(between(Year,2001,2022))

InterpolPapersbyYear <- aggregate(InterpolPapersbyYear, by=list(Year=InterpolPapersbyYear$Year), FUN=length)
names(InterpolPapersbyYear)[2] <- c("Count")

names(InterpolSamplingTotal)[1] <- c("Sampling_List_Corrected")

# subset against the top list
InterpolSamplingbyYearCount <- subset(InterpolSamplingbyYearCount, InterpolSamplingbyYearCount$Sampling_List_Corrected %in% InterpolSamplingTotal$Sampling_List_Corrected)

# this is to set the range for the keyword figure
range <- as.numeric(max(InterpolSamplingbyYearCount$x, na.rm = TRUE))

source("Functions/KeywordRange.R")

#############################################################
#####                      GRAPH                        #####
#############################################################

# Create a new variable from incidence (breaks to be changed to fit Interpol vs. Interpol data)
#Breaks and labels for Interpol

InterpolSamplingbyYearCountGraph <- InterpolSamplingbyYearCount

InterpolSamplingbyYearCountGraph$YearTotal <- InterpolSamplingbyYearCountGraph$Year
InterpolSamplingbyYearCountGraph$YearTotal <- gsr(as.character(InterpolSamplingbyYearCountGraph$YearTotal),as.character(InterpolSamplingYearTotal$Year),as.character(InterpolSamplingYearTotal$Total))

names(InterpolSamplingbyYearCountGraph)[3] <- c("Count")
names(InterpolSamplingbyYearCountGraph)[4] <- c("YearTotal")
InterpolSamplingbyYearCountGraph$Percentage <- as.numeric(InterpolSamplingbyYearCountGraph$Count)/as.numeric(InterpolSamplingbyYearCountGraph$YearTotal)*100
InterpolSamplingbyYearCountGraph$YearTotal <- InterpolSamplingbyYearCountGraph$Year
InterpolSamplingbyYearCountGraph$YearTotal <- gsr(as.character(InterpolSamplingbyYearCountGraph$YearTotal),as.character(InterpolPapersbyYear$Year),as.character(InterpolPapersbyYear$Count))

InterpolSamplingbyYearCountGraph$YearTotal <-as.numeric(InterpolSamplingbyYearCountGraph$YearTotal)

# InterpolSamplingbyYearCountGraph$Incidenceweight <- cut(InterpolSamplingbyYearCount$x,
#      breaks = c(BreakRange,max(InterpolSamplingbyYearCount$x,na.rm=T)),
#     labels=DatasetRange)

#InterpolSamplingbyYearCountGraph <- InterpolSamplingbyYearCountGraph %>%
# convert state to factor and reverse order of levels
#mutate(Sampling_List_Corrected=factor(Sampling_List_Corrected,levels=rev(sort(unique(Sampling_List_Corrected))))) %>%
# create a new variable from count
#mutate(countfactor=cut(x,breaks=c(BreakRange,max(x,na.rm=T)),
#   labels=DatasetRange))  %>%

# change level order
# mutate(countfactor=factor(as.character(countfactor),levels=rev(levels(countfactor))))
InterpolSamplingbyYearCountGraphReduced <- aggregate(InterpolSamplingbyYearCountGraph[, 1], list(InterpolSamplingbyYearCountGraph$Sampling_List_Corrected), min)

InterpolSamplingbyYearCountGraph$graphorder <- as.numeric(gsr(InterpolSamplingbyYearCountGraph$Sampling_List_Corrected,InterpolSamplingbyYearCountGraphReduced$Group.1,InterpolSamplingbyYearCountGraphReduced$x))

# further modified ggplot
InterpolTopExplosivesPlot <- ggplot(InterpolSamplingbyYearCountGraph,aes(x=Year))+
  geom_line(aes(y=Count, color=Sampling_List_Corrected))+
  labs(x="Year",y="Number of Mentions")+
  scale_colour_manual(labels = c("Collection", "Headspace", "Recovery", "Sampling", "Swab", "Vacuum Collection", "Vapor Collection", "Vapour Sampling"), name = "Sampling Terms", values = c("#FF5D00","#FFB900","#00CC00","#008BFF","#8968CD","pink","green","yellow" ) )+
  scale_y_continuous()+
  scale_x_continuous(breaks=c(2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2020,2022))+
  #scale_fill_manual(values = c("#FF5D00","black","#00CC00","#008BFF","#8968CD"))+
theme_grey(base_size=8)+
  theme(text = element_text(family = "sans"),
        legend.position="right",legend.direction="vertical",
        legend.margin=margin(grid::unit(0,"cm")),
        legend.text=element_text(colour=textcol,size=10),
        legend.key.height=grid::unit(1.2,"cm"),
        legend.key.width=grid::unit(0.8,"cm"),
        axis.text.x=element_text(size=7,colour=textcol),
        axis.text.y=element_text(vjust=0.2,colour=textcol),
        axis.ticks=element_line(size=0.4),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.margin=margin(-0.2,0.4,0.1,0.2,"cm"),
        plot.title=element_text(colour=textcol,hjust=0,size=12))

#save figure
Var1 <- paste0("Fig_Interpol_Sampling_By_Year")
show(InterpolTopExplosivesPlot) 
ggsave(paste0(Figure.dir,sprintf("%s.tiff",Var1)), InterpolTopExplosivesPlot, width = 8, height = 8.5, units = "in", dpi=300)

#Export to top keywords list
write.csv(InterpolSamplingbyYearCountGraph, file=paste0(Results.dir,sprintf("%s.csv",Var1)), row.names = F)

print("Processing complete. Please check 'Results/' and 'Figures/' folders for output")


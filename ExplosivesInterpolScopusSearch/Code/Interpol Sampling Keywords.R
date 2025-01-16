#######################################################
#####       Interpol Explosive Keywords           #####
#######################################################
#Load keywords
SamplingList <- read.csv("SamplingDatabase.csv", header = TRUE)

SamplingListString <- SamplingList$Keyword

#######################################################
#######                For abstract               #####
#######################################################
# Check for presence of each word in Abstract
InterpolAbs <- vector("list", nrow(Interpol_data))

for(d in 1:nrow(Interpol_data)){
  for(w in seq_along(SamplingListString)){
    intermed   <- grep(SamplingListString[[w]], Interpol_data[[d,"Abstract"]])
    InterpolAbs[[d]] <- c(InterpolAbs[[d]], 
                          SamplingListString[[w]][ (length(intermed) > 0) ])
  }
}

# combined together
InterpolSamplingFromAbs <- sapply(InterpolAbs, paste0, collapse=";")
InterpolSamplingFromAbs <- as.data.frame(InterpolSamplingFromAbs)

InterpolSamplingFromAbs <- data.frame(Interpol_data$Title,InterpolSamplingFromAbs)

# remane fist column to match original dataset 
names(InterpolSamplingFromAbs)[1] <- c("Title")

# #########################################################
# #####                For title                      #####
##############################################
# Check for presence of each word in Title
InterpolTitles <- vector("list", nrow(Interpol_data))

for(d in 1:nrow(Interpol_data)){
  for(w in seq_along(SamplingListString)){
    intermed   <- grep(SamplingListString[[w]], Interpol_data[[d,"Title"]])
    InterpolTitles[[d]] <- c(InterpolTitles[[d]], 
                             SamplingListString[[w]][ (length(intermed) > 0) ])
  }
}

# combined together
InterpolSamplingFromTitle    <- sapply(InterpolTitles, paste0, collapse=";")
InterpolSamplingFromTitle <- as.data.frame(InterpolSamplingFromTitle)

InterpolSamplingFromTitle <- data.frame(Interpol_data$Title,InterpolSamplingFromTitle)

# remane fist column to match original dataset 
names(InterpolSamplingFromTitle)[1] <- c("Title")

InterpolSamplingFromAbsTitle <- full_join(InterpolSamplingFromAbs,InterpolSamplingFromTitle)

# #########################################################
# #####                For keywords                   #####
##############################################
# Check for presence of each word in Keywords
InterpolKeywords <- vector("list", nrow(Interpol_data))

for(d in 1:nrow(Interpol_data)){
  for(w in seq_along(SamplingListString)){
    intermed   <- grep(SamplingListString[[w]], Interpol_data[[d,"KeywordsCorrected"]])
    InterpolKeywords[[d]] <- c(InterpolKeywords[[d]], 
                               SamplingListString[[w]][ (length(intermed) > 0) ])
  }
}

# combined together
InterpolSamplingFromKeyword    <- sapply(InterpolKeywords, paste0, collapse=";")
InterpolSamplingFromKeyword <- as.data.frame(InterpolSamplingFromKeyword)

InterpolSamplingFromKeyword <- data.frame(Interpol_data$Title,InterpolSamplingFromKeyword)

# remane fist column to match original dataset 
names(InterpolSamplingFromKeyword)[1] <- c("Title")

InterpolSampling <- full_join(InterpolSamplingFromAbsTitle,InterpolSamplingFromKeyword)
#write.csv(InterpolSampling,file = "InterpolSampling.csv",row.names = TRUE)

############
# Combine Columns Abs,Titles,Keywords and place in Column name "Sampling_List" and remove original columns
InterpolSampling <- InterpolSampling %>%
  unite("Sampling_List", InterpolSamplingFromAbs, InterpolSamplingFromTitle, InterpolSamplingFromKeyword,sep = ";", remove = TRUE)

#Split Column "Sampling_List" in row by the separator ";", remove leading white space to generate list
InterpolSampling <- InterpolSampling %>% 
  mutate(Sampling_List = strsplit(as.character(Sampling_List), ";")) %>% 
  unnest(Sampling_List) %>%
  mutate_if(is.character, str_trim)

# Remove duplicates from explosive list
InterpolSampling <- InterpolSampling %>%
  select(Title,Sampling_List)%>%
  distinct()

#Group keywords together
InterpolSamplingCollapsed <- InterpolSampling %>% 
  group_by(Title) %>%
  summarize(Sampling_List=paste0(Sampling_List, collapse = ";"))

############
# get the Title and respective country names to join to the InterpolSampling
Interpol_Title_Country <- Interpol_data %>%
  select(Title,Year,Country)

InterpolSampling <- full_join(InterpolSamplingCollapsed,Interpol_Title_Country, by = "Title")

InterpolSamplingPapers <- InterpolSampling
InterpolSamplingPapers[InterpolSamplingPapers==""] <- NA
InterpolSamplingPapers <- InterpolSamplingPapers[complete.cases(InterpolSamplingPapers[ ,2]),]


print('Interpol Sampling Keywords Extracted')





#######################################################
#####       Interpol Explosive Keywords           #####
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
  mutate(Sampling_List = strsplit(as.character(Sampling_List), ";")) %>% 
  unnest(Sampling_List) %>%
  mutate_if(is.character, str_trim)

# replace blank with NA and remove
InterpolSamplingCountryPairs[InterpolSamplingCountryPairs==""] <- NA
InterpolSamplingCountryPairs[InterpolSamplingCountryPairs=="NA"] <- NA
InterpolSamplingCountryPairs <- InterpolSamplingCountryPairs[complete.cases(InterpolSamplingCountryPairs[ ,2]),]

InterpolSamplingCountryPairsReduced <- InterpolSamplingCountryPairs %>%
  select(Sampling_List,Country)

InterpolSamplingCountryPairCount <- aggregate(InterpolSamplingCountryPairsReduced$Country, by=list(Country=InterpolSamplingCountryPairsReduced$Country, Sampling_List=InterpolSamplingCountryPairsReduced$Sampling_List), FUN=length)

# count the number of match to the Sampling_List list per country
InterpolSamplingCountryTotal <- aggregate(InterpolSamplingCountryPairsReduced$Country, by=list(Country=InterpolSamplingCountryPairsReduced$Country), FUN=length)

#select the top 10 countries with the most matching keywords
InterpolSamplingCountryTotal <- top_n(InterpolSamplingCountryTotal,30)

# subset against the top list
InterpolSamplingCountryPairCount <- subset(InterpolSamplingCountryPairCount, Country %in% InterpolSamplingCountryTotal$Country)

##################################################################
# select one of the two possible display for match to Sampling_List #
##################################################################

##### Either combined across all countries together

# This will include the top n countries with the highest number of Sampling_List match. This is different from the country map output as this would include all records regardless of match.
# This part is however bias toward country with the highest number of records as they may focus on specific entries

# count the number of match to the Sampling_List
InterpolSamplingCount <- aggregate(InterpolSamplingCountryPairsReduced$Sampling_List, by=list(Sampling_List = InterpolSamplingCountryPairsReduced$Sampling_List), FUN=length)

#select the top 100 Sampling_List appearing in list
InterpolSamplingCount <- top_n(InterpolSamplingCount,90)
#write.csv(InterpolSamplingCount, file = "InterpolSamplingCount.csv", row.names = FALSE)

# subset against the top list
InterpolSamplingCountryPairCountKeywordSubset <- subset(InterpolSamplingCountryPairCount, Sampling_List %in% InterpolSamplingCount$Sampling_List)


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
  
  # count the number of match to the Sampling_List
  InterpolSamplingCountryPairsCount_temp <- aggregate(InterpolSamplingCountryPairsCount_temp$Sampling_List, by=list(Sampling_List = InterpolSamplingCountryPairsCount_temp$Sampling_List), FUN=length)
  
  #select the top 100 Sampling_List appearing in list
  InterpolSamplingCountryPairsCount_temp <- top_n(InterpolSamplingCountryPairsCount_temp,3)
  InterpolSamplingCountryPairsCount_temp$Country <- InterpolSamplingCountrySubset[i]
  InterpolSamplingCountryPairsCount_temp <- InterpolSamplingCountryPairsCount_temp %>%
    select(Country,Sampling_List,x)
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
InterpolSamplingCountryPairsCountSubset$Sampling_List <- as.factor(InterpolSamplingCountryPairsCountSubset$Sampling_List)

# ordering keywords in plot by most frequent across all countries
InterpolKeywordOrder <- aggregate(InterpolSamplingCountryPairsCountSubset$Sampling_List, by=list(InterpolSamplingCountryPairsCountSubset$Sampling_List), FUN=length)

InterpolSamplingCountryPairsCountSubset$InterpolKeywordOrder <-  gsr(as.character(InterpolSamplingCountryPairsCountSubset$Sampling_List),as.character(InterpolKeywordOrder$Group.1),InterpolKeywordOrder$x)
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
  mutate(KeywordsCorrected=factor(Sampling_List,levels=rev(sort(unique(Sampling_List))))) %>%
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

InterpolSamplingbyCountryPlot <- ggplot(InterpolSamplingbyCountry_Graph,aes(x=reorder(Country, CountryOrder),y=reorder(Sampling_List,desc(InterpolKeywordOrder)),fill=countfactor))+
  geom_tile(colour=InterpolSamplingbyCountry_Graph$ColourCode,width=0.85, height=0.85, size=0.1) + 
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
  mutate(Sampling_List = strsplit(as.character(Sampling_List), ";")) %>% 
  unnest(Sampling_List) %>%
  mutate_if(is.character, str_trim)

InterpolSamplingbyYear <- InterpolSamplingbyYear %>%
  filter(between(Year,2001,2022)) 

# replace blank with NA
InterpolSamplingbyYear[InterpolSamplingbyYear=="NA"] <- NA
InterpolSamplingbyYear [InterpolSamplingbyYear==""] <- NA
# remove NA in Sampling_List
InterpolSamplingbyYear <- InterpolSamplingbyYear[complete.cases(InterpolSamplingbyYear[ ,2]),]

InterpolSamplingbyYearCount <- aggregate(InterpolSamplingbyYear$Year, by=list(Year=InterpolSamplingbyYear$Year, Sampling_List=InterpolSamplingbyYear$Sampling_List), FUN=length)

# count the number of match to the Sampling_List list
InterpolSamplingTotal <- InterpolSamplingbyYearCount %>%
  group_by(Sampling_List) %>%
  summarise(x = sum(x))

#InterpolSamplingTotal <- aggregate(InterpolSamplingbyYearCount$Sampling_List, by=list(Explosive=InterpolSamplingbyYearCount$x), FUN=sum)

# count the total number of match to the Sampling_List list per year
InterpolSamplingYearTotal <- aggregate(InterpolSamplingbyYear$Year, by=list(InterpolSamplingbyYear$Year), FUN=length)
names(InterpolSamplingYearTotal)[1] <- c("Year")
names(InterpolSamplingYearTotal)[2] <- c("Total")

##### Count total number of papers per year ####
InterpolPapersbyYear <- Interpol_data %>%
  select(Year) %>%
  filter(between(Year,2001,2022))

InterpolPapersbyYear <- aggregate(InterpolPapersbyYear, by=list(Year=InterpolPapersbyYear$Year), FUN=length)
names(InterpolPapersbyYear)[2] <- c("Count")

#to select the top 5 explosives with the most matching keywords
InterpolSamplingTotal <- top_n(InterpolSamplingTotal,5)

#to choose which explosives to plot
#SelectedExplosives <- c("2,4,6-Trinitrotoluene (TNT)", "Cyclotetramethylene-Tetranitramine (HMX)","Triacetone Triperoxide (TATP)","Hexamethylene Triperoxide Diamine (HMTD)")
#InterpolSamplingTotal <-data_frame(SelectedExplosives)
names(InterpolSamplingTotal)[1] <- c("Sampling_List")

# subset against the top list
InterpolSamplingbyYearCount <- subset(InterpolSamplingbyYearCount, InterpolSamplingbyYearCount$Sampling_List %in% InterpolSamplingTotal$Sampling_List)

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
#mutate(Sampling_List=factor(Sampling_List,levels=rev(sort(unique(Sampling_List))))) %>%
# create a new variable from count
#mutate(countfactor=cut(x,breaks=c(BreakRange,max(x,na.rm=T)),
#   labels=DatasetRange))  %>%

# change level order
# mutate(countfactor=factor(as.character(countfactor),levels=rev(levels(countfactor))))
InterpolSamplingbyYearCountGraphReduced <- aggregate(InterpolSamplingbyYearCountGraph[, 1], list(InterpolSamplingbyYearCountGraph$Sampling_List), min)

InterpolSamplingbyYearCountGraph$graphorder <- as.numeric(gsr(InterpolSamplingbyYearCountGraph$Sampling_List,InterpolSamplingbyYearCountGraphReduced$Group.1,InterpolSamplingbyYearCountGraphReduced$x))

# further modified ggplot
InterpolTopExplosivesPlot <- ggplot(InterpolSamplingbyYearCountGraph,aes(x=Year))+
  geom_line(aes(y=Count, color=Sampling_List))+
  labs(x="Year",y="Number of Mentions")+
  scale_colour_manual(labels = c("Collection", "Recovery", "Sampling", "Swab"), name = "Sampling Terms", values = c("#FF5D00","#FFB900","#00CC00","#008BFF","#8968CD") )+
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


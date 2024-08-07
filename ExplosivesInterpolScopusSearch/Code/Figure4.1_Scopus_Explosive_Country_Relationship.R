#######################################################
#####         Explosive Keyword vs Country        #####
#####                   Scopus Data               #####
#######################################################
#############################################################
#####                     Keywords                      #####
#############################################################
# This section is looks at Keywords

# Load the Corpus of interest to search in the Scopus output entries
ExplosiveList <- read.csv("Explosives-List.txt", sep = "\t", header = TRUE)

ExplosiveList$ExplosiveProperName <- ExplosiveList$ExplosiveCompound

ExplosiveList$ExplosiveCompound <- gsub('[^[:alnum:] ]', ' ', ExplosiveList$ExplosiveCompound)
ExplosiveList$ExplosiveCompound <- gsub("\\s+", " ", ExplosiveList$ExplosiveCompound)

# Remove trailing (right) whitespace and make lowercase
ExplosiveList$ExplosiveCompound <- trimws(ExplosiveList$ExplosiveCompound, which = c("right"))
ExplosiveList$ExplosiveCompound <- tolower(ExplosiveList$ExplosiveCompound)

# Convert to Object
ExplosiveListString <- ExplosiveList$ExplosiveCompound

# #########################################################
# #####                For full text                  #####
# 
# ScopusCountryListbyPaperUniqueReducedtemp <- read.table("Scopus/FullTest.txt", header = TRUE)
# 
# ScopusCountryListbyPaperUniqueReducedtemp$FullText <- gsub('[^[:alnum:] ]', ' ', ScopusCountryListbyPaperUniqueReducedtemp$FullText)
# ScopusCountryListbyPaperUniqueReducedtemp$FullText <- gsub('      ', ' ', ScopusCountryListbyPaperUniqueReducedtemp$FullText)
# ScopusCountryListbyPaperUniqueReducedtemp$FullText <- gsub('     ', ' ', ScopusCountryListbyPaperUniqueReducedtemp$FullText)
# ScopusCountryListbyPaperUniqueReducedtemp$FullText <- gsub('    ', ' ', ScopusCountryListbyPaperUniqueReducedtemp$FullText)
# ScopusCountryListbyPaperUniqueReducedtemp$FullText <- gsub('   ', ' ', ScopusCountryListbyPaperUniqueReducedtemp$FullText)
# ScopusCountryListbyPaperUniqueReducedtemp$FullText <- gsub('  ', ' ', ScopusCountryListbyPaperUniqueReducedtemp$FullText)
# 
# ScopusCountryListbyPaperUniqueReducedtemp$FullText <- tolower(ScopusCountryListbyPaperUniqueReducedtemp$FullText)
# 
# ScopusCountryListbyPaperUniqueReduced <- ScopusCountryListbyPaperUniqueReducedtemp
# 
# ##############################################
# # Check for presence of each word in Full text
# Fulltext <- vector("list", nrow(ScopusCountryListbyPaperUniqueReduced))
# 
# for(d in 1:nrow(ScopusCountryListbyPaperUniqueReduced)){
#   for(w in seq_along(ExplosiveListString)){
#     intermed   <- grep(ExplosiveListString[[w]], ScopusCountryListbyPaperUniqueReduced[[d,1]])
#     Fulltext[[d]] <- c(Fulltext[[d]], 
#                        ExplosiveListString[[w]][ (length(intermed) > 0) ])
#   }
# }
# 
# # combined together
# FullTEXT <- sapply(Fulltext, paste0, collapse=";")
# FullTEXT <- as.data.frame(FullTEXT)
# 
# FullTEXT_temp <- data.frame(ScopusCountryListbyPaperUniqueReduced[,1],FullTEXT)
# 
# ##############################################

##############################################
# Check for presence of each word in Abstract
ScopusAbstracts <- vector("list", nrow(ScopusCountryListbyPaperUniqueReduced))

for(d in 1:nrow(ScopusCountryListbyPaperUniqueReduced)){
  for(w in seq_along(ExplosiveListString)){
    intermed   <- grep(ExplosiveListString[[w]], ScopusCountryListbyPaperUniqueReduced[[d,"Abstract"]])
    ScopusAbstracts[[d]] <- c(ScopusAbstracts[[d]], 
                              ExplosiveListString[[w]][ (length(intermed) > 0) ])
  }
}

# combined together
ExplosivesFromAbstract <- sapply(ScopusAbstracts, paste0, collapse=";")
ExplosivesFromAbstract <- as.data.frame(ExplosivesFromAbstract)

EID_ExplosivesFromAbstract <- data.frame(ScopusCountryListbyPaperUniqueReduced$EID,ExplosivesFromAbstract)

# remane fist column to match original dataset 
names(EID_ExplosivesFromAbstract)[1] <- c("EID")

##############################################
# Check for presence of each word in Title
ScopusTitles <- vector("list", nrow(ScopusCountryListbyPaperUniqueReduced))

for(d in 1:nrow(ScopusCountryListbyPaperUniqueReduced)){
  for(w in seq_along(ExplosiveListString)){
    intermed   <- grep(ExplosiveListString[[w]], ScopusCountryListbyPaperUniqueReduced[[d,"Title"]])
    ScopusTitles[[d]] <- c(ScopusTitles[[d]], 
                           ExplosiveListString[[w]][ (length(intermed) > 0) ])
  }
}

# combined together
ExplosivesFromTitle    <- sapply(ScopusTitles, paste0, collapse=";")
ExplosivesFromTitle <- as.data.frame(ExplosivesFromTitle)

EID_ExplosivesFromTitle <- data.frame(ScopusCountryListbyPaperUniqueReduced$EID,ExplosivesFromTitle)

# remane fist column to match original dataset 
names(EID_ExplosivesFromTitle)[1] <- c("EID")

EID_ExplosivesFromAbstractTitle <- full_join(EID_ExplosivesFromAbstract,EID_ExplosivesFromTitle)

##############################################
# Check for presence of each word in Keywords
ScopusKeywords <- vector("list", nrow(ScopusCountryListbyPaperUniqueReduced))

for(d in 1:nrow(ScopusCountryListbyPaperUniqueReduced)){
  for(w in seq_along(ExplosiveListString)){
    intermed   <- grep(ExplosiveListString[[w]], ScopusCountryListbyPaperUniqueReduced[[d,"AI.Keywords"]])
    ScopusKeywords[[d]] <- c(ScopusKeywords[[d]], 
                             ExplosiveListString[[w]][ (length(intermed) > 0) ])
  }
}

# combined together
ExplosivesFromKeyword    <- sapply(ScopusKeywords, paste0, collapse=";")
ExplosivesFromKeyword <- as.data.frame(ExplosivesFromKeyword)

EID_ExplosivesFromKeyword <- data.frame(ScopusCountryListbyPaperUniqueReduced$EID,ExplosivesFromKeyword)

# remane fist column to match original dataset 
names(EID_ExplosivesFromKeyword)[1] <- c("EID")

EID_ExplosivesFromAbstractTitleKeyword <- full_join(EID_ExplosivesFromAbstractTitle,EID_ExplosivesFromKeyword)

############
# Combine Columns Abstract,Titles,Keywords and place in Column name "Explosive_List" and remove original columns
EID_ExplosivesFromAbstractTitleKeyword <- EID_ExplosivesFromAbstractTitleKeyword %>%
  unite("Explosive_List", ExplosivesFromAbstract, ExplosivesFromTitle, ExplosivesFromKeyword,sep = ";", remove = TRUE)

############
# get the EID and respective country names to join to the EID_ExplosivesFromAbstractTitleKeyword

EID_Country <- ScopusCountryListbyPaperUniqueReduced %>%
  select(EID,Year,Country)

EID_ExplosivesFromAbstractTitleKeyword_Country <- full_join(EID_ExplosivesFromAbstractTitleKeyword,EID_Country)

# count the number of records with a match to the explosives reference list 
EID_ExplosivesFromAbstractTitleKeyword_CountryTemp <- EID_ExplosivesFromAbstractTitleKeyword_Country
EID_ExplosivesFromAbstractTitleKeyword_CountryTemp$Explosive_List <- gsub(";;","",EID_ExplosivesFromAbstractTitleKeyword_CountryTemp$Explosive_List)
# replace blank with NA
EID_ExplosivesFromAbstractTitleKeyword_CountryTemp[EID_ExplosivesFromAbstractTitleKeyword_CountryTemp==""] <- NA
EID_ExplosivesFromAbstractTitleKeyword_CountryTemp <- EID_ExplosivesFromAbstractTitleKeyword_CountryTemp[complete.cases(EID_ExplosivesFromAbstractTitleKeyword_CountryTemp[ ,2]),]

############
#Split Column "Country" in row by the separator ",", remove leading white space to generate list
KeywordListCountry <- EID_ExplosivesFromAbstractTitleKeyword_Country %>% 
  mutate(Country = strsplit(as.character(Country), ",")) %>% 
  unnest(Country) %>%
  mutate_if(is.character, str_trim)

#Split Column "AIKeywords" in row by the separator ";", remove leading white space to generate list
KeywordList <- KeywordListCountry %>% 
  mutate(Explosive_List = strsplit(as.character(Explosive_List), ";")) %>% 
  unnest(Explosive_List) %>%
  mutate_if(is.character, str_trim)

# remove duplicates
KeywordList <- KeywordList %>%
  distinct()

# replace blank with NA
KeywordList[KeywordList==""] <- NA
# remove NA in Explosive_List
KeywordList <- KeywordList[complete.cases(KeywordList[ ,2]),]

Keyword_CountryList <- KeywordList %>%
  select(Explosive_List,Country)

Keyword_Country_Count <- aggregate(Keyword_CountryList$Country, by=list(Country=Keyword_CountryList$Country, Explosive_List=Keyword_CountryList$Explosive_List), FUN=length)

# count the number of match to the Explosive_List list per country
Keyword_Country_Country_Count <- aggregate(Keyword_CountryList$Country, by=list(Country=Keyword_CountryList$Country), FUN=length)

#select the top 10 countries with the most matching keywords
Keyword_Country_Country_Count <- top_n(Keyword_Country_Country_Count,25)

# subset against the top list
Keyword_Country_Count_Subset <- subset(Keyword_Country_Count, Country %in% Keyword_Country_Country_Count$Country)

##################################################################
# select one of the two possible display for match to Explosive_List #
##################################################################

##### Either combined across all countries together

# This will include the top n countries with the highest number of Explosive_List match. This is different from the country map output as this would include all records regardless of match.
# This part is however bias toward country with the highest number of records as they may focus on specific entries

# count the number of match to the Explosive_List
Keyword_Keyword_Count <- aggregate(Keyword_CountryList$Explosive_List, by=list(Explosive_List = Keyword_CountryList$Explosive_List), FUN=length)

#select the top 100 Explosive_List appearing in list
#Keyword_Keyword_Count <- top_n(Keyword_Keyword_Count,100)
#write.csv(Keyword_Keyword_Count, file = "Keyword_Keyword_Count.csv", row.names = FALSE)

# subset against the top list
Keyword_Country_Explosive_List_Subset <- subset(Keyword_Country_Count_Subset, Explosive_List %in% Keyword_Keyword_Count$Explosive_List)


##### and by considering the top match of each country separately before merging together

# This part instead consider the top n countries and return their individual top n list of explosives reference list. They are then meged together in a list. 
Keyword_Country_Country_Count_list <- Keyword_Country_Country_Count$Country
# remove existing dataframe that may have been run on previous option selection
rm(Keyword_Keyword_Count)

for (i in 1:length(Keyword_Country_Country_Count_list)){
  
  Keyword_Country_Count_Subset_temp <- Keyword_CountryList %>%
    filter(Country == Keyword_Country_Country_Count_list[i])
  
  # count the number of match to the Explosive_List
  Keyword_Keyword_Count_temp <- aggregate(Keyword_Country_Count_Subset_temp$Explosive_List, by=list(Explosive_List = Keyword_Country_Count_Subset_temp$Explosive_List), FUN=length)
  
  #select the top 100 Explosive_List appearing in list
  Keyword_Keyword_Count_temp <- top_n(Keyword_Keyword_Count_temp,3)
  Keyword_Keyword_Count_temp$Country <- Keyword_Country_Country_Count_list[i]
  Keyword_Keyword_Count_temp <- Keyword_Keyword_Count_temp %>%
    select(Country,Explosive_List,x)
  # if the merged dataset doesn't exist, create it
  if (!exists("Keyword_Keyword_Count")){
    Keyword_Keyword_Count <- Keyword_Keyword_Count_temp
  }
  
  # if the merged dataset does exist, append to it
  if (exists("Keyword_Keyword_Count")){
    Keyword_Keyword_Count <-rbind(Keyword_Keyword_Count, Keyword_Keyword_Count_temp)
  }
  
}

# subset against the top list
# Keyword_Country_Explosive_List_Subset <- subset(Keyword_Country_Count_Subset, Explosive_List %in% Keyword_Keyword_Count$Explosive_List)
# Keyword_Country_Explosive_List_Subset <- Keyword_Keyword_Count
# write.table(Keyword_CountryList, file = "Keyword_CountryList.txt", quote = F, sep = "\t", row.names = F)

############################
##### For top keywords #####
############################

names(Keyword_Country_Explosive_List_Subset)[3] <- "Frequency"

Keyword_Country_Explosive_List_Subset <- full_join(Keyword_Country_Explosive_List_Subset, Keyword_Keyword_Count)
Keyword_Country_Explosive_List_Subset$x[!is.na(Keyword_Country_Explosive_List_Subset$x)] <- "black"
Keyword_Country_Explosive_List_Subset$x[is.na(Keyword_Country_Explosive_List_Subset$x)] <- "white"
names(Keyword_Country_Explosive_List_Subset)[4] <- "ColourCode"

Keyword_Country_Explosive_List_Subset$Country <- as.factor(Keyword_Country_Explosive_List_Subset$Country)
Keyword_Country_Explosive_List_Subset$Explosive_List <- as.factor(Keyword_Country_Explosive_List_Subset$Explosive_List)
Keyword_Country_Explosive_List_Subset$Explosive_List <- toupper(Keyword_Country_Explosive_List_Subset$Explosive_List)
# Keyword_Country_Explosive_List_Subset$Frequency <- factor(Keyword_Country_Explosive_List_Subset$Frequency)

# ordering keywords in plot by most frequent across all countries
Graph_order_keywords <- aggregate(Keyword_Country_Explosive_List_Subset$Explosive_List, by=list(Keyword_Country_Explosive_List_Subset$Explosive_List), FUN=length)

Keyword_Country_Explosive_List_Subset$Graph_order_Keywords <-  gsr(as.character(Keyword_Country_Explosive_List_Subset$Explosive_List),as.character(Graph_order_keywords$Group.1),Graph_order_keywords$x)
Keyword_Country_Explosive_List_Subset$Graph_order_Keywords <- as.numeric(Keyword_Country_Explosive_List_Subset$Graph_order_Keywords)

# ordering countries by highest number of keywords
Graph_order_countries <- aggregate(Keyword_Country_Explosive_List_Subset$Country, by=list(Keyword_Country_Explosive_List_Subset$Country), FUN=length)

Keyword_Country_Explosive_List_Subset$Graph_order_Countries <-  gsr(as.character(Keyword_Country_Explosive_List_Subset$Country),as.character(Graph_order_countries$Group.1),Graph_order_countries$x)
Keyword_Country_Explosive_List_Subset$Graph_order_Countries <- as.numeric(Keyword_Country_Explosive_List_Subset$Graph_order_Countries)

# this is to set the range for the keyword figure
# range <- as.numeric(max(Keyword_Country_Explosive_List_Subset$Frequency, na.rm = TRUE))
# 
# source("Functions/KeywordRange.R")

#############################################################
#####                      GRAPH                        #####
#############################################################

# Create a new variable from incidence (breaks to be changed to fit Interpol vs. Scopus data)
#Breaks and labels for Interpol
# Keyword_Country_Explosive_List_Subset$groups <- cut(Keyword_Country_Explosive_List_Subset$Frequency,
#                                                      breaks = c(BreakRange,max(Keyword_Country_Explosive_List_Subset$Frequency,na.rm=T)),
#                                                      labels=DatasetRange)
# 
# ScopusExplosivesbyCountry_Graph <- Keyword_Country_Explosive_List_Subset %>%
#   # convert state to factor and reverse order of levels
#   mutate(KeywordsCorrected=factor(Explosive_List,levels=rev(sort(unique(Explosive_List))))) %>%
#   # create a new variable from count
#   mutate(countfactor=cut(Frequency,breaks=c(BreakRange,max(Frequency,na.rm=T)),
#                          labels=DatasetRange))  %>%
#     # change level order
#   mutate(countfactor=factor(as.character(countfactor),levels=rev(levels(countfactor))))

# this is to set the range for the keyword figure
Keyword_Country_Explosive_List_Subset$groups <- cut(Keyword_Country_Explosive_List_Subset$Frequency,               # Add group column
                                                    breaks = c(-1, 0, 1, 2, 5, 10, 50, 100, max(Keyword_Country_Explosive_List_Subset$Frequency,na.rm = T)),
                                                    labels=c("0","1","2","3-5","6-10","11-50","51-100",">100"))

ScopusExplosivesbyCountry_Graph <- Keyword_Country_Explosive_List_Subset %>%
  # convert state to factor and reverse order of levels
  mutate(KeywordsCorrected=factor(Explosive_List,levels=rev(sort(unique(Explosive_List))))) %>%
  # create a new variable from count
  mutate(countfactor=cut(Frequency,breaks=c(-1, 0, 1, 2, 5, 10, 50, 100, max(Frequency,na.rm = T)),
                         labels=c("0","1","2","3-5","6-10","11-50","51-100",">100")))  %>%
  # change level order
  mutate(countfactor=factor(as.character(countfactor),levels=rev(levels(countfactor))))
# ScopusKeywordList$WYear <- gsr(ScopusKeywordList$Year,year$Var1,1/year$Freq)

# Change the name back to the their format in ExplosiveProperName
ScopusExplosivesbyCountry_Graph$Explosive_List <- tolower(ScopusExplosivesbyCountry_Graph$Explosive_List)

ScopusExplosivesbyCountry_Graph$ExplosiveProperName <- ScopusExplosivesbyCountry_Graph$Explosive_List

ScopusExplosivesbyCountry_Graph$ExplosiveProperName <- gsr(as.character(ScopusExplosivesbyCountry_Graph$Explosive_List),as.character(ExplosiveList$ExplosiveCompound),as.character(ExplosiveList$ExplosiveProperName))
# write.csv(ScopusExplosivesbyCountry_Graph,file = "temp.csv",row.names = FALSE)
textcol <- "black"

ScopusExplosivesbyCountryPlot <- ggplot(ScopusExplosivesbyCountry_Graph,aes(x=reorder(Country, Graph_order_Countries),y=reorder(ExplosiveProperName,desc(Graph_order_Keywords)),fill=countfactor))+
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
        axis.text.y=element_text(size=7, vjust=0.2),
        axis.ticks=element_line(size=0.4),
        plot.background=element_blank(),  # element_rect(fill, colour, size, linetype, color))
        panel.border=element_blank(),
        plot.margin=margin(0.7,0.4,0.1,0.2,"cm"),
        plot.title=element_text(colour=textcol,hjust=0,size=6),
        axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_text(size=7, angle = 60, hjust=1))

show(ScopusExplosivesbyCountryPlot)

#save figure
Var1 <- paste0("Fig_4.1_Scopus_Explosive_List_Country")

ggsave(paste0(Figure.dir,sprintf("%s.tiff",Var1)), ScopusExplosivesbyCountryPlot, width = 7.5, height = 8.5, units = "in", dpi=300)

print("Processing complete. Please check 'Figures/' folder for output")

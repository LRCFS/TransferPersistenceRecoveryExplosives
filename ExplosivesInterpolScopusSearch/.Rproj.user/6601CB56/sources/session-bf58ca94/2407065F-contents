#######################################################
######                 Keywords                  ######
#######################################################

#######################################################
######          Scopus Reference List            ######
######              Figure 1                     ######
#######################################################
#####                  Scopus Data                #####

Scopus_Fig1_Data <- Scopus_data

#############################################################
#####                  Data cleansing                   #####
#############################################################

#Correction to the keywords can be applied at this stage. This can be done in Notepad++, Excel etc. The ultimate order of the list must be kept so it can be binded to the orignial data.
#read the corrected list of keywords and combine it to the original list

ScopusKeywordList$KeywordsCorrected <- gsr(as.character(ScopusKeywordList$AIKeywords),as.character(KeywordCorrectionList$AIKeywords),as.character(KeywordCorrectionList$CorAIKeywordsAcronym))

ScopusKeywordListCollapsed <- ScopusKeywordList %>% 
  group_by(Title) %>%
  summarize(KeywordsCorrected=paste0(KeywordsCorrected, collapse = ", "))

# number of distinct Keywords after correction
ScopusDistinctKeywordListCorrected <- ScopusKeywordList %>%
  select(KeywordsCorrected) %>%
  distinct()

#############################################################
#####               Data analysis - Keywords            #####
#############################################################

#Count to number of time the same year is repeated in the "DatasetKeywordList$Year" and save in a data.frame "Year" 
ScopusPublicationYear<- data.frame(table(Scopus_Fig1_Data$Year))
names(ScopusPublicationYear) <- c("Year","Publications")
ScopusPublicationYear$Year <- as.numeric(as.character(ScopusPublicationYear$Year))

#count the number of keywords per title paper 
ScopusKeywordList_KeywordsPerTitle <- ScopusKeywordList  %>%
  select(Year,Title,Source.title,KeywordsCorrected) %>%
  distinct()

ScopusKeywordList_KeywordsPerTitle[ScopusKeywordList_KeywordsPerTitle==""] <- NA
ScopusKeywordList_KeywordsPerTitle <-ScopusKeywordList_KeywordsPerTitle[complete.cases(ScopusKeywordList_KeywordsPerTitle[ ,4]), ]

ScopusKeywordYearCount <- aggregate(ScopusKeywordList_KeywordsPerTitle$Year, by=list(Year=ScopusKeywordList_KeywordsPerTitle$Year, Keyword=ScopusKeywordList_KeywordsPerTitle$KeywordsCorrected), FUN=length)
ScopusKeywordTotalCount <- aggregate(ScopusKeywordList_KeywordsPerTitle$Year, by=list(Keyword=ScopusKeywordList_KeywordsPerTitle$KeywordsCorrected), FUN=length)

# narrowing range for plot
ScopusKeywordNarrowRangeGraph <- top_n(ScopusKeywordTotalCount, Count)

# count the number of rows, hence the number of keywords in figure
a <- nrow(ScopusKeywordNarrowRangeGraph)

while (a>maximum) {
  Count <- Count-1
  ScopusKeywordNarrowRangeGraph <- top_n(ScopusKeywordTotalCount, Count)
  a <- nrow(ScopusKeywordNarrowRangeGraph)
}

# DatasetKeywordNarrowRangeGraph <- subset(DatasetKeywordTotalCount,x>Count)
SubsetScopusKeywordNarrowRangeGraph <-subset(ScopusKeywordYearCount,Keyword %in% ScopusKeywordNarrowRangeGraph$Keyword)

# this is to set the range for the keyword figure
range <- as.numeric(max(SubsetScopusKeywordNarrowRangeGraph$x, na.rm = TRUE))

source("Functions/KeywordRange.R")

#############################################################
#####                      GRAPH                        #####
#############################################################

# Create a new variable from incidence (breaks to be changed to fit Scopus vs. Scopus data)
#Breaks and labels for Scopus

SubsetScopusKeywordNarrowRangeGraph$Incidenceweight <- cut(SubsetScopusKeywordNarrowRangeGraph$x,
                                                           breaks = c(BreakRange,max(SubsetScopusKeywordNarrowRangeGraph$x,na.rm=T)),
                                                           labels=DatasetRange)

ScopusTopKeywordsGraph <- SubsetScopusKeywordNarrowRangeGraph %>%
  # convert state to factor and reverse order of levels
  mutate(KeywordsCorrected=factor(Keyword,levels=rev(sort(unique(Keyword))))) %>%
  # create a new variable from count
  mutate(countfactor=cut(x,breaks=c(BreakRange,max(x,na.rm=T)),
                         labels=DatasetRange))  %>%
  
  # change level order
  mutate(countfactor=factor(as.character(countfactor),levels=rev(levels(countfactor))))
ScopusTopKeywordsGraphReduced <- aggregate(ScopusTopKeywordsGraph[, 1], list(ScopusTopKeywordsGraph$KeywordsCorrected), min)

ScopusTopKeywordsGraph$graphorder <- as.numeric(gsr(ScopusTopKeywordsGraph$KeywordsCorrected,ScopusTopKeywordsGraphReduced$Group.1,ScopusTopKeywordsGraphReduced$x))

# further modified ggplot
ScopusKeywordPlot <- ggplot(ScopusTopKeywordsGraph,aes(x=Year,y=reorder(KeywordsCorrected,graphorder),fill=countfactor))+
  geom_tile(colour="white",size=0.2)+
  guides(fill=guide_legend(title="Count"))+
  labs(x="Year",y="",title="")+
  scale_y_discrete(expand=c(0,0))+
  scale_x_continuous(breaks=c(1960,1970,1980,1990,2000,2010,2020))+
  scale_fill_manual(values=c(pal),na.value = "grey90")+
  #coord_fixed()+
  theme_grey(base_size=6)+
  theme(text = element_text(family = "sans"),
        legend.position="right",legend.direction="vertical",
        legend.title=element_text(colour=textcol),
        legend.margin=margin(grid::unit(0,"cm")),
        legend.text=element_text(colour=textcol,size=6),
        legend.key.height=grid::unit(0.8,"cm"),
        legend.key.width=grid::unit(0.2,"cm"),
        axis.text.x=element_text(size=6,colour=textcol),
        axis.text.y=element_text(size=6,vjust=0.2,colour=textcol),
        axis.ticks=element_line(size=0.4),
        plot.background=element_blank(),  # element_rect(fill, colour, size, linetype, color))
        panel.border=element_blank(),
        plot.margin=margin(0.2,0.2,0.2,0.2,"cm"),
        plot.title=element_text(colour=textcol,hjust=0,size=6))

#save figure
Var1 <- paste0("Fig_1_Scopus_",KeywordEntries)

ggsave(paste0(Figure.dir,sprintf("%s.tiff",Var1)), ScopusKeywordPlot, width = 14, height = 20, units = "cm", dpi=600)

show(ScopusKeywordPlot)

#Export to top keywords list
write.csv(SubsetScopusKeywordNarrowRangeGraph, file=paste0(Results.dir,sprintf("%s.csv",Var1)), row.names = F)

print("Processing complete. Please check 'Results/' and 'Figures/' folders for output")


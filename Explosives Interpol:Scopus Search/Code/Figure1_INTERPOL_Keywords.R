#######################################################
######                 Keywords                  ######
#######################################################

#######################################################
######          INTERPOL Reference List          ######
######              Figure 1                     ######
#######################################################
#####                 INTERPOL Data               #####

Interpol_Fig1_Data <- Interpol_data_selected

#############################################################
#####               Data analysis - Keywords            #####
#############################################################

#Count to number of time the same year is repeated in the "DatasetKeywordList$Year" and save in a data.frame "Year" 
InterpolPublicationYear<- data.frame(table(Interpol_Fig1_Data$Year))
names(InterpolPublicationYear) <- c("Year","Publications")
InterpolPublicationYear$Year <- as.numeric(as.character(InterpolPublicationYear$Year))

#count the number of keywords per title paper 
InterpolKeywordList_KeywordsPerTitle <- InterpolKeywordList  %>%
  select(Year,Title,Source.title,KeywordsCorrected) %>%
  distinct()
InterpolKeywordList_KeywordsPerTitle <-InterpolKeywordList_KeywordsPerTitle[complete.cases(InterpolKeywordList_KeywordsPerTitle), ]
sum(is.na(InterpolKeywordList$KeywordsCorrected))

InterpolKeywordYearCount <- aggregate(InterpolKeywordList_KeywordsPerTitle$Year, by=list(Year=InterpolKeywordList_KeywordsPerTitle$Year, Keyword=InterpolKeywordList_KeywordsPerTitle$KeywordsCorrected), FUN=length)
InterpolKeywordTotalCount <- aggregate(InterpolKeywordList_KeywordsPerTitle$Year, by=list(Keyword=InterpolKeywordList_KeywordsPerTitle$KeywordsCorrected), FUN=length)

# narrowing range for plot
InterpolKeywordNarrowRangeGraph <- top_n(InterpolKeywordTotalCount, Count)

# count the number of rows, hence the number of keywords in figure
a <- nrow(InterpolKeywordNarrowRangeGraph)

while (a>maximum) {
  Count <- Count-1
  InterpolKeywordNarrowRangeGraph <- top_n(InterpolKeywordTotalCount, Count)
  a <- nrow(InterpolKeywordNarrowRangeGraph)
}

# DatasetKeywordNarrowRangeGraph <- subset(DatasetKeywordTotalCount,x>Count)
SubsetInterpolKeywordNarrowRangeGraph <-subset(InterpolKeywordYearCount,Keyword %in% InterpolKeywordNarrowRangeGraph$Keyword)

# this is to set the range for the keyword figure
range <- as.numeric(max(SubsetInterpolKeywordNarrowRangeGraph$x, na.rm = TRUE))

source("Functions/KeywordRange.R")

#############################################################
#####                      GRAPH                        #####
#############################################################

# Create a new variable from incidence (breaks to be changed to fit Interpol vs. Scopus data)
#Breaks and labels for Interpol

SubsetInterpolKeywordNarrowRangeGraph$Incidenceweight <- cut(SubsetInterpolKeywordNarrowRangeGraph$x,
                                                     breaks = c(BreakRange,max(SubsetInterpolKeywordNarrowRangeGraph$x,na.rm=T)),
                                                     labels=DatasetRange)

InterpolTopKeywordsGraph <- SubsetInterpolKeywordNarrowRangeGraph %>%
  # convert state to factor and reverse order of levels
  mutate(KeywordsCorrected=factor(Keyword,levels=rev(sort(unique(Keyword))))) %>%
  # create a new variable from count
  mutate(countfactor=cut(x,breaks=c(BreakRange,max(x,na.rm=T)),
                         labels=DatasetRange))  %>%
  
  # change level order
  mutate(countfactor=factor(as.character(countfactor),levels=rev(levels(countfactor))))
InterpolTopKeywordsGraphReduced <- aggregate(InterpolTopKeywordsGraph[, 1], list(InterpolTopKeywordsGraph$KeywordsCorrected), min)

InterpolTopKeywordsGraph$graphorder <- as.numeric(gsr(InterpolTopKeywordsGraph$KeywordsCorrected,InterpolTopKeywordsGraphReduced$Group.1,InterpolTopKeywordsGraphReduced$x))

# further modified ggplot
InterpolKeywordPlot <- ggplot(InterpolTopKeywordsGraph,aes(x=Year,y=reorder(KeywordsCorrected,graphorder),fill=countfactor))+
  geom_tile(colour="white",size=0.2)+
  guides(fill=guide_legend(title="Count"))+
  labs(x="Year",y="",title="")+
  scale_y_discrete(expand=c(0,0))+
  scale_x_continuous(breaks=c(2000,2005,2010,2015,2020))+
  scale_fill_manual(values=c(pal),na.value = "grey90")+
  #coord_fixed()+
  theme_grey(base_size=8)+
  theme(text = element_text(family = "Arial"),
        legend.position="right",legend.direction="vertical",
        legend.title=element_text(colour=textcol),
        legend.margin=margin(grid::unit(0,"cm")),
        legend.text=element_text(colour=textcol,size=7),
        legend.key.height=grid::unit(0.8,"cm"),
        legend.key.width=grid::unit(0.2,"cm"),
        axis.text.x=element_text(size=8,colour=textcol),
        axis.text.y=element_text(vjust=0.2,colour=textcol),
        axis.ticks=element_line(size=0.4),
        plot.background=element_blank(),  # element_rect(fill, colour, size, linetype, color))
        panel.border=element_blank(),
        plot.margin=margin(-0.2,0.4,0.1,0.2,"cm"),
        plot.title=element_text(colour=textcol,hjust=0,size=12))

#save figure
Var1 <- paste0("Fig_1_INTERPOL_",KeywordEntries)

ggsave(paste0(Figure.dir,sprintf("%s.tiff",Var1)), InterpolKeywordPlot, width = 6.88, height = 8.5, units = "in", dpi=300)

show(InterpolKeywordPlot)

#Export to top keywords list
write.csv(SubsetInterpolKeywordNarrowRangeGraph, file=paste0(Results.dir,sprintf("%s.csv",Var1)), row.names = F)

print("Processing complete. Please check 'Results/' and 'Figures/' folders for output")

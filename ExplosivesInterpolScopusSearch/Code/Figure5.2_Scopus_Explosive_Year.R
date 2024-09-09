#Split Column "Country" in row by the separator ",", remove leading white space to generate list
ScopusExplosivesbyYear <- ScopusExplosives %>% 
  mutate(Explosive_List_Corrected = strsplit(as.character(Explosive_List_Corrected), ";")) %>% 
  unnest(Explosive_List_Corrected) %>%
  mutate_if(is.character, str_trim)

ScopusExplosivesbyYear <- ScopusExplosivesbyYear %>%
  filter(between(Year,2001,2022)) 

# replace blank with NA
ScopusExplosivesbyYear[ScopusExplosivesbyYear=="NA"] <- NA
ScopusExplosivesbyYear [ScopusExplosivesbyYear==""] <- NA
# remove NA in Explosive_List
ScopusExplosivesbyYear <- ScopusExplosivesbyYear[complete.cases(ScopusExplosivesbyYear[ ,2]),]

ScopusExplosivesbyYearCount <- aggregate(ScopusExplosivesbyYear$Year, by=list(Year=ScopusExplosivesbyYear$Year, Explosive_List=ScopusExplosivesbyYear$Explosive_List_Corrected), FUN=length)

# count the number of match to the Explosive_List list
ScopusExplosivesTotal <- ScopusExplosivesbyYearCount %>%
  group_by(Explosive_List) %>%
  summarise(x = sum(x))

#ScopusExplosivesTotal <- aggregate(ScopusExplosivesbyYearCount$Explosive_List, by=list(Explosive=ScopusExplosivesbyYearCount$x), FUN=sum)

# count the total number of match to the Explosive_List list per year
ScopusExplosivesYearTotal <- aggregate(ScopusExplosivesbyYear$Year, by=list(ScopusExplosivesbyYear$Year), FUN=length)
names(ScopusExplosivesYearTotal)[1] <- c("Year")
names(ScopusExplosivesYearTotal)[2] <- c("Total")

##### Count total number of papers per year ####
ScopusPapersbyYear <- Scopus_data %>%
  select(Year) %>%
  filter(between(Year,2001,2022))

ScopusPapersbyYear <- aggregate(ScopusPapersbyYear, by=list(Year=ScopusPapersbyYear$Year), FUN=length)
names(ScopusPapersbyYear)[2] <- c("Count")

#to select the top 5 explosives with the most matching keywords
ScopusExplosivesTotal <- top_n(ScopusExplosivesTotal,5)

#to choose which explosives to plot
#SelectedExplosives <- c("2,4,6-Trinitrotoluene (TNT)", "Cyclotrimethylene-Trinitramine (RDX)", "Cyclotetramethylene-Tetranitramine (HMX)", "Pentaerythritol Tetranitrate (PETN)","Ammonium Nitrate (AN)","Triacetone Triperoxide (TATP)")
#ScopusExplosivesTotal <-data_frame(SelectedExplosives)
names(ScopusExplosivesTotal)[1] <- c("Explosive_List")

# subset against the top list
ScopusExplosivesbyYearCount <- subset(ScopusExplosivesbyYearCount, ScopusExplosivesbyYearCount$Explosive_List %in% ScopusExplosivesTotal$Explosive_List)

# this is to set the range for the keyword figure
range <- as.numeric(max(ScopusExplosivesbyYearCount$x, na.rm = TRUE))

source("Functions/KeywordRange.R")



#############################################################
#####                      GRAPH                        #####
#############################################################

# Create a new variable from incidence (breaks to be changed to fit Scopus vs. Scopus data)
#Breaks and labels for Scopus

ScopusExplosivesbyYearCountGraph <- ScopusExplosivesbyYearCount

ScopusExplosivesbyYearCountGraph$YearTotal <- ScopusExplosivesbyYearCountGraph$Year
ScopusExplosivesbyYearCountGraph$YearTotal <- gsr(as.character(ScopusExplosivesbyYearCountGraph$YearTotal),as.character(ScopusExplosivesYearTotal$Year),as.character(ScopusExplosivesYearTotal$Total))

names(ScopusExplosivesbyYearCountGraph)[3] <- c("Count")
names(ScopusExplosivesbyYearCountGraph)[4] <- c("YearTotal")
ScopusExplosivesbyYearCountGraph$Percentage <- as.numeric(ScopusExplosivesbyYearCountGraph$Count)/as.numeric(ScopusExplosivesbyYearCountGraph$YearTotal)*100
ScopusExplosivesbyYearCountGraph$YearTotal <- ScopusExplosivesbyYearCountGraph$Year
ScopusExplosivesbyYearCountGraph$YearTotal <- gsr(as.character(ScopusExplosivesbyYearCountGraph$YearTotal),as.character(ScopusPapersbyYear$Year),as.character(ScopusPapersbyYear$Count))

ScopusExplosivesbyYearCountGraph$YearTotal <-as.numeric(ScopusExplosivesbyYearCountGraph$YearTotal)

# ScopusExplosivesbyYearCountGraph$Incidenceweight <- cut(ScopusExplosivesbyYearCount$x,
#      breaks = c(BreakRange,max(ScopusExplosivesbyYearCount$x,na.rm=T)),
#     labels=DatasetRange)

#ScopusExplosivesbyYearCountGraph <- ScopusExplosivesbyYearCountGraph %>%
# convert state to factor and reverse order of levels
#mutate(Explosive_List=factor(Explosive_List,levels=rev(sort(unique(Explosive_List))))) %>%
# create a new variable from count
#mutate(countfactor=cut(x,breaks=c(BreakRange,max(x,na.rm=T)),
#   labels=DatasetRange))  %>%

# change level order
# mutate(countfactor=factor(as.character(countfactor),levels=rev(levels(countfactor))))
ScopusExplosivesbyYearCountGraphReduced <- aggregate(ScopusExplosivesbyYearCountGraph[, 1], list(ScopusExplosivesbyYearCountGraph$Explosive_List), min)

ScopusExplosivesbyYearCountGraph$graphorder <- as.numeric(gsr(ScopusExplosivesbyYearCountGraph$Explosive_List,ScopusExplosivesbyYearCountGraphReduced$Group.1,ScopusExplosivesbyYearCountGraphReduced$x))

# further modified ggplot
ScopusTopExplosivesPlot <- ggplot(ScopusExplosivesbyYearCountGraph,aes(x=Year))+
  geom_line(aes(y=Percentage, color=Explosive_List))+
  geom_point(aes(y=Percentage, color=Explosive_List))+
  geom_line(aes(y=YearTotal/100, colour = "'Number of Papers with Explosive Keywords"), linetype = 3)+
  labs(x="Year",y="Percentage Mentions (%)")+
  scale_y_continuous()+
  scale_y_continuous(sec.axis = sec_axis(transform = ~.* 100, name = "Number of Papers"))+
  scale_x_continuous(breaks=c(2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2020,2022))+
  scale_fill_manual(values=c(pal),na.value = "grey90")+
  theme_grey(base_size=8)+
  theme(text = element_text(family = "Arial"),
        legend.position="right",legend.direction="vertical",
        #legend.title=element_text(colour=textcol),
        legend.margin=margin(grid::unit(0,"cm")),
        legend.text=element_text(colour=textcol,size=7),
        legend.key.height=grid::unit(0.8,"cm"),
        legend.key.width=grid::unit(0.2,"cm"),
        legend.title=element_blank(),
        axis.text.x=element_text(size=7,colour=textcol),
        axis.text.y=element_text(vjust=0.2,colour=textcol),
        axis.ticks=element_line(size=0.4),
        plot.background=element_blank(),  # element_rect(fill, colour, size, linetype, color))
        panel.border=element_blank(),
        plot.margin=margin(-0.2,0.4,0.1,0.2,"cm"),
        plot.title=element_text(colour=textcol,hjust=0,size=12))

ScopusTopExplosivesPlot <- ScopusTopExplosivesPlot + scale_fill_discrete(breaks=c('Number of Papers Published', '2,4,6-Trinitrotoluene (TNT)','Ammonium Nitrate (AN)','Cyclotetramethylene-Tetranitramine (HMX)','Cyclotrimethylene-Trinitramine (RDX)','Pentaerythritol Tetranitrate (PETN)','Triacetone Triperoxide (TATP)'))
  
  #save figure
  Var1 <- paste0("Fig_5.2_Scopus_Explosives_By_Year")
  show(ScopusTopExplosivesPlot) 
  ggsave(paste0(Figure.dir,sprintf("%s.tiff",Var1)), ScopusTopExplosivesPlot, width = 6.88, height = 8.5, units = "in", dpi=300)

#Export to top keywords list
write.csv(ScopusExplosivesbyYearCountGraph, file=paste0(Results.dir,sprintf("%s.csv",Var1)), row.names = F)

print("Processing complete. Please check 'Results/' and 'Figures/' folders for output")


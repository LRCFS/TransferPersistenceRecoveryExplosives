#######################################################
#####       Interpol Explosive Keywords           #####
#####              vs Year    Plot                #####
#######################################################
#Split Column "Country" in row by the separator ",", remove leading white space to generate list
InterpolExplosivesbyYear <- InterpolExplosives %>% 
  mutate(Explosive_List_Corrected = strsplit(as.character(Explosive_List_Corrected), ";")) %>% 
  unnest(Explosive_List_Corrected) %>%
  mutate_if(is.character, str_trim)

InterpolExplosivesbyYear <- InterpolExplosivesbyYear %>%
  filter(between(Year,2001,2022)) 

# replace blank with NA
InterpolExplosivesbyYear[InterpolExplosivesbyYear=="NA"] <- NA
InterpolExplosivesbyYear [InterpolExplosivesbyYear==""] <- NA
# remove NA in Explosive_List
InterpolExplosivesbyYear <- InterpolExplosivesbyYear[complete.cases(InterpolExplosivesbyYear[ ,2]),]

InterpolExplosivesbyYearCount <- aggregate(InterpolExplosivesbyYear$Year, by=list(Year=InterpolExplosivesbyYear$Year, Explosive_List=InterpolExplosivesbyYear$Explosive_List_Corrected), FUN=length)

# count the number of match to the Explosive_List list
InterpolExplosivesTotal <- InterpolExplosivesbyYearCount %>%
  group_by(Explosive_List) %>%
  summarise(x = sum(x))

#InterpolExplosivesTotal <- aggregate(InterpolExplosivesbyYearCount$Explosive_List, by=list(Explosive=InterpolExplosivesbyYearCount$x), FUN=sum)

# count the total number of match to the Explosive_List list per year
InterpolExplosivesYearTotal <- aggregate(InterpolExplosivesbyYear$Year, by=list(InterpolExplosivesbyYear$Year), FUN=length)
names(InterpolExplosivesYearTotal)[1] <- c("Year")
names(InterpolExplosivesYearTotal)[2] <- c("Total")

##### Count total number of papers per year ####
InterpolPapersbyYear <- Interpol_data %>%
  select(Year) %>%
  filter(between(Year,2001,2022))

InterpolPapersbyYear <- aggregate(InterpolPapersbyYear, by=list(Year=InterpolPapersbyYear$Year), FUN=length)
names(InterpolPapersbyYear)[2] <- c("Count")

#to select the top 5 explosives with the most matching keywords
# InterpolExplosivesTotal <- top_n(InterpolExplosivesTotal,5)

#to choose which explosives to plot
SelectedExplosives <- c("2,4,6-Trinitrotoluene (TNT)", "Cyclotetramethylene-Tetranitramine (HMX)","Triacetone Triperoxide (TATP)","Hexamethylene Triperoxide Diamine (HMTD)")
InterpolExplosivesTotal <-data_frame(SelectedExplosives)
names(InterpolExplosivesTotal)[1] <- c("Explosive_List")

# subset against the top list
InterpolExplosivesbyYearCount <- subset(InterpolExplosivesbyYearCount, InterpolExplosivesbyYearCount$Explosive_List %in% InterpolExplosivesTotal$Explosive_List)

# this is to set the range for the keyword figure
range <- as.numeric(max(InterpolExplosivesbyYearCount$x, na.rm = TRUE))

source("Functions/KeywordRange.R")

#############################################################
#####                      GRAPH                        #####
#############################################################

# Create a new variable from incidence (breaks to be changed to fit Interpol vs. Interpol data)
#Breaks and labels for Interpol

InterpolExplosivesbyYearCountGraph <- InterpolExplosivesbyYearCount

InterpolExplosivesbyYearCountGraph$YearTotal <- InterpolExplosivesbyYearCountGraph$Year
InterpolExplosivesbyYearCountGraph$YearTotal <- gsr(as.character(InterpolExplosivesbyYearCountGraph$YearTotal),as.character(InterpolExplosivesYearTotal$Year),as.character(InterpolExplosivesYearTotal$Total))

names(InterpolExplosivesbyYearCountGraph)[3] <- c("Count")
names(InterpolExplosivesbyYearCountGraph)[4] <- c("YearTotal")
InterpolExplosivesbyYearCountGraph$Percentage <- as.numeric(InterpolExplosivesbyYearCountGraph$Count)/as.numeric(InterpolExplosivesbyYearCountGraph$YearTotal)*100
InterpolExplosivesbyYearCountGraph$YearTotal <- InterpolExplosivesbyYearCountGraph$Year
InterpolExplosivesbyYearCountGraph$YearTotal <- gsr(as.character(InterpolExplosivesbyYearCountGraph$YearTotal),as.character(InterpolPapersbyYear$Year),as.character(InterpolPapersbyYear$Count))

InterpolExplosivesbyYearCountGraph$YearTotal <-as.numeric(InterpolExplosivesbyYearCountGraph$YearTotal)

# InterpolExplosivesbyYearCountGraph$Incidenceweight <- cut(InterpolExplosivesbyYearCount$x,
#      breaks = c(BreakRange,max(InterpolExplosivesbyYearCount$x,na.rm=T)),
#     labels=DatasetRange)

#InterpolExplosivesbyYearCountGraph <- InterpolExplosivesbyYearCountGraph %>%
# convert state to factor and reverse order of levels
#mutate(Explosive_List=factor(Explosive_List,levels=rev(sort(unique(Explosive_List))))) %>%
# create a new variable from count
#mutate(countfactor=cut(x,breaks=c(BreakRange,max(x,na.rm=T)),
#   labels=DatasetRange))  %>%

# change level order
# mutate(countfactor=factor(as.character(countfactor),levels=rev(levels(countfactor))))
InterpolExplosivesbyYearCountGraphReduced <- aggregate(InterpolExplosivesbyYearCountGraph[, 1], list(InterpolExplosivesbyYearCountGraph$Explosive_List), min)

InterpolExplosivesbyYearCountGraph$graphorder <- as.numeric(gsr(InterpolExplosivesbyYearCountGraph$Explosive_List,InterpolExplosivesbyYearCountGraphReduced$Group.1,InterpolExplosivesbyYearCountGraphReduced$x))

# further modified ggplot
InterpolTopExplosivesPlot <- ggplot(InterpolExplosivesbyYearCountGraph,aes(x=Year))+
  geom_line(aes(y=Percentage, color=Explosive_List))+
  #geom_point(aes(y=Percentage, color=Explosive_List))+
  geom_line(aes(y=YearTotal/10, colour = "'Number of Papers with Explosive Keywords"), linetype = 3)+
  labs(x="Year",y="Percentage Mentions (%)")+
  scale_y_continuous()+
  scale_y_continuous(sec.axis = sec_axis(transform = ~.* 10, name = "Number of Papers"))+
  scale_x_continuous(breaks=c(2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2020,2022))+
  scale_color_manual(values = c("black", brewer.pal(5, "Set1")))
  theme_grey(base_size=8)+
  theme(text = element_text(family = "Arial"),
        legend.position="right",legend.direction="vertical",
        legend.margin=margin(grid::unit(0,"cm")),
        legend.text=element_text(colour=textcol,size=7),
        legend.key.height=grid::unit(0.8,"cm"),
        legend.key.width=grid::unit(0.2,"cm"),
        legend.title=element_blank(),
        axis.text.x=element_text(size=7,colour=textcol),
        axis.text.y=element_text(vjust=0.2,colour=textcol),
        axis.ticks=element_line(size=0.4),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.margin=margin(-0.2,0.4,0.1,0.2,"cm"),
        plot.title=element_text(colour=textcol,hjust=0,size=12))

#save figure
Var1 <- paste0("Fig_5.1_Interpol_Explosives_By_Year")
show(InterpolTopExplosivesPlot) 
ggsave(paste0(Figure.dir,sprintf("%s.tiff",Var1)), InterpolTopExplosivesPlot, width = 8, height = 8.5, units = "in", dpi=300)

#Export to top keywords list
write.csv(InterpolExplosivesbyYearCountGraph, file=paste0(Results.dir,sprintf("%s.csv",Var1)), row.names = F)

print("Processing complete. Please check 'Results/' and 'Figures/' folders for output")


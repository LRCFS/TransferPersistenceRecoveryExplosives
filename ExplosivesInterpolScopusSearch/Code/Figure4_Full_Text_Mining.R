#######################################################
#####           Interpol Full Text                #####
#####           Explosive Keywords                #####
#######################################################

names(ExplosivesCountSubset)[2] <- "Explosives from Full Text"
names(ExplosivesCountSubset)[3] <- "Explosives from Abstract, Title, and Keywords"
names(ExplosivesCountSubset)[4] <- "Explosives that make up more than % of mentions"

ExplosivesCountSubsetGraph <- ExplosivesCountSubset %>%
  pivot_longer(cols=c("Explosives from Full Text", "Explosives from Abstract, Title, and Keywords"),
               names_to="Source", values_to="Count")
ExplosivesCountSubsetGraph$Source <- factor(ExplosivesCountSubsetGraph$Source, levels = c("Explosives from Full Text", "Explosives from Abstract, Title, and Keywords"))
ExplosivesCountSubsetGraph <- subset(ExplosivesCountSubsetGraph, Source %in% c("Explosives from Full Text","Explosives from Abstract, Title, and Keywords"))

TopExplosivesOrder <- ExplosivesCountSubset %>%
  arrange(desc(`Explosives from Full Text`),(Explosive))

  ######## Graph ########
ExplosivesMentionsbySource <- ggplot(ExplosivesCountSubsetGraph)+
  geom_point(aes(x=factor(Explosive, levels = TopExplosivesOrder$Explosive), y=Count, color=Source, shape=Source), size=1) + 
  scale_shape_manual(values = c(15, 17)) +
  scale_color_manual(values = c("#969696","#636363"))+
  geom_point(aes(x=factor(Explosive, levels = TopExplosivesOrder$Explosive), y=((Ratio+1)*100)), color = "#525252", size=1, shape = 1) + 
  scale_y_continuous(sec.axis = sec_axis(transform = ~./100-1 , name = "Ratio of Counts"),limits=c(0,200))+
  labs(y="Number of Papers")+
  theme(legend.position="none",
        axis.text.y=element_text(size=6, vjust=0.2),
        axis.title.y=element_text(size=6),
        axis.ticks=element_line(size=0.4),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.margin=margin(0.2,0.2,0.2,0.2,"cm"),
        plot.title=element_text(colour=textcol,hjust=0,size=6),
        axis.text.x = element_text(size=6, angle = 90, hjust=1),
        axis.title.x = element_blank())

show(ExplosivesMentionsbySource)

#save figure
Var1 <- paste0("Fig_4_ExplosivesMentionsbySource")

ggsave(paste0(Figure.dir,sprintf("%s.tiff",Var1)), ExplosivesMentionsbySource, width = 9, height = 12, units = "cm", dpi=600)

print("Processing complete. Please check 'Figures/' folder for output")
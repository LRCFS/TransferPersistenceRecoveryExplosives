
#############################################################
#####                     countries                     #####
#############################################################

# get city/country data
data(world.cities)

#######################################################
###### INTERPOL Country Affiliation    ######
######              Figure 3                     ######
#######################################################
# load INTERPOL data

dat_csv_temp <- read.csv(file = "INTERPOL/INTERPOL_Combined.csv")

dat_csv_temp4 <- dat_csv_temp

dat_csv_temp4$Affiliations <- gsub("\\.", "", dat_csv_temp4$Affiliations)
dat_csv_temp4$Affiliations <- gsub("USA\\, United States", "USA", dat_csv_temp4$Affiliations)
dat_csv_temp4$Affiliations <- gsub("Russian Federation", "Russia", dat_csv_temp4$Affiliations)
dat_csv_temp4$Affiliations <- gsub("Cairo Egypt", "Cairo\\, Egypt", dat_csv_temp4$Affiliations)
dat_csv_temp4$Affiliations <- gsub("Hong Kong", "Hong Kong\\, China", dat_csv_temp4$Affiliations)

# replace "United States" with USA & "United Kingdom" with UK.
aff.lst <- gsub("United States", "USA", dat_csv_temp4$Affiliations, perl = TRUE)
aff.lst <- gsub("United Kingdom", "UK", aff.lst, perl = TRUE)
aff.lst <- gsub("South Korea", "Korea South", aff.lst, perl = TRUE)

# replace ';' with ',' as multiple affiliations are separated with ';'
# but that doesn't fit with the strsplit()
aff.lst <- gsub(";", ",", aff.lst)
# split fields by ", "
splt.lst <- sapply(aff.lst, strsplit, split = ", ", USE.NAMES = FALSE)
# extract fields which match a known city making sure that diacritics aren't a problem...
city.lst <- lapply(splt.lst, function(x)x[which(removeDiacritics(x) %in% world.cities$name)])
# ... or country
# cntry.lst <- lapply(splt.lst, function(x)x[which(removeDiacritics(x) %in% world.cities$country.etc)])
# this version only returns unique instances of countries per publication
cntry.lst <- lapply(splt.lst, function(x)unique(x[which(x %in% world.cities$country.etc)]))

## generate plot of papers per country
#threshold for Figure 
threshold <- 10

cntry.dat <- data.frame(Country = removeDiacritics(unlist(cntry.lst)), stringsAsFactors = FALSE)

# define continent for each country
cntry.dat$Continent <- countrycode(sourcevar = cntry.dat[, "Country"],
                                   origin = "country.name",
                                   destination = "continent")

# get countries under threshold
other.dat <- cntry.dat %>% 
  group_by(Country, Continent) %>% 
  dplyr::summarise(Count = dplyr::n()) %>% 
  filter(Count <= threshold)
# aggregate counts as 'Others'
other.dat <- data.frame(Country = "Others", Continent = "Other", Count = sum(other.dat$Count))

# order by count
other.dat$Country <- reorder(other.dat$Country, other.dat$Count)
other.dat <- as.data.frame(other.dat)

# Collate counts for countries over threshold
cntry.dat <- cntry.dat %>% 
  group_by(Country, Continent) %>% 
  dplyr::summarise(Count = dplyr::n()) %>%  
  filter(Count > threshold)
# order by count
cntry.dat$Country <- reorder(cntry.dat$Country, cntry.dat$Count)
cntry.dat <- as.data.frame(cntry.dat)
# add in 'Others'
cntry.dat <- rbind(other.dat, cntry.dat)
# plot
p <-  ggplot(cntry.dat, (aes(x=Country, y=Count, fill=Continent))) + 
  geom_col() +
  scale_fill_manual(values = c("gray", brewer.pal(5, "Set1")), breaks = c("Africa", "Americas", "Asia", "Europe", "Oceania", "Other")) +
  xlab('Country Affiliation') +
  ylab('Total Papers') +
  coord_flip() +
  theme_minimal() +
  theme(text = element_text(family = "Arial"),
        legend.position="right",legend.direction="vertical",
        legend.title=element_text(colour=textcol, size = 8),
        legend.margin=margin(grid::unit(0,"cm")),
        legend.text=element_text(colour=textcol,size=7),
        legend.key.height=grid::unit(0.8,"cm"),
        legend.key.width=grid::unit(0.2,"cm"),
        axis.text.x=element_text(size=6,colour=textcol),
        axis.title = element_text(size = 8),
        axis.text.y=element_text(size=6,vjust=0.2,colour=textcol),
        axis.ticks=element_line(size=0.4),
        plot.background=element_blank(),  # element_rect(fill, colour, size, linetype, color))
        panel.border=element_blank(),
        plot.margin=margin(0.2,0.4,0.1,0.2,"cm"),
        plot.title=element_text(colour=textcol,hjust=0,size=6))+ labs(fill = "Region") # change Continent to region
p <- p+theme(legend.position = c(0.9, 0.5))

show(p)

#save figure
Var1 <- paste0("Fig_3_INTERPOL_Country_Affiliation")

ggsave(paste0(Figure.dir,sprintf("%s.tiff",Var1)), p, width = 3.3, height = 4.5, units = "in", dpi=300)

print("Processing complete. Please check 'Figures/' folder for output")

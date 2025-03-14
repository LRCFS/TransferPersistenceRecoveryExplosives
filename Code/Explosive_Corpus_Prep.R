#############################################################
#####     Format and Correct Explosive Corpus           #####
#############################################################

ExplosiveList$UncorrectedNoSpecials <- ExplosiveList$Uncorrected.Explosive
ExplosiveList$UncorrectedNoSpecials <- paste0(" ", ExplosiveList$UncorrectedNoSpecials)
ExplosiveList$UncorrectedNoSpecials<- gsub('[^[:alnum:] ]', ' ', ExplosiveList$UncorrectedNoSpecials)
ExplosiveList$UncorrectedNoSpecials <- gsub("\\s+", " ", ExplosiveList$UncorrectedNoSpecials)

# Remove trailing (right) whitespace and make lowercase
ExplosiveList$UncorrectedNoSpecials <- trimws(ExplosiveList$UncorrectedNoSpecials, which = c("right"))
ExplosiveList$UncorrectedNoSpecials <- toupper(ExplosiveList$UncorrectedNoSpecials)

ExplosiveList$UncorrectedNoSpaces <- ExplosiveList$UncorrectedNoSpecials
ExplosiveList$UncorrectedNoSpaces <- trimws(ExplosiveList$UncorrectedNoSpaces, which = c("both"))

ExplosiveList$Colour <- ExplosiveList$No.Practical.Use.
ExplosiveList$Colour <-gsub('No Practical Use','tomato',ExplosiveList$Colour)
ExplosiveList$Colour <- replace_na(ExplosiveList$Colour,'black')

# Convert to Object
ExplosiveListString <- ExplosiveList$UncorrectedNoSpecials

#Get distinct explosives list
ExplosiveListDistinct <- data.frame(ExplosiveList$Corrected.Explosive) %>%
  distinct()
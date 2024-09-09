#######################################################
#####       Scopus Explosive Keywords           #####
#######################################################
#######################################################
#######                For abstract               #####
#######################################################
# Check for presence of each word in Abstract
ScopusAbs <- vector("list", nrow(Scopus_data))

for(d in 1:nrow(Scopus_data)){
  for(w in seq_along(ExplosiveListString)){
    intermed   <- grep(ExplosiveListString[[w]], Scopus_data[[d,"Abstract"]])
    ScopusAbs[[d]] <- c(ScopusAbs[[d]], 
                          ExplosiveListString[[w]][ (length(intermed) > 0) ])
  }
}

# combined together
ScopusExplosivesFromAbs <- sapply(ScopusAbs, paste0, collapse=";")
ScopusExplosivesFromAbs <- as.data.frame(ScopusExplosivesFromAbs)

ScopusExplosivesFromAbs <- data.frame(Scopus_data$EID,ScopusExplosivesFromAbs)

# remane fist column to match original dataset 
names(ScopusExplosivesFromAbs)[1] <- c("EID")

# #########################################################
# #####                For title                      #####
##############################################
# Check for presence of each word in Title
ScopusTitles <- vector("list", nrow(Scopus_data))

for(d in 1:nrow(Scopus_data)){
  for(w in seq_along(ExplosiveListString)){
    intermed   <- grep(ExplosiveListString[[w]], Scopus_data[[d,"Title"]])
    ScopusTitles[[d]] <- c(ScopusTitles[[d]], 
                             ExplosiveListString[[w]][ (length(intermed) > 0) ])
  }
}

# combined together
ScopusExplosivesFromTitle    <- sapply(ScopusTitles, paste0, collapse=";")
ScopusExplosivesFromTitle <- as.data.frame(ScopusExplosivesFromTitle)

ScopusExplosivesFromTitle <- data.frame(Scopus_data$EID,ScopusExplosivesFromTitle)

# remane fist column to match original dataset 
names(ScopusExplosivesFromTitle)[1] <- c("EID")

ScopusExplosivesFromAbsTitle <- full_join(ScopusExplosivesFromAbs,ScopusExplosivesFromTitle)

# #########################################################
# #####                For keywords                   #####
##############################################
# Check for presence of each word in Keywords
ScopusKeywords <- vector("list", nrow(Scopus_data))

for(d in 1:nrow(Scopus_data)){
  for(w in seq_along(ExplosiveListString)){
    intermed   <- grep(ExplosiveListString[[w]], Scopus_data[[d,"KeywordsCorrected"]])
    ScopusKeywords[[d]] <- c(ScopusKeywords[[d]], 
                               ExplosiveListString[[w]][ (length(intermed) > 0) ])
  }
}

# combined together
ScopusExplosivesFromKeyword    <- sapply(ScopusKeywords, paste0, collapse=";")
ScopusExplosivesFromKeyword <- as.data.frame(ScopusExplosivesFromKeyword)

ScopusExplosivesFromKeyword <- data.frame(Scopus_data$EID,ScopusExplosivesFromKeyword)

# remane fist column to match original dataset 
names(ScopusExplosivesFromKeyword)[1] <- c("EID")

ScopusExplosives <- full_join(ScopusExplosivesFromAbsTitle,ScopusExplosivesFromKeyword)

############
# Combine Columns Abs,Titles,Keywords and place in Column name "Explosive_List" and remove original columns
ScopusExplosives <- ScopusExplosives %>%
  unite("Explosive_List", ScopusExplosivesFromAbs, ScopusExplosivesFromTitle, ScopusExplosivesFromKeyword,sep = ";", remove = TRUE)

#Split Column "Explosive_List" in row by the separator ";", remove leading white space to generate list
ScopusExplosives <- ScopusExplosives %>% 
  mutate(Explosive_List = strsplit(as.character(Explosive_List), ";")) %>% 
  unnest(Explosive_List) %>%
  mutate_if(is.character, str_trim)

# Apply corrections to Scopus Explosive List
ScopusExplosives$Explosive_List_Corrected <- gsr(as.character(ScopusExplosives$Explosive_List),as.character(ExplosiveList$UncorrectedNoSpaces),as.character(ExplosiveList$Corrected.Explosive))

# Remove duplicates from explosive list
ScopusExplosives <- ScopusExplosives %>%
  select(EID,Explosive_List_Corrected)%>%
  distinct()

#Group keywords together
ScopusExplosivesCollapsed <- ScopusExplosives %>% 
  group_by(EID) %>%
  summarize(Explosive_List_Corrected=paste0(Explosive_List_Corrected, collapse = ";"))

############
# get the Title and respective country names to join to the ScopusExplosives
Scopus_Title_Country <- Scopus_data %>%
  select(Title,Year,Country,EID)

ScopusExplosives <- full_join(ScopusExplosivesCollapsed,Scopus_Title_Country, by = "EID")

print('Scopus Explosive Keywords Extracted')

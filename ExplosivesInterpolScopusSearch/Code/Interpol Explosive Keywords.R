#######################################################
#####       Interpol Explosive Keywords           #####
#######################################################
#######################################################
#######                For abstract               #####
#######################################################
# Check for presence of each word in Abstract
InterpolAbs <- vector("list", nrow(Interpol_data))

for(d in 1:nrow(Interpol_data)){
  for(w in seq_along(ExplosiveListString)){
    intermed   <- grep(ExplosiveListString[[w]], Interpol_data[[d,"Abstract"]])
    InterpolAbs[[d]] <- c(InterpolAbs[[d]], 
                          ExplosiveListString[[w]][ (length(intermed) > 0) ])
  }
}

# combined together
InterpolExplosivesFromAbs <- sapply(InterpolAbs, paste0, collapse=";")
InterpolExplosivesFromAbs <- as.data.frame(InterpolExplosivesFromAbs)

InterpolExplosivesFromAbs <- data.frame(Interpol_data$Title,InterpolExplosivesFromAbs)

# remane fist column to match original dataset 
names(InterpolExplosivesFromAbs)[1] <- c("Title")

# #########################################################
# #####                For title                      #####
##############################################
# Check for presence of each word in Title
InterpolTitles <- vector("list", nrow(Interpol_data))

for(d in 1:nrow(Interpol_data)){
  for(w in seq_along(ExplosiveListString)){
    intermed   <- grep(ExplosiveListString[[w]], Interpol_data[[d,"Title"]])
    InterpolTitles[[d]] <- c(InterpolTitles[[d]], 
                             ExplosiveListString[[w]][ (length(intermed) > 0) ])
  }
}

# combined together
InterpolExplosivesFromTitle    <- sapply(InterpolTitles, paste0, collapse=";")
InterpolExplosivesFromTitle <- as.data.frame(InterpolExplosivesFromTitle)

InterpolExplosivesFromTitle <- data.frame(Interpol_data$Title,InterpolExplosivesFromTitle)

# remane fist column to match original dataset 
names(InterpolExplosivesFromTitle)[1] <- c("Title")

InterpolExplosivesFromAbsTitle <- full_join(InterpolExplosivesFromAbs,InterpolExplosivesFromTitle)

# #########################################################
# #####                For keywords                   #####
##############################################
# Check for presence of each word in Keywords
InterpolKeywords <- vector("list", nrow(Interpol_data))

for(d in 1:nrow(Interpol_data)){
  for(w in seq_along(ExplosiveListString)){
    intermed   <- grep(ExplosiveListString[[w]], Interpol_data[[d,"KeywordsCorrected"]])
    InterpolKeywords[[d]] <- c(InterpolKeywords[[d]], 
                               ExplosiveListString[[w]][ (length(intermed) > 0) ])
  }
}

# combined together
InterpolExplosivesFromKeyword    <- sapply(InterpolKeywords, paste0, collapse=";")
InterpolExplosivesFromKeyword <- as.data.frame(InterpolExplosivesFromKeyword)

InterpolExplosivesFromKeyword <- data.frame(Interpol_data$Title,InterpolExplosivesFromKeyword)

# remane fist column to match original dataset 
names(InterpolExplosivesFromKeyword)[1] <- c("Title")

InterpolExplosives <- full_join(InterpolExplosivesFromAbsTitle,InterpolExplosivesFromKeyword)
#write.csv(InterpolExplosives,file = "INTERPOLExplosives.csv",row.names = TRUE)

############
# Combine Columns Abs,Titles,Keywords and place in Column name "Explosive_List" and remove original columns
InterpolExplosives <- InterpolExplosives %>%
  unite("Explosive_List", InterpolExplosivesFromAbs, InterpolExplosivesFromTitle, InterpolExplosivesFromKeyword,sep = ";", remove = TRUE)

#Split Column "Explosive_List" in row by the separator ";", remove leading white space to generate list
InterpolExplosives <- InterpolExplosives %>% 
  mutate(Explosive_List = strsplit(as.character(Explosive_List), ";")) %>% 
  unnest(Explosive_List) %>%
  mutate_if(is.character, str_trim)

# Apply corrections to Interpol Explosive List
InterpolExplosives$Explosive_List_Corrected <- gsr(as.character(InterpolExplosives$Explosive_List),as.character(ExplosiveList$UncorrectedNoSpaces),as.character(ExplosiveList$Corrected.Explosive))

# Remove duplicates from explosive list
InterpolExplosives <- InterpolExplosives %>%
  select(Title,Explosive_List_Corrected)%>%
  distinct()

#Group keywords together
InterpolExplosivesCollapsed <- InterpolExplosives %>% 
  group_by(Title) %>%
  summarize(Explosive_List_Corrected=paste0(Explosive_List_Corrected, collapse = ";"))

############
# get the Title and respective country names to join to the InterpolExplosives
Interpol_Title_Country <- Interpol_data %>%
  select(Title,Year,Country)

InterpolExplosives <- full_join(InterpolExplosivesCollapsed,Interpol_Title_Country, by = "Title")

print('Interpol Explosive Keywords Extracted')

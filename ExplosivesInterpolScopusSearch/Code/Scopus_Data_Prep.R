#############################################################
#####          Load and Format Scopus Data              #####
#############################################################
#####      Generate Scopus combined list                #####

#Scopus combined list is too large to be hosted on GitHub, and is instead present as a compressed file (Scopus_Combined.zip)
#This will check if the combined data is present and has already been extracted from the zip file
if (file.exists("ScopusOutputs/Scopus_Combined.csv",recursive = TRUE)){
  print("SCOPUS data already combined")
  Scopus_data <- read.csv(file = "ScopusOutputs/Scopus_Combined.csv")

#This will check if the zip file is present, and if so extract the Scopus_Combined.csv file
}else if (file.exists("ScopusCompressed/Scopus_Combined.zip",recursive = TRUE)){
  unzip("ScopusCompressed/Scopus_Combined.zip", exdir = "ScopusOutputs/")
  Scopus_data <- read.csv(file = "ScopusOutputs/Scopus_Combined.csv")
  print("SCOPUS data extracted")

#If the Scopus combined data is not present,  this will combine all csv files in the ScopusInputs folder to create it
}else{
Scopus_data <- list.files(cit.path.ScopusInputs, pattern=extension, full.names=TRUE)
Scopus_data <- rbindlist(lapply(Scopus_data,fread, encoding='UTF-8')) %>%
  select("Authors","Title","Year","Source title","DOI","Affiliations","Abstract","Author Keywords","Index Keywords","EID") %>%
  distinct()
  write.csv(Scopus_data,file = "ScopusOutputs/Scopus_Combined.csv",row.names = FALSE)
  print("Scopus data has been combined")
}

names(Scopus_data)[3] <- c("Year")

#############################################################
##### Correct Country Names and Create Affiliations List ####
#############################################################
## Using Scopus data, apply corrections to countries

Scopus_data$Affiliations <- gsub("\\.", "", Scopus_data$Affiliations)
Scopus_data$Affiliations <- gsub("USA\\, United States", "USA", Scopus_data$Affiliations)
Scopus_data$Affiliations <- gsub("Russian Federation", "Russia", Scopus_data$Affiliations)
Scopus_data$Affiliations <- gsub("Cairo Egypt", "Cairo\\, Egypt", Scopus_data$Affiliations)
Scopus_data$Affiliations <- gsub("Hong Kong", "Hong Kong\\, China", Scopus_data$Affiliations)
Scopus_data$Affiliations <- gsub("United States", "USA", Scopus_data$Affiliations, perl = TRUE)
Scopus_data$Affiliations <- gsub("United Kingdom", "UK", Scopus_data$Affiliations, perl = TRUE)
Scopus_data$Affiliations <- gsub("South Korea", "Korea South", Scopus_data$Affiliations, perl = TRUE)

# replace ';' with ',' as multiple affiliations are separated with ';'
ScopusAffiliations <- Scopus_data
ScopusAffiliations <- gsub(";", ",", ScopusAffiliations$Affiliations)

# split fields by ", "
ScopusAffiliations <- sapply(ScopusAffiliations, strsplit, split = ", ", USE.NAMES = FALSE)

# extract fields which match a known city making sure that diacritics aren't a problem...

ScopusCityList <- lapply(ScopusAffiliations, function(x)x[which(removeDiacritics(x) %in% world.cities$name)])

# ... or country
ScopusCountryList <- lapply(ScopusAffiliations, function(x)x[which(removeDiacritics(x) %in% world.cities$country.etc)])

# extract unique instances of countries per publication
ScopusCountryListUnique <- lapply(ScopusAffiliations, function(x)unique(x[which(x %in% world.cities$country.etc)]))

#extract the list of country per paper and place in dataframe
ScopusCountryListbyPaperUnique <- as.data.table(matrix(ScopusCountryListUnique))

# bind to the original data
Scopus_data<- cbind(Scopus_data,ScopusCountryListbyPaperUnique)

# rename Country column
names(Scopus_data)[11] <- c("Country")

#convert to character
Scopus_data$Country <- as.character(Scopus_data$Country)

# remove unwanted characters  
Scopus_data$Country <-  gsub("c\\(","",Scopus_data$Country)
Scopus_data$Country <-  gsub("\\)","",Scopus_data$Country)
Scopus_data$Country <-  gsub("\"","",Scopus_data$Country)
Scopus_data$Country <-  gsub(", ",",",Scopus_data$Country)
Scopus_data$Country <-  gsub("character\\(0",NA,Scopus_data$Country)

#Remove special characters and extra white spaces from Abstract, Title and AI Keywords
Scopus_data$Abstract <- gsub('[^[:alnum:] ]', ' ', Scopus_data$Abstract)
Scopus_data$Abstract <- gsub("\\s+", " ", Scopus_data$Abstract)
Scopus_data$Abstract <- toupper(Scopus_data$Abstract)

Scopus_data$Title <- gsub('[^[:alnum:] ]', ' ', Scopus_data$Title)
Scopus_data$Title <- gsub("\\s+", " ", Scopus_data$Title)
Scopus_data$Title <- toupper(Scopus_data$Title)
Scopus_data$Title <- trimws(Scopus_data$Title)

#############################################################
#####           Scopus keyword selection             #####
#############################################################

if (KeywordEntries == "Author_Keywords"){
  #   Author Keywords only
  names(Scopus_data) <- sub("Author.Keywords","AIKeywords", names(Scopus_data))
  Keyname <- "A_Keywords"
} else{
  if (KeywordEntries == "Database_Keywords"){
    #   Index Keywords only
    names(Scopus_data) <- sub("Index.Keywords","AIKeywords", names(Scopus_data))
    Keyname <- "I_Keywords"
  }
  else {
    # Index and Author Keywords
    # Combine Columns Author.Keywords and Index.Keywords and place in Column name "AIKeywords" and remove original columns
    Scopus_data <- Scopus_data %>%
      unite("AIKeywords", Author.Keywords, Index.Keywords,sep = ";", remove = TRUE)
    Keyname <- "AI_Keywords"
  }}

#Split Column "AIKeywords" in row by the separator ";", remove leading white space to generate list
ScopusKeywordList <- Scopus_data %>% 
  mutate(AIKeywords = strsplit(as.character(AIKeywords), ";")) %>% 
  unnest(AIKeywords) %>%
  mutate_if(is.character, str_trim)

# Upper case "AIKeywords" in "DatasetKeywordList" and save in dataframe
ScopusKeywordList$AIKeywords <- toupper(ScopusKeywordList$AIKeywords)

# Extract list of "AIkeywords" and remove duplicate
ScopusDistinctKeywordList <- ScopusKeywordList %>%
  select(AIKeywords) %>%
  distinct()

# Apply corrections to Scopus Keyword list
ScopusKeywordList$KeywordsCorrected <- gsr(as.character(ScopusKeywordList$AIKeywords),as.character(KeywordCorrectionList$AIKeywords),as.character(KeywordCorrectionList$CorAIKeywordsAcronym))

#Save Scopus keyword list
write.csv(ScopusKeywordList, file=paste0(cit.path.ScopusOutputs,sprintf("%s.csv","Scopus_Keyword_List")), row.names = F)

# number of distinct Keywords after correction
ScopusDistinctKeywordListCorrected <- ScopusKeywordList %>%
  select(KeywordsCorrected) %>%
  distinct()

#Group keywords by paper
ScopusKeywordListCollapsed <- ScopusKeywordList %>% 
  group_by(Title) %>%
  summarize(KeywordsCorrected=paste0(KeywordsCorrected, collapse = ", "))

ScopusKeywordListCollapsed$Title <- trimws(ScopusKeywordListCollapsed$Title)

Scopus_data <- merge(Scopus_data, ScopusKeywordListCollapsed, all = TRUE)

Scopus_data$KeywordsCorrected <- gsub('[^[:alnum:] ]', ' ', Scopus_data$KeywordsCorrected)
Scopus_data$KeywordsCorrected <- gsub("\\s+", " ", Scopus_data$KeywordsCorrected)

ScopusAuthKey <- Scopus_data
ScopusAuthKey$AIKeywords[ScopusAuthKey$AIKeywords==""] <- NA
ScopusAuthKey <- ScopusAuthKey %>%
  filter(!is.na(AIKeywords))

write.csv(Scopus_data, file=paste0(cit.path.ScopusOutputs,sprintf("%s.csv","Scopus_processed_data")), row.names = F)

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

print("Scopus data prepared for figure generation")
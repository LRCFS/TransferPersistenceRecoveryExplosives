#############################################################
#####               Load and Format Data                #####
#############################################################
#####      Generate SCOPUS combined list              #####

if (file.exists("SCOPUS/SCOPUS_Combined.csv",recursive = TRUE)){
  print("SCOPUS data already combined")
  Scopus_data <- read.csv(file = "SCOPUS/SCOPUS_Combined.csv")
}else{Scopus_data <- list.files(cit.path.SCOPUS, pattern=extension, full.names=TRUE)
Scopus_data <- rbindlist(lapply(Scopus_data,fread, encoding='UTF-8'))
write.csv(Scopus_data,file = "SCOPUS/SCOPUS_Combined.csv",row.names = TRUE)
print("SCOPUS data has been combined")
Scopus_data <- read.csv(file = "SCOPUS/SCOPUS_Combined.csv")
}

names(Scopus_data)[3] <- c("AuthorID")

Scopus_data <- Scopus_data %>%
  select(Year,Title,Source.title,Authors,Affiliations,AuthorID,Abstract,Author.Keywords,Index.Keywords,EID,DOI)%>%
  distinct()

Scopus_data <- Scopus_data %>%
  filter(between(Year,2001,2022)) 

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
Scopus_data$Affiliations <- gsub(";", ",", Scopus_data$Affiliations)

# split fields by ", "
ScopusAffiliations <- sapply(Scopus_data$Affiliations, strsplit, split = ", ", USE.NAMES = FALSE)

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
names(Scopus_data)[12] <- c("Country")

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

#### Split Explosive List by Military/Commercial and Home-made #####


print("Scopus data prepared for figure generation")


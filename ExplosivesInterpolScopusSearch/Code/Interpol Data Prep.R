#############################################################
#####               Load and Format Data                #####
#############################################################
#####      Generate INTERPOL combined list              #####

if (file.exists("INTERPOL/INTERPOL_Combined.csv",recursive = TRUE)){
  print("INTERPOL data already combined")
  Interpol_data <- read.csv(file = "INTERPOL/INTERPOL_Combined.csv")
}else{Interpol_data <- list.files(cit.path.INTERPOL, pattern=extension, full.names=TRUE)
Interpol_data <- rbindlist(lapply(Interpol_data,fread, encoding='UTF-8'))
write.csv(Interpol_data,file = "INTERPOL/INTERPOL_Combined.csv",row.names = TRUE)
print("INTERPOL data has been combined")
Interpol_data <- read.csv(file = "INTERPOL/INTERPOL_Combined.csv")
}

Interpol_data <- Interpol_data %>%
  select(Year,Title,Source.title,Authors,Affiliations,AuthorID,Abstract,Author.Keywords,Index.Keywords,EID,DOI)%>%
  distinct()

Interpol_data <- Interpol_data %>%
  filter(between(Year,2001,2022)) 

#############################################################
##### Correct Country Names and Create Affiliations List ####
#############################################################
## Using Interpol data, apply corrections to countries

Interpol_data$Affiliations <- gsub("\\.", "", Interpol_data$Affiliations)
Interpol_data$Affiliations <- gsub("USA\\, United States", "USA", Interpol_data$Affiliations)
Interpol_data$Affiliations <- gsub("Russian Federation", "Russia", Interpol_data$Affiliations)
Interpol_data$Affiliations <- gsub("Cairo Egypt", "Cairo\\, Egypt", Interpol_data$Affiliations)
Interpol_data$Affiliations <- gsub("Hong Kong", "Hong Kong\\, China", Interpol_data$Affiliations)
Interpol_data$Affiliations <- gsub("United States", "USA", Interpol_data$Affiliations, perl = TRUE)
Interpol_data$Affiliations <- gsub("United Kingdom", "UK", Interpol_data$Affiliations, perl = TRUE)
Interpol_data$Affiliations <- gsub("South Korea", "Korea South", Interpol_data$Affiliations, perl = TRUE)

# replace ';' with ',' as multiple affiliations are separated with ';'
Interpol_data$Affiliations <- gsub(";", ",", Interpol_data$Affiliations)

# split fields by ", "
InterpolAffiliations <- sapply(Interpol_data$Affiliations, strsplit, split = ", ", USE.NAMES = FALSE)

# extract fields which match a known city making sure that diacritics aren't a problem...
InterpolCityList <- lapply(InterpolAffiliations, function(x)x[which(removeDiacritics(x) %in% world.cities$name)])

# ... or country
InterpolCountryList <- lapply(InterpolAffiliations, function(x)x[which(removeDiacritics(x) %in% world.cities$country.etc)])

# extract unique instances of countries per publication
InterpolCountryListUnique <- lapply(InterpolAffiliations, function(x)unique(x[which(x %in% world.cities$country.etc)]))

#extract the list of country per paper and place in dataframe
InterpolCountryListbyPaperUnique <- as.data.table(matrix(InterpolCountryListUnique))

# bind to the original data
Interpol_data<- cbind(Interpol_data,InterpolCountryListbyPaperUnique)

# rename Country column
names(Interpol_data)[12] <- c("Country")

#convert to character
Interpol_data$Country <- as.character(Interpol_data$Country)

# remove unwanted characters  
Interpol_data$Country <-  gsub("c\\(","",Interpol_data$Country)
Interpol_data$Country <-  gsub("\\)","",Interpol_data$Country)
Interpol_data$Country <-  gsub("\"","",Interpol_data$Country)
Interpol_data$Country <-  gsub(", ",",",Interpol_data$Country)
Interpol_data$Country <-  gsub("character\\(0",NA,Interpol_data$Country)

#Remove special characters and extra white spaces from Abstract, Title and AI Keywords
Interpol_data$Abstract <- gsub('[^[:alnum:] ]', ' ', Interpol_data$Abstract)
Interpol_data$Abstract <- gsub("\\s+", " ", Interpol_data$Abstract)
Interpol_data$Abstract <- toupper(Interpol_data$Abstract)

Interpol_data$Title <- gsub('[^[:alnum:] ]', ' ', Interpol_data$Title)
Interpol_data$Title <- gsub("\\s+", " ", Interpol_data$Title)
Interpol_data$Title <- toupper(Interpol_data$Title)
Interpol_data$Title <- trimws(Interpol_data$Title)

#############################################################
#####           Interpol keyword selection             #####
#############################################################

if (KeywordEntries == "Author_Keywords"){
  #   Author Keywords only
  names(Interpol_data) <- sub("Author.Keywords","AIKeywords", names(Interpol_data))
  Keyname <- "A_Keywords"
} else{
  if (KeywordEntries == "Database_Keywords"){
    #   Index Keywords only
    names(Interpol_data) <- sub("Index.Keywords","AIKeywords", names(Interpol_data))
    Keyname <- "I_Keywords"
  }
  else {
    # Index and Author Keywords
    # Combine Columns Author.Keywords and Index.Keywords and place in Column name "AIKeywords" and remove original columns
    Interpol_data <- Interpol_data %>%
      unite("AIKeywords", Author.Keywords, Index.Keywords,sep = ";", remove = TRUE)
    Keyname <- "AI_Keywords"
  }}

#Split Column "AIKeywords" in row by the separator ";", remove leading white space to generate list
InterpolKeywordList <- Interpol_data %>% 
  mutate(AIKeywords = strsplit(as.character(AIKeywords), ";")) %>% 
  unnest(AIKeywords) %>%
  mutate_if(is.character, str_trim)

# Upper case "AIKeywords" in "DatasetKeywordList" and save in dataframe
InterpolKeywordList$AIKeywords <- toupper(InterpolKeywordList$AIKeywords)

# Extract list of "AIkeywords" and remove duplicate
InterpolDistinctKeywordList <- InterpolKeywordList %>%
  select(AIKeywords) %>%
  distinct()

# Apply corrections to Interpol Keyword list
InterpolKeywordList$KeywordsCorrected <- gsr(as.character(InterpolKeywordList$AIKeywords),as.character(KeywordCorrectionList$AIKeywords),as.character(KeywordCorrectionList$CorAIKeywordsAcronym))

# number of distinct Keywords after correction
InterpolDistinctKeywordListCorrected <- InterpolKeywordList %>%
  select(KeywordsCorrected) %>%
  distinct()

#Group keywords by paper
InterpolKeywordListCollapsed <- InterpolKeywordList %>% 
  group_by(Title) %>%
  summarize(KeywordsCorrected=paste0(KeywordsCorrected, collapse = ", "))

InterpolKeywordListCollapsed$Title <- trimws(InterpolKeywordListCollapsed$Title)

Interpol_data <- merge(Interpol_data, InterpolKeywordListCollapsed, all = TRUE)

Interpol_data$KeywordsCorrected <- gsub('[^[:alnum:] ]', ' ', Interpol_data$KeywordsCorrected)
Interpol_data$KeywordsCorrected <- gsub("\\s+", " ", Interpol_data$KeywordsCorrected)

#### Split Explosive List by Military/Commercial and Home-made #####


print("Interpol data prepared for figure generation")

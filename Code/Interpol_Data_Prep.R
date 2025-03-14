#############################################################
#####       Load and Format Interpol Data               #####
#############################################################
#####      Generate Interpol combined list              #####

if (file.exists("InterpolOutputs/Interpol_Combined.csv",recursive = TRUE)){
  print("Interpol data already combined")
  Interpol_data <- read.csv(file = "InterpolOutputs/Interpol_Combined.csv")
}else{
  Interpol_data <- list.files(cit.path.InterpolInputs, pattern=extension, full.names=TRUE)
Interpol_data <- rbindlist(lapply(Interpol_data,fread, encoding='UTF-8'))
write.csv(Interpol_data,file = "InterpolOutputs/Interpol_Combined.csv",row.names = FALSE)
print("Interpol data has been combined")
}

colnames(Interpol_data)[4] <- "AuthorID"

Interpol_data <- Interpol_data %>%
  select(Year,Title,Source.title,Authors,Affiliations,AuthorID,Abstract,Author.Keywords,Index.Keywords,EID,DOI)%>%
  distinct(Title, .keep_all=TRUE)

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

InterpolKeywordList$AIKeywords[InterpolKeywordList$AIKeywords==""] <- NA
InterpolKeywordList <- InterpolKeywordList[complete.cases(InterpolKeywordList[ ,8]),]

# Extract list of "AIkeywords" and remove duplicate
InterpolDistinctKeywordList <- InterpolKeywordList %>%
  select(AIKeywords) %>%
  distinct()

# Apply corrections to Interpol Keyword list
InterpolKeywordList$KeywordsCorrected <- gsr(as.character(InterpolKeywordList$AIKeywords),as.character(KeywordCorrectionList$AIKeywords),as.character(KeywordCorrectionList$CorrectedAIKeywords))

#Save Interpol keyword list
write.csv(InterpolKeywordList, file=paste0(cit.path.InterpolOutputs,sprintf("%s.csv","Interpol_Keyword_List")), row.names = F)

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

write.csv(Interpol_data, file=paste0(cit.path.InterpolOutputs,sprintf("%s.csv","Interpol_processed_data")), row.names = F)

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

InterpolExplosivesFromAbsTitle <- full_join(InterpolExplosivesFromAbs,InterpolExplosivesFromTitle, relationship = "many-to-many")

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

InterpolExplosives <- full_join(InterpolExplosivesFromAbsTitle,InterpolExplosivesFromKeyword, relationship = "many-to-many")

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

write.csv(InterpolExplosives, file=paste0(cit.path.InterpolOutputs,sprintf("%s.csv","Interpol_Explosives")), row.names = F)

#############################################################
#####  Load and Process Interpol Full Text Data         #####
#############################################################
#This section requires the papers to be search to be downloaded to be saved in a folder named "Papers" in the directory
#To download Propellants, Explosives, Pyrotechnics papers using the Wiley API
#First obtain an API token here
#https://onlinelibrary.wiley.com/library-info/resources/text-and-datamining
#Note: academic institutional access is required
#Define API token
#APIToken <- '###############################'
#Then the following code can be used to automatically download the papers
#source("Code/Journal_Paper_Downloads.R")

if(file.exists("Papers",recursive = TRUE)){
  
# Step 1: Define your specific corpus (keywords or phrases) to search for
ExplosiveKeywords <- as.character(ExplosiveList$Uncorrected.Explosive)
ExplosiveKeywords <-tolower(ExplosiveKeywords)
ExplosiveKeywords <- unique(ExplosiveKeywords)

#Define tokenizer function to allow NGram from n=1 to n=3
BigramTokenizer <- function(ExplosiveKeywords) NGramTokenizer(ExplosiveKeywords, Weka_control(min=1, max=3))

########## Get Keywords from full text ######
# Step 2: Get list of files
FileNames <- list.files(path = "Papers/",pattern = ".pdf", full.name = TRUE)
FileNames <- gsub('//','/',FileNames)
FileNamesdf <- data_frame(FileNames)

# Step 3: Read and extract text from the PDF
for (d in 1:nrow(FileNamesdf)) {
  PaperText <- pdf_text(FileNames[d])
  
  # Convert the text into a single character string (if the PDF has multiple pages)
  PaperText <- paste(PaperText, collapse = " ")
  
  # Step 4: Preprocess the text
  # Convert the text to a corpus
  PaperCorpus <- VCorpus(VectorSource(PaperText))
  
  # Step 5: Search for occurrences of these keywords in the text
  # Create a Term-Document Matrix for the keywords
  
  TDM <- TermDocumentMatrix(PaperCorpus, control = list(tokenize = BigramTokenizer, dictionary=ExplosiveKeywords))
  
  # Inspect the matrix to see the frequency of keywords
  inspect(TDM)
  
  # Extract the counts of each keyword
  keyword_counts <- rowSums(as.matrix(TDM))
  keyword_counts <- as.data.frame(keyword_counts)
  keyword_counts <- rownames_to_column(keyword_counts)
  
  keyword_counts <- subset(keyword_counts, keyword_counts!=0)
  if (dim(keyword_counts)[1] == 0) {
    
  }else {
    keyword_counts$DOI <- (FileNames[d])
    keyword_counts$DOI <- gsub("Papers/","",keyword_counts$DOI)
    keyword_counts$DOI <- gsub(".pdf","",keyword_counts$DOI)
    keyword_counts$DOI <- gsub("%2F","/",keyword_counts$DOI)
    
    if (exists("FullTextCount")){
      FullTextCount <- rbind(keyword_counts,FullTextCount)
    }else{FullTextCount <-keyword_counts
    }
    rm(keyword_counts)
  }
}

names(FullTextCount)[1] <- "FullTextExplosives"
names(FullTextCount)[2] <- "FullTextExplosivesCount"
names(FullTextCount)[3] <- "DOI"

FullTextCount$FullTextExplosives <- toupper(FullTextCount$FullTextExplosives)

# Apply corrections to Full Text Count
FullTextCount$FullTextExplosives <- gsr(as.character(FullTextCount$FullTextExplosives),as.character(ExplosiveList$Uncorrected.Explosive),as.character(ExplosiveList$Corrected.Explosive))
FullTextCount$FullTextExplosives <- gsr(as.character(FullTextCount$FullTextExplosives),as.character(ExplosiveList$UncorrectedNoSpaces),as.character(ExplosiveList$Corrected.Explosive))
FullTextCountReduced <- FullTextCount %>%
  select(FullTextExplosives,DOI) %>%
  group_by(DOI) %>%
  summarize(FullTextExplosives=paste0(FullTextExplosives, collapse = "; "))

FullTextExplo <- FullTextCount %>%
  group_by(FullTextExplosives, DOI) %>%
  summarise(Occurence=sum(FullTextExplosivesCount))
FullTextExplo$Occurence <- as.numeric(FullTextExplo$Occurence)

FullTextDOI <- data.frame(FullTextExplo$DOI)
FullTextDOI <- distinct(FullTextDOI)

# #To limit the explosive count to explosive with most mentions per paper
# for (i in 1:nrow(FullTextDOI)){
#   FullTextExplo_temp <- FullTextExplo %>%
#     filter(DOI == FullTextDOI[i,1])
#   FullTextExplo_temp <- ungroup(FullTextExplo_temp)
#   FullTextExplo_temp <- top_n(FullTextExplo_temp,1)
#   # if the merged dataset doesn't exist, create it
#   if (!exists("FullTextExploTop")){
#     FullTextExploTop <- FullTextExplo_temp
#     # if the merged dataset does exist, append to it
#   }else {FullTextExploTop <-rbind(FullTextExploTop, FullTextExplo_temp)
#   }
# }
# 
# FullTextExploTop <- aggregate(FullTextExploTop$FullTextExplosives, by=list(Explosive=FullTextExploTop$FullTextExplosives), FUN=length)
# names(FullTextExploTop)[2] <- c("Count")
# FullTextExploTop$Source <- "Top Explosive Per Paper"

#To limit the explosive count to only explosives with more than n mentions
n = 10
for (i in 1:nrow(FullTextDOI)){
  FullTextExplo_temp <- FullTextExplo %>%
    filter(DOI == FullTextDOI[i,1])
  FullTextMentions <- sum(FullTextExplo_temp$Occurence)
  FullTextExplo_temp$Relative <- FullTextExplo_temp$Occurence/FullTextMentions*100
  FullTextExplo_temp <- FullTextExplo_temp %>%
    filter(Relative > n)
  # if the merged dataset doesn't exist, create it
  if (!exists("FullTextExploTopn")){
    FullTextExploTopn <- FullTextExplo_temp
    # if the merged dataset does exist, append to it
  }else {FullTextExploTopn <-rbind(FullTextExploTopn, FullTextExplo_temp)
  }
}

FullTextExploTopn <- aggregate(FullTextExploTopn$FullTextExplosives, by=list(Explosive=FullTextExploTopn$FullTextExplosives), FUN=length)
names(FullTextExploTopn)[2] <- paste0("Explosives that make up more than ",n,"% of mentions")

######## Get Keywords from abstract, title and author keywords ##########
# Step 2: Get abstract, title and author keywords for searching
InterpolSelectedData <- read.csv(file = "InterpolOutputs/Interpol_Combined.csv")
InterpolSelectedData <- InterpolSelectedData %>%
  select(Title,Abstract,Author.Keywords,DOI,Source.title)%>%
  distinct()

InterpolSelectedData <- InterpolSelectedData[InterpolSelectedData$Source.title %in% "Propellants, Explosives, Pyrotechnics", ]

#Select if abstract, title or keywords
InterpolSelectedData$AbsTitleKeywords <- paste(InterpolSelectedData$Title, InterpolSelectedData$Abstract, InterpolSelectedData$Author.Keywords, sep=" ")

# Step 4: Preprocess the text
# Convert the text to a corpus
AbsTitleKeywordsSelected <- as.data.frame(InterpolSelectedData$AbsTitleKeywords)
names(AbsTitleKeywordsSelected)[1] <- "AbsTitleKeywords"

for (d in 1:nrow(AbsTitleKeywordsSelected)) {
  AbsTitleKeywords <- (AbsTitleKeywordsSelected$AbsTitleKeywords[d])
  
  AbsTitleKeywordsCorpus <- VCorpus(VectorSource(AbsTitleKeywords))
  
  # Step 5: Search for occurrences of these keywords in the text
  
  # Create a Term-Document Matrix for the keywords
  
  TDM <- TermDocumentMatrix(AbsTitleKeywordsCorpus, control = list(tokenize = BigramTokenizer, dictionary=ExplosiveKeywords))
  
  # Inspect the matrix to see the frequency of keywords
  inspect(TDM)
  
  # Extract the counts of each keyword
  keyword_counts <- rowSums(as.matrix(TDM))
  keyword_counts <- as.data.frame(keyword_counts)
  keyword_counts <- rownames_to_column(keyword_counts)
  
  keyword_counts <- subset(keyword_counts, keyword_counts!=0)
  if (dim(keyword_counts)[1] == 0) {
    
  }else {
    keyword_counts$DOI <- (InterpolSelectedData$DOI[d])
    keyword_counts$DOI <- gsub("Papers/","",keyword_counts$DOI)
    keyword_counts$DOI <- gsub(".pdf","",keyword_counts$DOI)
    keyword_counts$DOI <- gsub("%2F","/",keyword_counts$DOI)
    
    if (exists("AbsTitleKeywordsCount")){
      AbsTitleKeywordsCount <- rbind(keyword_counts,AbsTitleKeywordsCount)
    }else{AbsTitleKeywordsCount <-keyword_counts
    }
    rm(keyword_counts)
  }
}

names(AbsTitleKeywordsCount)[1] <- "AbsTitleKeywordsExplosives"
names(AbsTitleKeywordsCount)[2] <- "AbsTitleKeywordsExplosivesCount"
names(AbsTitleKeywordsCount)[3] <- "DOI"

TotalATKMentions = sum(AbsTitleKeywordsCount$AbsTitleKeywordsExplosivesCount)

AbsTitleKeywordsCount$AbsTitleKeywordsExplosives <- toupper(AbsTitleKeywordsCount$AbsTitleKeywordsExplosives)

# Apply corrections to Abstract, Title, Keywords Count
AbsTitleKeywordsCount$AbsTitleKeywordsExplosives <- gsr(as.character(AbsTitleKeywordsCount$AbsTitleKeywordsExplosives),as.character(ExplosiveList$Uncorrected.Explosive),as.character(ExplosiveList$Corrected.Explosive))
AbsTitleKeywordsCount$AbsTitleKeywordsExplosives <- gsr(as.character(AbsTitleKeywordsCount$AbsTitleKeywordsExplosives),as.character(ExplosiveList$UncorrectedNoSpaces),as.character(ExplosiveList$Corrected.Explosive))
AbsTitleKeywordsCountReduced <- AbsTitleKeywordsCount %>%
  select(AbsTitleKeywordsExplosives,DOI) %>%
  group_by(DOI) %>%
  summarize(AbsTitleKeywordsExplosives=paste0(AbsTitleKeywordsExplosives, collapse = "; "))

#######
#Join results from full text to results from abs,title,keywords
CombinedCount<- full_join(AbsTitleKeywordsCountReduced,FullTextCountReduced, by = "DOI")
#names(FullTextCountReduced)[2] ="AbsTitleKeywordsExplosives"

# Join with Interpol data to get titles
CombinedCount <- right_join(Interpol_data,CombinedCount, by = "DOI", relationship = "many-to-many") %>%
  select(DOI, Title, AbsTitleKeywordsExplosives, FullTextExplosives)

CombinedCount <- right_join(InterpolSelectedData, CombinedCount, by ="DOI", relationship = "many-to-many") %>%
  select(DOI, Title.x, AbsTitleKeywordsExplosives, FullTextExplosives, AbsTitleKeywords) %>%
  filter(!DOI=="")%>%
  distinct(DOI,.keep_all = T)

CombinedCountTitleAbsKey <- CombinedCount

CombinedCountNoTitleAbsKey <- CombinedCount %>%
  filter(is.na(AbsTitleKeywordsExplosives))

CombinedCountHasTitleAbsKey <- CombinedCount %>%
  filter(!is.na(AbsTitleKeywordsExplosives))


CombinedCountHasTitleAbsKey <- subset(CombinedCountTitleAbsKey, !(DOI %in% CombinedCountNoTitleAbsKey$DOI))
CombinedCountTitleAbsKeyExpEng <- CombinedCountTitleAbsKey %>%
  filter(grepl("explosive", AbsTitleKeywords, ignore.case=T))

#Split Column "AbsTitleKeywordsExplosives" in row by the separator ";", remove leading white space to generate list
AbsTitleKeywordsExplosives <- CombinedCount %>% 
  mutate(AbsTitleKeywordsExplosives = strsplit(as.character(AbsTitleKeywordsExplosives), ";")) %>% 
  unnest(AbsTitleKeywordsExplosives) %>%
  mutate_if(is.character, str_trim)

# replace blank with NA and remove
AbsTitleKeywordsExplosives[AbsTitleKeywordsExplosives==""] <- NA
AbsTitleKeywordsExplosives <- AbsTitleKeywordsExplosives[complete.cases(AbsTitleKeywordsExplosives[ ,2]),]

AbsTitleKeywordsExplosives <- AbsTitleKeywordsExplosives %>%
  distinct()

AbsTitleKeywordsExplosivesCount <- aggregate(AbsTitleKeywordsExplosives$AbsTitleKeywordsExplosives, by=list(Explosive=AbsTitleKeywordsExplosives$AbsTitleKeywordsExplosives), FUN=length)

names(AbsTitleKeywordsExplosivesCount)[2] <- "Explosives from Abstract, Title, and Keywords"

#Split Column "FullTextExplosives" in row by the separator ";", remove leading white space to generate list
FullTextExplosives <- CombinedCount %>% 
  mutate(FullTextExplosives = strsplit(as.character(FullTextExplosives), ";")) %>% 
  unnest(FullTextExplosives) %>%
  mutate_if(is.character, str_trim)

# replace blank with NA and remove
FullTextExplosives[FullTextExplosives==""] <- NA
FullTextExplosives <- FullTextExplosives[complete.cases(FullTextExplosives[ ,2]),]

FullTextExplosives <- FullTextExplosives %>%
  distinct()

FullTextExplosivesCount <- aggregate(FullTextExplosives$FullTextExplosives, by=list(Explosive=FullTextExplosives$FullTextExplosives), FUN=length)

names(FullTextExplosivesCount)[2] <- "Explosives from Full Text"

ExplosivesCount <- full_join(FullTextExplosivesCount,AbsTitleKeywordsExplosivesCount)
ExplosivesCount <- full_join(ExplosivesCount, FullTextExploTopn)

FullTextExplosivesCount <- aggregate(FullTextExplosives$FullTextExplosives, by=list(Explosive=FullTextExplosives$FullTextExplosives), FUN=length)

names(FullTextExplosivesCount)[2] <- "Explosives from Full Text"

ExplosivesCount <- full_join(FullTextExplosivesCount,AbsTitleKeywordsExplosivesCount)
ExplosivesCount <- full_join(ExplosivesCount, FullTextExploTopn)

TopExplosives <- top_n(FullTextExplosivesCount,20)

ExplosivesCountSubset <-subset(ExplosivesCount,Explosive %in% TopExplosives$Explosive)

ExplosivesCountSubset$Ratio <- ExplosivesCountSubset$`Explosives from Abstract, Title, and Keywords`/ ExplosivesCountSubset$`Explosives from Full Text`

write.csv(ExplosivesCountSubset, file=paste0(cit.path.InterpolOutputs,sprintf("%s.csv","Full_Text_Top20_Explo")), row.names = F)

}else{ print("Full Text Papers unavailable")}

print("Interpol data prepared for figure generation")
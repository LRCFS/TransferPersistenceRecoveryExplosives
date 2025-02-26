###########################################################################
#
# TBC - Copyright (C) 2021
#
# Leverhulme Research Centre for Forensic Science

# Centre for Forensic Science, Department of Pure and Applied Chemistry,
# University of Strathclyde, Royal College, 204 George Street, Glasgow
# Chiron AS, Stiklestadveien 1, NO-7041 Trondheim, Norway

# Caitlyn Norman, Dorothy Xi Yue Lim , Taylor Henderson, Fabio Casali,
# Niamh Nic Daéid, Lorna Nisbet, Hervé Ménard

# Website: https://github.com/HerveMenard/CollaborationNetwork-DrugsEvidence
# Contact: lrc@dundee.ac.uk
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#

###########################################################################

# To clean the Global environment
rm(list=ls()) 

#############################################################
#####                 File requirement                  #####
#############################################################
# The files to be imported are generated from Scopus and Web Of Science databases
# The columns will need to contain:
# Year; Title; Source.title; Authors; AuthorID; DE; DES; EID; SO; DT

#############################################################
#####                 Load Libraries                    #####
#############################################################

library(plyr)
library(dplyr)
library(expss)
library(stringr)
library(tidyr)
library(ggplot2)
library(maps)
library(countrycode)
library(RColorBrewer)
library(tidyverse)
library(reshape2)
library(tikzDevice)
library(Cairo)
library(extrafont)
library(ggmap)
library(mgsub)
library(httr2)
library(pdftools)
library(tm)
library(RWeka)
library(ggtext)
library(ggsci)

#############################################################
#####                Load Functions                     #####
#############################################################

source("Functions/SearchAndReplace.R")

# function to replace accented characters with unaccented equivalents 
# adapted from https://stackoverflow.com/questions/15253954/replace-multiple-letters-with-accents-with-gsub
removeDiacritics <- function(string) {
  chartr(
    "ŠŽšžŸÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝàáâãäåçèéêëìíîïðñòóôõöøùúûüýÿ",
    "SZszYAAAAAACEEEEIIIIDNOOOOOOUUUUYaaaaaaceeeeiiiidnoooooouuuuyy", 
    string
  )
}

#############################################################
#####          Create Folders and Filenames             #####
#############################################################

# set extension and Citation
extension <- ".csv"
cit.path.INTERPOL <- "INTERPOL/"
cit.path.SCOPUS <- "Scopus/"

# where the generated figures are saved, create folder if not existing
Results.dir <- "Results/"
if (!dir.exists("Results/")){
  dir.create(file.path(Results.dir),recursive = TRUE)
}
Figure.dir <- "Figures/"
if (!dir.exists("Figures/")){
  dir.create(file.path(Figures.dir),recursive = TRUE)
}

# filename for figure export
FigureName <- "Fig1_Scopus_Keyword_"
TableName <- "Table1_Scopus_Keyword_"

#############################################################
#####                    Load Countries                 #####
#############################################################

# get city/country data
data(world.cities)

#############################################################
##### Country and Institution figures
# Number of individual country appearing on the figures
NumberCountry <- 20

#############################################################
#####     This section is used to determine which       #####
#####           keywords will be searched               #####
#############################################################

#     Select one of the following three options
 KeywordEntries <- "Author_Keywords"
# KeywordEntries <- "Database_Keywords"
# KeywordEntries <- "All_Keywords"

#############################################################
#####                  Data cleansing                   #####
#############################################################

#Correction to the keywords can be applied at this stage. This can be done in Notepad++, Excel etc. The ultimate order of the list must be kept so it can be binded to the orignial data.
#read the corrected list of keywords and combine it to the original list

#Load Keyword correction list
KeywordCorrectionList <- read.csv("CorrectionLists/KeywordsCorrectionFull.txt", sep="\t", header=TRUE)
KeywordCorrectionList <- as.data.frame(KeywordCorrectionList)
# 
# #############################################################
# #####           Load Sampling Corpus                   #####
# #############################################################
# 
# Load the Corpus of interest to search in the Interpol output entries
SamplingList <- read.csv("SamplingDatabase.csv", header = TRUE)

SamplingList$UncorrectedNoSpecials <- SamplingList$Keyword
SamplingList$UncorrectedNoSpecials <- paste0(" ", SamplingList$UncorrectedNoSpecials)
SamplingList$UncorrectedNoSpecials<- gsub('[^[:alnum:] ]', ' ', SamplingList$UncorrectedNoSpecials)
SamplingList$UncorrectedNoSpecials <- gsub("\\s+", " ", SamplingList$UncorrectedNoSpecials)

# Remove trailing (right) whitespace and make lowercase
SamplingList$UncorrectedNoSpecials <- trimws(SamplingList$UncorrectedNoSpecials, which = c("right"))
SamplingList$UncorrectedNoSpecials <- toupper(SamplingList$UncorrectedNoSpecials)

SamplingList$UncorrectedNoSpaces <- SamplingList$UncorrectedNoSpecials
SamplingList$UncorrectedNoSpaces <- trimws(SamplingList$UncorrectedNoSpaces, which = c("both"))

# Convert to Object
SamplingListString <- SamplingList$UncorrectedNoSpecials

#Get distinct Sampling list
SamplingListDistinct <- data.frame(SamplingList$Corrected.Sampling) %>%
  distinct()

#############################################################
#####           Figure Settings                         #####
#############################################################
#############################################################
##### Figure colours
# assign text colour
textcol <- "black"

#############################################################
##### Keyword Figures
# the number of keywords (top most frequent) appearing in the figure
number <- 100   # target number of keywords apearing in the keyword figure
maximum <- 105  # maximum number of keywords appearing in the keyword figure

# colour palette for the Keyword figure
pal <- c("#990000","#FF5D00","#FFB900","#FFFF00","#ACFF00","#00CC00","#33FFFF","#008BFF","#0000FF","#8968CD","#551A8B")

#############################################################
#####  This is the code for generating Keyword figures  #####
#############################################################

# for (file in filenames){
# # remove the extension and path of the file in column reference  
#   name <- gsub(extension, "", file)
name <- "_Scopus"
Count <- number

#############################################################
#####       Load and Format INTERPOL Data               #####
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

InterpolKeywordList$AIKeywords[InterpolKeywordList$AIKeywords==""] <- NA
InterpolKeywordList <- InterpolKeywordList[complete.cases(InterpolKeywordList[ ,8]),]

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

print("Interpol data prepared for figure generation")

#############################################################
#####          Load and Format Scopus Data              #####
#############################################################
#####      Generate SCOPUS combined list                #####

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

ScopusAuthKey <- Scopus_data
ScopusAuthKey$AIKeywords[ScopusAuthKey$AIKeywords==""] <- NA
ScopusAuthKey <- ScopusAuthKey %>%
  filter(!is.na(AIKeywords))

print("Scopus data prepared for figure generation")

#######################################################
#####       Interpol Sampling Keywords           #####
#######################################################
#######################################################
#######                For abstract               #####
#######################################################
# Check for presence of each word in Abstract
InterpolAbs <- vector("list", nrow(Interpol_data))

for(d in 1:nrow(Interpol_data)){
  for(w in seq_along(SamplingListString)){
    intermed   <- grep(SamplingListString[[w]], Interpol_data[[d,"Abstract"]])
    InterpolAbs[[d]] <- c(InterpolAbs[[d]], 
                          SamplingListString[[w]][ (length(intermed) > 0) ])
  }
}

# combined together
InterpolSamplingFromAbs <- sapply(InterpolAbs, paste0, collapse=";")
InterpolSamplingFromAbs <- as.data.frame(InterpolSamplingFromAbs)

InterpolSamplingFromAbs <- data.frame(Interpol_data$Title,InterpolSamplingFromAbs)

# remane fist column to match original dataset 
names(InterpolSamplingFromAbs)[1] <- c("Title")

# #########################################################
# #####                For title                      #####
##############################################
# Check for presence of each word in Title
InterpolTitles <- vector("list", nrow(Interpol_data))

for(d in 1:nrow(Interpol_data)){
  for(w in seq_along(SamplingListString)){
    intermed   <- grep(SamplingListString[[w]], Interpol_data[[d,"Title"]])
    InterpolTitles[[d]] <- c(InterpolTitles[[d]], 
                             SamplingListString[[w]][ (length(intermed) > 0) ])
  }
}

# combined together
InterpolSamplingFromTitle    <- sapply(InterpolTitles, paste0, collapse=";")
InterpolSamplingFromTitle <- as.data.frame(InterpolSamplingFromTitle)

InterpolSamplingFromTitle <- data.frame(Interpol_data$Title,InterpolSamplingFromTitle)

# remane fist column to match original dataset 
names(InterpolSamplingFromTitle)[1] <- c("Title")

InterpolSamplingFromAbsTitle <- full_join(InterpolSamplingFromAbs,InterpolSamplingFromTitle)

# #########################################################
# #####                For keywords                   #####
##############################################
# Check for presence of each word in Keywords
InterpolKeywords <- vector("list", nrow(Interpol_data))

for(d in 1:nrow(Interpol_data)){
  for(w in seq_along(SamplingListString)){
    intermed   <- grep(SamplingListString[[w]], Interpol_data[[d,"KeywordsCorrected"]])
    InterpolKeywords[[d]] <- c(InterpolKeywords[[d]], 
                               SamplingListString[[w]][ (length(intermed) > 0) ])
  }
}

# combined together
InterpolSamplingFromKeyword    <- sapply(InterpolKeywords, paste0, collapse=";")
InterpolSamplingFromKeyword <- as.data.frame(InterpolSamplingFromKeyword)

InterpolSamplingFromKeyword <- data.frame(Interpol_data$Title,InterpolSamplingFromKeyword)

# remane fist column to match original dataset 
names(InterpolSamplingFromKeyword)[1] <- c("Title")

InterpolSampling <- full_join(InterpolSamplingFromAbsTitle,InterpolSamplingFromKeyword)
#write.csv(InterpolSampling,file = "InterpolSampling.csv",row.names = TRUE)

############
# Combine Columns Abs,Titles,Keywords and place in Column name "Sampling_List" and remove original columns
InterpolSampling <- InterpolSampling %>%
  unite("Sampling_List", c(InterpolSamplingFromAbs), c(InterpolSamplingFromTitle), c(InterpolSamplingFromKeyword),sep = ";", remove = TRUE)

#Split Column "Sampling_List" in row by the separator ";", remove leading white space to generate list
InterpolSampling <- InterpolSampling %>% 
  mutate(Sampling_List = strsplit(as.character(Sampling_List), ";")) %>% 
  unnest(Sampling_List) %>%
  mutate_if(is.character, str_trim)

# Apply corrections to Interpol Sampling List
#InterpolSampling$Sampling_List_Corrected <- gsr(as.character(InterpolSampling$Sampling_List),as.character(SamplingList$UncorrectedNoSpaces),as.character(SamplingList$Corrected.Sampling))

# Remove duplicates from Sampling list
InterpolSampling <- InterpolSampling %>%
  select(Title,Sampling_List)%>%
  distinct()

#Group keywords together
InterpolSamplingCollapsed <- InterpolSampling %>% 
  group_by(Title) %>%
  summarize(Sampling_List_Corrected=paste0(Sampling_List, collapse = ";"))

############
# get the Title and respective country names to join to the InterpolSampling
Interpol_Title_Country <- Interpol_data %>%
  select(Title,Year,Country)

InterpolSampling <- full_join(InterpolSamplingCollapsed,Interpol_Title_Country, by = "Title")

print('Interpol Sampling Keywords Extracted')

#######################################################
#####       Scopus Sampling Keywords           #####
#######################################################
#######################################################
#######                For abstract               #####
#######################################################
# Check for presence of each word in Abstract
ScopusAbs <- vector("list", nrow(Scopus_data))

for(d in 1:nrow(Scopus_data)){
  for(w in seq_along(SamplingListString)){
    intermed   <- grep(SamplingListString[[w]], Scopus_data[[d,"Abstract"]])
    ScopusAbs[[d]] <- c(ScopusAbs[[d]], 
                        SamplingListString[[w]][ (length(intermed) > 0) ])
  }
}

# combined together
ScopusSamplingFromAbs <- sapply(ScopusAbs, paste0, collapse=";")
ScopusSamplingFromAbs <- as.data.frame(ScopusSamplingFromAbs)

ScopusSamplingFromAbs <- data.frame(Scopus_data$EID,ScopusSamplingFromAbs)

# remane fist column to match original dataset 
names(ScopusSamplingFromAbs)[1] <- c("EID")

# #########################################################
# #####                For title                      #####
##############################################
# Check for presence of each word in Title
ScopusTitles <- vector("list", nrow(Scopus_data))

for(d in 1:nrow(Scopus_data)){
  for(w in seq_along(SamplingListString)){
    intermed   <- grep(SamplingListString[[w]], Scopus_data[[d,"Title"]])
    ScopusTitles[[d]] <- c(ScopusTitles[[d]], 
                           SamplingListString[[w]][ (length(intermed) > 0) ])
  }
}

# combined together
ScopusSamplingFromTitle    <- sapply(ScopusTitles, paste0, collapse=";")
ScopusSamplingFromTitle <- as.data.frame(ScopusSamplingFromTitle)

ScopusSamplingFromTitle <- data.frame(Scopus_data$EID,ScopusSamplingFromTitle)

# remane fist column to match original dataset 
names(ScopusSamplingFromTitle)[1] <- c("EID")

ScopusSamplingFromAbsTitle <- full_join(ScopusSamplingFromAbs,ScopusSamplingFromTitle)

# #########################################################
# #####                For keywords                   #####
##############################################
# Check for presence of each word in Keywords
ScopusKeywords <- vector("list", nrow(Scopus_data))

for(d in 1:nrow(Scopus_data)){
  for(w in seq_along(SamplingListString)){
    intermed   <- grep(SamplingListString[[w]], Scopus_data[[d,"KeywordsCorrected"]])
    ScopusKeywords[[d]] <- c(ScopusKeywords[[d]], 
                             SamplingListString[[w]][ (length(intermed) > 0) ])
  }
}

# combined together
ScopusSamplingFromKeyword    <- sapply(ScopusKeywords, paste0, collapse=";")
ScopusSamplingFromKeyword <- as.data.frame(ScopusSamplingFromKeyword)

ScopusSamplingFromKeyword <- data.frame(Scopus_data$EID,ScopusSamplingFromKeyword)

# remane fist column to match original dataset 
names(ScopusSamplingFromKeyword)[1] <- c("EID")

ScopusSampling <- full_join(ScopusSamplingFromAbsTitle,ScopusSamplingFromKeyword)

############
# Combine Columns Abs,Titles,Keywords and place in Column name "Sampling_List" and remove original columns
ScopusSampling <- ScopusSampling %>%
  unite("Sampling_List", ScopusSamplingFromAbs, ScopusSamplingFromTitle, ScopusSamplingFromKeyword,sep = ";", remove = TRUE)

#Split Column "Sampling_List" in row by the separator ";", remove leading white space to generate list
ScopusSampling <- ScopusSampling %>% 
  mutate(Sampling_List = strsplit(as.character(Sampling_List), ";")) %>% 
  unnest(Sampling_List) %>%
  mutate_if(is.character, str_trim)

# Apply corrections to Scopus Sampling List
#ScopusSampling$Sampling_List_Corrected <- gsr(as.character(ScopusSampling$Sampling_List),as.character(SamplingList$UncorrectedNoSpaces),as.character(SamplingList$Corrected.Sampling))

# Remove duplicates from Sampling list
ScopusSampling <- ScopusSampling %>%
  select(EID,Sampling_List)%>%
  distinct()

#Group keywords together
ScopusSamplingCollapsed <- ScopusSampling %>% 
  group_by(EID) %>%
  summarize(Sampling_List_Corrected=paste0(Sampling_List, collapse = ";"))

############
# get the Title and respective country names to join to the ScopusSampling
Scopus_Title_Country <- Scopus_data %>%
  select(Title,Year,Country,EID)

ScopusSampling <- full_join(ScopusSamplingCollapsed,Scopus_Title_Country, by = "EID")

print('Scopus Sampling Keywords Extracted')

#############################################################
#####                       Codes                       #####
#############################################################

# These codes can be run subsequently or independently

# # Figure 1, Scopus Keywords as a function of year
#source("Code/Figure1_Scopus_Keywords.R")

# # Figure 2, INTERPOL Keywords as a function of year
#source("Code/Figure2_Interpol_Keywords.R")

# # Figure 3, Interpol Sampling Country
#source("Code/Figure3_Interpol_Sampling_Country.R")

# # Figure 4, Full Text Mining Comparison
#source("Code/Figure4_Full_Text_Mining.R")


##Other Code
## To determine the most used journal and download papers using the Wiley API
#source("Code/Journal_Paper_Downloads.R")

## To compare keywords between evidence types
#source("Code/Evidence Comparison.R")

####Unused Figures#####
##To illustrate countries publishing papers included in INTERPOL reviews
#source("Code/Interpol_Country_Affiliation_Figure.R")

##To illustrate countries publishing papers included in Scopus dataset
#source("Code/Scopus_Country_Affiliation_Figure.R")

##To illustrate the most-mentioned Samplings by country in Scopus dataset
#source("Code/Scopus_Sampling_Country_Figure.R")

##To illustrate the occurrence of Samplings by year in INTERPOL reviews
#source("Code/Interpol_Sampling_Year_Figure.R")

##To illustrate the occurrence of Samplings by year in Scopus dataset
#source("Code/Scopus_Sampling_Year_Figure.R")

##To generate a country network from Scopus dataset
# source("Code/Scopus_Country_Network_Figure.R")

##To illustrate the percentage of papers published with international collaboration
#source("Code/Scopus_International_Collaboration_Figure.R")

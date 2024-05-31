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
#####                 Create Folders                    #####
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

# rename some of the columns to remove special characters or encoding
names(Interpol_data)[3] <- c("AuthorID")

Interpol_data <- Interpol_data %>%
  select(Year,Title,Source.title,Authors,Affiliations,AuthorID,Author.Keywords,Index.Keywords,EID)%>%
  distinct()

#####        Generate Scopus combined list              #####

if (file.exists("Scopus/SCOPUS.csv",recursive = TRUE)){
  print("Scopus data already combined")
  Scopus_data <- read.csv(file = "Scopus/SCOPUS.csv")
}else{Scopus_data <- list.files(cit.path.SCOPUS, pattern=extension, full.names=TRUE)
Scopus_data <- rbindlist(lapply(Scopus_data,fread, encoding='UTF-8'))
write.csv(Scopus_data,file = "Scopus/SCOPUS.csv",row.names = TRUE)
print("Scopus data has been combined")
Scopus_data <- read.csv(file = "Scopus/SCOPUS.csv")
}

# rename some of the columns to remove special characters or encoding
names(Scopus_data)[3] <- c("AuthorID")

Scopus_data <- Scopus_data %>%
  select(Year,Title,Source.title,Authors,Affiliations,AuthorID,Abstract,Author.Keywords,Index.Keywords,EID)%>%
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
Scopus_data$Affiliations <- gsub(";", ",", Scopus_data$Affiliations)

# split fields by ", "
ScopusAffiliations <- sapply(Scopus_data$Affiliations, strsplit, split = ", ", USE.NAMES = FALSE)

# extract fields which match a known city making sure that diacritics aren't a problem...
CityList <- lapply(ScopusAffiliations, function(x)x[which(removeDiacritics(x) %in% world.cities$name)])

# ... or country
CountryList <- lapply(ScopusAffiliations, function(x)x[which(removeDiacritics(x) %in% world.cities$country.etc)])

# this version only returns unique instances of countries per publication
ScopusCountryListUnique <- lapply(ScopusAffiliations, function(x)unique(x[which(x %in% world.cities$country.etc)]))

#extract the list of country per paper and place in dataframe
CountryListbyPaperUnique <- as.data.table(matrix(ScopusCountryListUnique),ExplosiveListStringAsFactors=FALSE)

# bind to the original data
ScopusCountryListbyPaperUnique<- cbind(Scopus_data,CountryListbyPaperUnique)

# rename Country column
names(ScopusCountryListbyPaperUnique)[11] <- c("Country")

#convert to character - may not be necessary
ScopusCountryListbyPaperUnique$Country <- as.character(ScopusCountryListbyPaperUnique$Country)

# remove unwanted characters  
ScopusCountryListbyPaperUnique$Country <-  gsub("c\\(","",ScopusCountryListbyPaperUnique$Country)
ScopusCountryListbyPaperUnique$Country <-  gsub("\\)","",ScopusCountryListbyPaperUnique$Country)
ScopusCountryListbyPaperUnique$Country <-  gsub("\"","",ScopusCountryListbyPaperUnique$Country)
ScopusCountryListbyPaperUnique$Country <-  gsub(", ",",",ScopusCountryListbyPaperUnique$Country)
ScopusCountryListbyPaperUnique$Country <-  gsub("character\\(0",NA,ScopusCountryListbyPaperUnique$Country)

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
#####           Interpol keyword selection             #####
#############################################################

Interpol_data_selected <- Interpol_data 

if (KeywordEntries == "Author_Keywords"){
  #   Author Keywords only
  names(Interpol_data_selected) <- sub("Author.Keywords","AIKeywords", names(Interpol_data_selected))
  Keyname <- "A_Keywords"
} else{
  if (KeywordEntries == "Database_Keywords"){
    #   Index Keywords only
    names(Interpol_data_selected) <- sub("Index.Keywords","AIKeywords", names(Interpol_data_selected))
    Keyname <- "I_Keywords"
  }
  else {
    # Index and Author Keywords
    # Combine Columns Author.Keywords and Index.Keywords and place in Column name "AIKeywords" and remove original columns
    Interpol_data_selected <- Interpol_data_selected %>%
      unite("AIKeywords", Author.Keywords, Index.Keywords,sep = ";", remove = TRUE)
    Keyname <- "AI_Keywords"
  }}

#Split Column "AIKeywords" in row by the separator ";", remove leading white space to generate list
InterpolKeywordList <- Interpol_data_selected %>% 
  mutate(AIKeywords = strsplit(as.character(AIKeywords), ";")) %>% 
  unnest(AIKeywords) %>%
  mutate_if(is.character, str_trim)

# Upper case "AIKeywords" in "DatasetKeywordList" and save in dataframe
InterpolKeywordList$AIKeywords <- toupper(InterpolKeywordList$AIKeywords)

# Extract list of "AIkeywords" and remove duplicate

InterpolDistinctKeywordList <- InterpolKeywordList %>%
  select(AIKeywords) %>%
  distinct()

#############################################################
#####           Scopus keyword selection             #####
#############################################################

Scopus_data_selected <- Scopus_data 

if (KeywordEntries == "Author_Keywords"){
  #   Author Keywords only
  names(Scopus_data_selected) <- sub("Author.Keywords","AIKeywords", names(Scopus_data_selected))
  Keyname <- "A_Keywords"
} else{
  if (KeywordEntries == "Database_Keywords"){
    #   Index Keywords only
    names(Scopus_data_selected) <- sub("Index.Keywords","AIKeywords", names(Scopus_data_selected))
    Keyname <- "I_Keywords"
  }
  else {
    # Index and Author Keywords
    # Combine Columns Author.Keywords and Index.Keywords and place in Column name "AIKeywords" and remove original columns
    Scopus_data_selected <- Scopus_data_selected %>%
      unite("AIKeywords", Author.Keywords, Index.Keywords,sep = ";", remove = TRUE)
    Keyname <- "AI_Keywords"
  }}

#Split Column "AIKeywords" in row by the separator ";", remove leading white space to generate list
ScopusKeywordList <- Scopus_data_selected %>% 
  mutate(AIKeywords = strsplit(as.character(AIKeywords), ";")) %>% 
  unnest(AIKeywords) %>%
  mutate_if(is.character, str_trim)

# Upper case "AIKeywords" in "DatasetKeywordList" and save in dataframe
ScopusKeywordList$AIKeywords <- toupper(ScopusKeywordList$AIKeywords)

# Extract list of "AIkeywords" and remove duplicate

ScopusDistinctKeywordList <- ScopusKeywordList %>%
  select(AIKeywords) %>%
  distinct()

# Select column label $Year, $Title,  $Source.title, $Author.Keywords, $Index.Keyword
ScopusCountryListbyPaperUniqueReduced <- ScopusCountryListbyPaperUnique

ScopusCountryListbyPaperUniqueReduced$Abstract <- gsub('[^[:alnum:] ]', ' ', ScopusCountryListbyPaperUniqueReduced$Abstract)
ScopusCountryListbyPaperUniqueReduced$Abstract <- gsub("\\s+", " ", ScopusCountryListbyPaperUniqueReduced$Abstract)

ScopusCountryListbyPaperUniqueReduced$Abstract <- tolower(ScopusCountryListbyPaperUniqueReduced$Abstract)

ScopusCountryListbyPaperUniqueReduced$Title <- gsub('[^[:alnum:] ]', ' ', ScopusCountryListbyPaperUniqueReduced$Title)
ScopusCountryListbyPaperUniqueReduced$Title <- gsub("\\s+", " ", ScopusCountryListbyPaperUniqueReduced$Title)

ScopusCountryListbyPaperUniqueReduced$Title <- tolower(ScopusCountryListbyPaperUniqueReduced$Title)

ScopusCountryListbyPaperUniqueReduced$Author.Keywords <- gsub('[^[:alnum:] ]', ' ', ScopusCountryListbyPaperUniqueReduced$Author.Keywords)
ScopusCountryListbyPaperUniqueReduced$Author.Keywords <- gsub("\\s+", " ", ScopusCountryListbyPaperUniqueReduced$Author.Keywords)

ScopusCountryListbyPaperUniqueReduced$Author.Keywords <- tolower(ScopusCountryListbyPaperUniqueReduced$Author.Keywords)


#############################################################
#####                  Data cleansing                   #####
#############################################################

#Correction to the keywords can be applied at this stage. This can be done in Notepad++, Excel etc. The ultimate order of the list must be kept so it can be binded to the orignial data.
#read the corrected list of keywords and combine it to the original list

#Load Keyword correction list
KeywordCorrectionList <- read.csv("CorrectionLists/KeywordsCorrectionFull.txt", sep="\t", header=TRUE)
KeywordCorrectionList <- as.data.frame(KeywordCorrectionList)

# Apply corrections to Interpol data
InterpolKeywordList$KeywordsCorrected <- gsr(as.character(InterpolKeywordList$AIKeywords),as.character(KeywordCorrectionList$AIKeywords),as.character(KeywordCorrectionList$CorAIKeywordsAcronym))

# number of distinct Keywords after correction
InterpolDistinctKeywordListCorrected <- InterpolKeywordList %>%
  select(KeywordsCorrected) %>%
  distinct()

# Apply corrections to Scopus data
ScopusKeywordList$KeywordsCorrected <- gsr(as.character(ScopusKeywordList$AIKeywords),as.character(KeywordCorrectionList$AIKeywords),as.character(KeywordCorrectionList$CorAIKeywordsAcronym))

ScopusKeywordListCollapsed <- ScopusKeywordList %>% 
  group_by(Title) %>%
  summarize(KeywordsCorrected=paste0(KeywordsCorrected, collapse = ", "))

# number of distinct Keywords after correction
ScopusDistinctKeywordListCorrected <- ScopusKeywordList %>%
  select(KeywordsCorrected) %>%
  distinct()

ScopusKeywordListCollapsedSummary <- data.frame(ScopusKeywordListCollapsed$KeywordsCorrected)
ScopusCountryListbyPaperUniqueReduced <- merge(ScopusCountryListbyPaperUniqueReduced, ScopusKeywordListCollapsedSummary, by = 0, all = TRUE)
names(ScopusCountryListbyPaperUniqueReduced)[13] <- c("AI.Keywords")

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
#####                    Data loading                   #####
#############################################################

# filename for figure export
FigureName <- "Fig1_Scopus_Keyword_"
TableName <- "Table1_Scopus_Keyword_"

#############################################################
#####  This is the code for generating Keyword figures  #####
#############################################################

# for (file in filenames){
# # remove the extension and path of the file in column reference  
#   name <- gsub(extension, "", file)
name <- "_Scopus"
Count <- number

#############################################################
#####                       Codes                       #####
#############################################################

# These codes can be run subsequently or independently as each create necessary outputs for the next codes.

# # Figure 1, INTERPOL Keywords as a function of year
# source("Code/Figure1_INTERPOL_Keywords.R")
#
# # Figure 2, Scopus Keywords as a function of year
# source("Code/Figure2_Scopus_Keywords.R")
#
# # Figure 3.1, Interpol - Country affiliations
# source("Code/Figure3.1_Interpol_Country_Affiliation.R")
#
# # Figure 3.2, Scopus - Country affiliations
# source("Code/Figure3.2_Scopus_Country_Affiliation.R")
#
# # Figure 4.1 Scopus - Explosives per country and relationship
#source("Code/Figure4.1_Scopus_Explosive_Country_Relationship.R")
#
# # Figure 4.2 Scopus - Explosives per country and relationship
# source("Code/Figure4.2_Scopus_Technique_Country_Relationship.R")
#
# # Figure 4.3 Scopus - Explosives per country and relationship
# source("Code/Figure4.3_Scopus_Surface_Country_Relationship.R")
#
# # Figure 5, Scopus - Country network map
source("Code/Figure5_CountryNetwork.R")
#
# # Figure 6, Scopus - Country collaboration percentage
# source("Code/Figure6_InternationalCollaboration.R")

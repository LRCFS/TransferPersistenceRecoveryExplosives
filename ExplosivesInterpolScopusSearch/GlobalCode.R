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

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.

# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
library(zip)

# This is computer specific and needs editing to the location of Java.
# This is for computer with limited Admin rights.
# Eclipse Temurin (an OpenJDK distribution from the Adoptium project) is also a great alternative to Java
# Download and install Eclipse Temurin (JDK 8, 11, or later) from: https://adoptium.net/
# Choose JDK (not JRE) and install it.
#
# To make it permanent ( with Admin rights)
# Manually Set JAVA_HOME in Windows:
# Open Control Panel → System → Advanced system settings.
# Click Environment Variables.
# Under System Variables, click New, and add:
# Variable name: JAVA_HOME
# Variable value: C:\Program Files\Eclipse Adoptium\jdk-XX.X.X (Replace with your installed path)
# Find the Path variable under System Variables, click Edit, and add:
# C:\Program Files\Eclipse Adoptium\jdk-XX.X.X\bin
# Restart your computer to apply the changes.
# 

Sys.setenv(JAVA_HOME = "C:/Program Files/Eclipse Adoptium/jdk-23.0.2.7-hotspot") # This ensures JAVA_HOME is set for the entire R session 
# to edit .Renviron in R:
# file.edit("~/.Renviron")
# Save and restart R to apply changes automatically at each session start.

library(rJava)
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
cit.path.InterpolInputs <- "InterpolInputs/"
cit.path.InterpolOutputs <- "InterpolOutputs/"
cit.path.ScopusInputs <- "ScopusInputs/"
cit.path.ScopusOutputs <- "Scopusoutputs/"

# where the generated figures are saved, create folder if not existing
Results.dir <- "Results/"
if (!dir.exists("Results/")){
  dir.create(file.path(Results.dir),recursive = TRUE)
}
Figure.dir <- "Figures/"
if (!dir.exists("Figures/")){
  dir.create(file.path(Figures.dir),recursive = TRUE)
}

if (!dir.exists("ScopusOutputs/")){
  dir.create(file.path(cit.path.ScopusOutputs),recursive = TRUE)
}

if (!dir.exists("InterpolOutputs/")){
  dir.create(file.path(cit.path.InterpolOutputs),recursive = TRUE)
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

# Select one of the following three options
 KeywordEntries <- "Author_Keywords"
# KeywordEntries <- "Database_Keywords"
# KeywordEntries <- "All_Keywords"

#############################################################
#####                  Data cleansing                   #####
#############################################################

#Correction to the keywords can be applied at this stage. This can be done in Notepad++, Excel etc. The ultimate order of the list must be kept so it can be binded to the orignial data.
#read the corrected list of keywords and combine it to the original list

#Load Keyword correction list
KeywordCorrectionList <- read.csv("ReferenceLists/KeywordsCorrectionFull.txt", sep="\t", header=TRUE)
KeywordCorrectionList <- as.data.frame(KeywordCorrectionList)

#############################################################
#####           Load Explosive Corpus                   #####
#############################################################

# Load the Corpus of interest to search in the Interpol output entries
ExplosiveList <- read.csv("ReferenceLists/ExplosiveDatabase.csv", header = TRUE)

# Format the Corpus to remove special characters etc.
source("Code/Explosive_Corpus_Prep.R")

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
number <- 100   # target number of keywords appearing in the keyword figure
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
#####       Load and Format Interpol Data               #####
#############################################################
#This will check if the Interpol data has already been processed and saved, and if so will read the file
#This allows figures to be generated without reprocessing the data
if (file.exists("InterpolOutputs/Interpol_Processed_Data.csv",recursive = TRUE)){
  #Interpol_Processed_data contains the list of interpol papers and associated info (title, authors, country etc and corrected keywords)
  Interpol_data <- read.csv(file = "InterpolOutputs/Interpol_Processed_Data.csv")
  #IntepolKeywordList contains the list of distinct keywords found in the Interpol papers
  InterpolKeywordList <- read.csv(file = "InterpolOutputs/Interpol_Keyword_List.csv")
  #InterpolExplosives contains the terms from the Explosive Corpus found in the Interpol title, abstract and keywords
  InterpolExplosives <- read.csv(file = "InterpolOutputs/Interpol_Explosives.csv")
  if(file.exists("InterpolOutputs/Full_Text_Top20_Explo.csv")){
    #Full_Text_Top20_Explo contains the 20 most mentioned terms from the Explosive Corpus in the Interpol full text papers
    ExplosivesCountSubset <- read.csv(file = "InterpolOutputs/Full_Text_Top20_Explo.csv")
  }else{print("Full text data unavailable")} 
  print("Interpol data already processed")
}else{ source("Code/Interpol_Data_Prep.R")

}

#############################################################
#####       Load and Format Scopus Data               #####
#############################################################
#This will check if the Scopus processed data is present and has already been extracted from the zip file
if (file.exists("ScopusOutputs/Scopus_Processed_Data.csv",recursive = TRUE)){
  #Scopus_Processed_data contains the list of scopus papers and associated info (title, authors, country etc and corrected keywords)
  Scopus_data <- read.csv(file = "ScopusOutputs/Scopus_Processed_Data.csv")
  #ScopusKeywordList contains the list of distinct keywords found in the Scopus papers
  ScopusKeywordList <- read.csv(file = "ScopusOutputs/Scopus_Keyword_List.csv")
  #ScopusExplosives contains the terms from the Explosive Corpus found in the Scopus title, abstract and keywords
  ScopusExplosives <- read.csv(file = "ScopusOutputs/Scopus_Explosives.csv")
  #ScopusCountryListUnique contains the list of distinct countries found in the scopus papers
  ScopusCountryListUnique <- read.csv(file = "ScopusOutputs/ScopusCountryListUnique.csv")
  print("Scopus data already processed")
  
#This will check if the zip file is present, and if so extract the Scopus_Processed_Data.csv file
  }else if (file.exists("ScopusCompressed/Scopus_processed_data.zip",recursive = TRUE)){
  unzip("ScopusCompressed/Scopus_processed_data.zip", exdir = "ScopusOutputs")
  unzip("ScopusCompressed/Scopus_Keyword_List.zip", exdir = "ScopusOutputs")
  unzip("ScopusCompressed/Scopus_Explosives.zip", exdir = "ScopusOutputs")
  unzip("ScopusCompressed/ScopusCountryListUnique.zip", exdir = "ScopusOutputs")
  #Scopus_Processed_data contains the list of scopus papers and associated info (title, authors, country etc and corrected keywords)
  Scopus_data <- read.csv(file = "ScopusOutputs/Scopus_processed_data.csv")
  #ScopusKeywordList contains the list of distinct keywords found in the Scopus papers
  ScopusKeywordList <- read.csv(file = "ScopusOutputs/Scopus_Keyword_List.csv")
  #ScopusExplosives contains the terms from the Explosive Corpus found in the Scopus title, abstract and keywords
  ScopusExplosives <- read.csv(file = "ScopusOutputs/Scopus_Explosives.csv")
  #ScopusCountryListUnique contains the list of distinct countries found in the scopus papers
  ScopusCountryListUnique <- read.csv(file = "ScopusOutputs/ScopusCountryListUnique.csv")
  print("Scopus processed data extracted")

#If the Scopus processed data is not present,  this will run the code to process the data
}else{source("Code/Scopus_Data_Prep.R")
  
}

#############################################################
#####                       Codes                       #####
#############################################################

# These codes can be run subsequently or independently

# # Figure 1, Scopus Keywords as a function of year
source("Code/Figure1_Scopus_Keywords.R")

# # Figure 2, Interpol Keywords as a function of year
source("Code/Figure2_Interpol_Keywords.R")

# # Figure 3, Interpol Explosive Country
source("Code/Figure3_Interpol_Explosive_Country.R")

# # Figure 4, Full Text Mining Comparison
source("Code/Figure4_Full_Text_Mining.R")


##Other Code
## To determine the most used journal and download papers using the Wiley API
#source("Code/Journal_Paper_Downloads.R")

## To compare keywords between evidence types
#source("Code/Evidence_Comparison.R")

## For data analysis in paper
#source("Code/Data_Analysis.R")

####Unused Figures#####
##To illustrate countries publishing papers included in Interpol reviews
#source("Code/Interpol_Country_Affiliation_Figure.R")

##To illustrate countries publishing papers included in Scopus dataset
#source("Code/Scopus_Country_Affiliation_Figure.R")

##To illustrate the most-mentioned explosives by country in Scopus dataset
#source("Code/Scopus_Explosive_Country_Figure.R")

##To illustrate the occurrence of explosives by year in Interpol reviews
#source("Code/Interpol_Explosive_Year_Figure.R")

##To illustrate the occurrence of explosives by year in Scopus dataset
#source("Code/Scopus_Explosive_Year_Figure.R")
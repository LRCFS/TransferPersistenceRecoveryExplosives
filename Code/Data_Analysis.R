#############################################################
#####                Data Analysis                      #####
#############################################################

#To determine how many Interpol records have an EID
InterpolEID <- Interpol_data %>%
  filter(!(EID==""))

#To determine how many Interpol records are in Scopus search
InterpolinScopus <- inner_join(Interpol_data,Scopus_data, by="EID") %>%
  select(EID)

#To determine how many keywords shared between Interpol and Scopus
InterpolScopusKeywords <- inner_join(InterpolTopKeywordsGraphReduced, ScopusTopKeywordsGraphReduced, by="Group.1")

#To determine how many keywords contain "forensic"
InterpolForensicKeyword <- InterpolKeywordList %>%
  select(KeywordsCorrected) %>%
  distinct() %>%
  filter(grepl("forensic", KeywordsCorrected, ignore.case = T))

#To determine how many papers contain "forensic science" as a keyword
InterpolForensicScienceKeyword <- InterpolKeywordList %>%
  select(KeywordsCorrected) %>%
  filter(grepl("forensic science", KeywordsCorrected, ignore.case = T))

#To determine how many distinct explosives are present in the Interpol selected papers
InterpolExplosivesDistinct <- InterpolExplosives %>%
  select(Explosive_List_Corrected) %>%
  mutate(Explosive_List_Corrected = strsplit(as.character(Explosive_List_Corrected), ";")) %>% 
  unnest(Explosive_List_Corrected) %>%
  distinct()

#To determine the total, mean, and median number of times an explosive is mentioned in full text
TotalMentions = sum(FullTextExplo$Occurence)
FullTextMeanMentions <- mean(FullTextExplo$Occurence)
FullTextMedianMentions <- median(FullTextExplo$Occurence)

#To determine how many times an explosive is only mentioned once in a paper
ExplosivesSingleOccurence <- FullTextExplo %>%
  filter(Occurence=="1")

#Step 0 Remove any previous results
rm(FullTextCount, AbsTitleKeywordsCount, FullTextExploTop, FullTextExploTopn)

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
FullTextExplo$Occurence <- factor(FullTextExplo$Occurence)
FullTextExplo$Frequency <- FullTextExplo$Occurence

FullTextExploHist <- aggregate(FullTextExplo$Frequency, by=list(Occurence=FullTextExplo$Occurence), FUN=length)

######Histogram########
ExplosivesHistogram <- ggplot(FullTextExploHist, aes(x=Occurence, y=x))+
  geom_bar(stat="identity")

show(ExplosivesHistogram)

FullTextExplo$Occurence <- as.numeric(FullTextExplo$Occurence)
TotalMentions = sum(FullTextExplo$Occurence)
FullTextMeanMentions <- mean(FullTextExplo$Occurence)
FullTextMedianMentions <- median(FullTextExplo$Occurence)

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
n = 1
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
InterpolSelectedData <- read.csv(file = "INTERPOL/INTERPOL_Combined.csv")
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
CombinedCount <- right_join(Interpol_data,CombinedCount, by = "DOI") %>%
  select(DOI, Title, AbsTitleKeywordsExplosives, FullTextExplosives)

CombinedCount <- right_join(InterpolSelectedData, CombinedCount, by ="DOI") %>%
  select(DOI, Title.x, AbsTitleKeywordsExplosives, FullTextExplosives, AbsTitleKeywords)

CombinedCountTitleAbsKey <- CombinedCount

CombinedCountNoTitleAbsKey <- CombinedCount %>%
  filter(is.na(AbsTitleKeywordsExplosives))
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

TopExplosives <- top_n(FullTextExplosivesCount,20)

TopExplosivesOrder <- TopExplosives
TopExplosivesOrder <- TopExplosivesOrder %>%
  arrange(desc(`Explosives from Full Text`),(Explosive))

ExplosivesCountSubset <-subset(ExplosivesCount,Explosive %in% TopExplosives$Explosive)

ExplosivesCountSubset$Ratio <- ExplosivesCountSubset$`Explosives from Abstract, Title, and Keywords`/ ExplosivesCountSubset$`Explosives from Full Text`

ExplosivesCountSubsetGraph <- ExplosivesCountSubset %>%
  pivot_longer(cols=c("Explosives from Full Text", "Explosives from Abstract, Title, and Keywords"),
               names_to="Source", values_to="Count")
ExplosivesCountSubsetGraph$Source <- factor(ExplosivesCountSubsetGraph$Source, levels = c("Explosives from Full Text", "Explosives from Abstract, Title, and Keywords"))
ExplosivesCountSubsetGraph <- subset(ExplosivesCountSubsetGraph, Source %in% c("Explosives from Full Text","Explosives from Abstract, Title, and Keywords"))

  ######## Graph ########
ExplosivesMentionsbySource <- ggplot(ExplosivesCountSubsetGraph)+
  geom_point(aes(x=factor(Explosive, levels = TopExplosivesOrder$Explosive), y=Count, color=Source, shape=Source), size=1) + 
  scale_shape_manual(values = c(15, 17)) +
  scale_color_manual(values = c("#969696","#636363"))+
  geom_point(aes(x=factor(Explosive, levels = TopExplosivesOrder$Explosive), y=((Ratio+1)*100)), color = "#525252", size=1, shape = 1) + 
  scale_y_continuous(sec.axis = sec_axis(transform = ~./100-1 , name = "Ratio of Counts"),limits=c(0,200))+
  labs(y="Number of Papers")+
  theme(legend.position="none",
        axis.text.y=element_text(size=6, vjust=0.2),
        axis.title.y=element_text(size=6),
        axis.ticks=element_line(size=0.4),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.margin=margin(0.2,0.2,0.2,0.2,"cm"),
        plot.title=element_text(colour=textcol,hjust=0,size=6),
        axis.text.x = element_text(size=6, angle = 90, hjust=1),
        axis.title.x = element_blank())

show(ExplosivesMentionsbySource)

#save figure
Var1 <- paste0("Fig_4_ExplosivesMentionsbySource")

ggsave(paste0(Figure.dir,sprintf("%s.tiff",Var1)), ExplosivesMentionsbySource, width = 9, height = 12, units = "cm", dpi=600)


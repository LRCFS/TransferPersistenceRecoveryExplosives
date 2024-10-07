
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

######## Get Keywords from abstract, title and author keywords ##########
# Step 2: Get abstract, title and author keywords for searching
InterpolSelectedData <- read.csv(file = "INTERPOL/INTERPOL_Combined.csv")
InterpolSelectedData <- InterpolSelectedData %>%
  select(Title,Abstract,Author.Keywords,DOI,Source.title)%>%
  distinct()

InterpolSelectedData <- InterpolSelectedData[InterpolSelectedData$Source.title %in% "Propellants, Explosives, Pyrotechnics", ]

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
names(FullTextCountReduced)[2] ="AbsTitleKeywordsExplosives"

# Join with Interpol data to get titles
CombinedCount <- right_join(Interpol_data,CombinedCount, by = "DOI") %>%
  select(DOI, Title, AbsTitleKeywordsExplosives, FullTextExplosives)

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

names(AbsTitleKeywordsExplosivesCount)[2] <- "Count"

AbsTitleKeywordsExplosivesCount$Source <- "Explosives from Abstract, Title, and Keywords"

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

names(FullTextExplosivesCount)[2] <- "Count"

FullTextExplosivesCount$Source <- "Explosives from Full Text"

ExplosivesCount <- bind_rows(FullTextExplosivesCount,AbsTitleKeywordsExplosivesCount)

TopExplosives <- top_n(FullTextExplosivesCount,20,wt=Count)

ExplosivesCountSubset <-subset(ExplosivesCount,Explosive %in% TopExplosives$Explosive)

######## Graph ########
ExplosivesMentionsbySource <- ggplot(ExplosivesCountSubset)+
  geom_col(aes(x=Explosive, y=Count, fill=Source), position=position_dodge2(preserve="single")) + 
  scale_fill_manual(values = c(brewer.pal(3, "Set1")))+
  labs(y="Number of Papers")+
  theme(legend.position="right",legend.direction="vertical",
        legend.title=element_text(colour=textcol),
        legend.margin=margin(grid::unit(0,"cm")),
        legend.text=element_text(colour=textcol,size=8),
        legend.key.height=grid::unit(0.8,"cm"),
        legend.key.width=grid::unit(0.2,"cm"),
        axis.text.y=element_text(size=5, vjust=0.2),
        axis.ticks=element_line(size=0.4),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.margin=margin(0.7,0.6,0.1,0.8,"cm"),
        plot.title=element_text(colour=textcol,hjust=0,size=6),
        axis.text.x = element_text(size=6, angle = 60, hjust=1))

show(ExplosivesMentionsbySource)

#save figure
Var1 <- paste0("Fig_6_ExplosivesMentionsbySource")

ggsave(paste0(Figure.dir,sprintf("%s.tiff",Var1)), ExplosivesMentionsbySource, width = 7.5, height = 8.5, units = "in", dpi=300)

print("Processing complete. Please check 'Figures/' folder for output")

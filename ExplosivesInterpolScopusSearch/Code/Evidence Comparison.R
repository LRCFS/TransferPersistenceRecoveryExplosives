InterpolDataEIDs <- Interpol_data
InterpolDataEIDs[InterpolDataEIDs==""] <- NA
InterpolDataEIDs <- InterpolDataEIDs[complete.cases(InterpolDataEIDs[ ,10]),]

InterpolScopus <- subset(InterpolDataEIDs, EID %in% Scopus_data$EID)

DrugKeywords <- read.csv(file = "DrugsEvidence/Fig_1_INTERPOL_Author_Keywords.csv")
DrugKeywords <- aggregate(DrugKeywords$x, by=list(Keyword=DrugKeywords$Keyword), FUN=sum)
DrugKeywords <- DrugKeywords[order(DrugKeywords$x,decreasing = T),]
DrugKeywords <- top_n(DrugKeywords,100)
DrugKeywords$KeywordsCorrected <- DrugKeywords$Keyword
DrugKeywords$KeywordsCorrected <- gsr(as.character(DrugKeywords$Keywords),as.character(KeywordCorrectionList$AIKeywords),as.character(KeywordCorrectionList$CorAIKeywordsAcronym))
InterpolDrugKeyword <- subset(InterpolKeywordNarrowRangeGraph, Keyword %in% DrugKeywords$KeywordsCorrected)

Drug_data <- read.csv(file = "DrugsEvidence/IFSMS_DG.csv")
Drug_data[Drug_data==""] <- NA
Drug_data <- Drug_data[complete.cases(Drug_data[ ,13]),]

InterpolDrug <- subset(Interpol_data, DOI %in% Drug_data$DOI)

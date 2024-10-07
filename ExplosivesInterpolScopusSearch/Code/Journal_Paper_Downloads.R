InterpolJournalCount <- aggregate(Interpol_data$Source.title, by= list(Interpol_data$Source.title), FUN=length)
names(InterpolJournalCount)[2] <- c("Count")
names(InterpolJournalCount)[1] <- c("Journal")
InterpolJournalCountOrder <- InterpolJournalCount[order(InterpolJournalCount$Count, decreasing = TRUE),  ]
TopPaper <- top_n(InterpolJournalCountOrder,1)

PropPapers <- Interpol_data
PropPapers <- PropPapers[Interpol_data$Source.title %in% TopPaper$Journal, ]

for(d in 297:nrow(PropPapers)){
  doi <- PropPapers[1, "DOI"]
  doi <- gsub('/', '%2F', doi)
  link <- paste("https://api.wiley.com/onlinelibrary/tdm/v1/articles/",doi, sep ="")
  FileName <- paste("Papers/",doi,".pdf",sep="")
  req <- request(link) %>%
    req_headers(`Wiley-TDM-Client-Token` = 'b082a446-7d79-4b13-93d1-6fc06b5f51eb') %>%
    req_throttle(rate = 20 / 60)
  resp <- req_perform(req)
  resp <- resp_body_raw(resp)
  writeBin(resp,FileName)
}


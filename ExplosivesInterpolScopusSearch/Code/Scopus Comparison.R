InterpolScopus <- subset(InterpolTopKeywordsGraphReduced, Group.1 %in% ScopusTopKeywordsGraphReduced$Group.1)

Test1 <- as.data.frame(InterpolTopKeywordsGraphReduced$Group.1)
Test2 <- as.data.frame(ScopusTopKeywordsGraphReduced$Group.1)
names(Test1)[1] <- c("Keyword")
names(Test2)[1] <- c("Keyword")
InterpolScopus2 <- inner_join(Test2,Test1)
InterpolScopus3 <- anti_join(Test1,Test2)
InterpolScopus4 <- anti_join(Test2,Test1)

InterpolScopusCounty <- subset(InterpolCountryList, Country %in% ScopusCountryListUniqueFig3.2$Country)                              

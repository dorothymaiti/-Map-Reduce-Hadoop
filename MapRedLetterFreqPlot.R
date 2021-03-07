setwd("~/Assignment")

library(readxl)
library(ggplot2)
library(dplyr)
library(reshape2)

Hadoop1 <- read_excel("Hadoop1.xlsx")
Hadoop2 <- read_excel("Hadoop2.xlsx")
Hadoop3 <- read_excel("Hadoop3.xlsx")
Hadoop4 <- read_excel("Hadoop4.xlsx")
Hadoop5 <- read_excel("Hadoop5.xlsx")
Hadoop6 <- read_excel("Hadoop6.xlsx")

Italian<- merge(Hadoop1, Hadoop4, by.x = "Alphabet", 
                by.y = "Alphabet", all.x = TRUE)
Italian$totalCount <- as.numeric((Italian$Count.x + Italian$Count.y))
Italian$freq <- as.numeric(Italian$totalCount/sum(Italian$totalCount))


ggplot(Italian, aes(x= Alphabet,y= freq)) + geom_bar(aes(fill= freq),stat = "identity") +   labs(x = 'Alphabet', y = 'Frequency',
                                                                                                   title = 'Analysis of letter frequency in Italian text')

Finnish <- merge(Hadoop2, Hadoop5, by.x = "Alphabet", 
                by.y = "Alphabet", all.x = TRUE)
Finnish$totalCount <- as.numeric((Finnish$Count.x + Finnish$Count.y))
Finnish$freq <- as.numeric(Finnish$totalCount/sum(Finnish$totalCount))

ggplot(Finnish, aes(x= Alphabet,y= freq)) + geom_bar(aes(fill= freq),stat = "identity") + labs(x = 'Alphabet', y = 'Frequency',
                                                                                                 title = 'Analysis of letter frequency in Finnish text')

Romanian <- merge(Hadoop3, Hadoop6, by.x = "Alphabet", 
                 by.y = "Alphabet", all.x = TRUE)
Romanian$totalCount <- as.numeric((Romanian$Count.x + Romanian$Count.y))
Romanian$freq <- as.numeric(Romanian$totalCount/sum(Romanian$totalCount))

ggplot(Romanian, aes(x= Alphabet,y= freq)) + geom_bar(aes(fill= freq),stat = "identity") + labs(x = 'Alphabet', y = 'Frequency',
                                                                                                 title = 'Analysis of letter frequency in Romanian text')

df<- merge(Italian,Romanian, by.x = "Alphabet", by.y = "Alphabet", all.x = TRUE)
df<- merge(df, Finnish,by.x = "Alphabet", by.y = "Alphabet", all.x = TRUE)
df1<- df%>% select(Alphabet, freq.x, freq.y,freq)
newDf <- melt(df1, id.vars = c('Alphabet'))
ggplot(newDf, aes(x=Alphabet, y=value)) +  geom_bar(aes(fill=variable),stat="identity",colour="black", position=position_dodge()) +
  scale_fill_discrete(name = "languagess", labels = c("Italian", "Romanian","Finnish"))+ labs(x = 'Alphabet', y = 'letterFreq',
                                                                                                   title = 'Letter freq for different languages')


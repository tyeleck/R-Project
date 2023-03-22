#library
library(tidyverse)
library(ggplot2)
library(ggwordcloud)
unique(train$class) #class = " ", figurative, irony, regular, and sarcasm


#Filter tweets with their classes
figurativeSet <- filter(train, class=="figurative")
summary(figurativeSet)
class(figurativeSet$tweets)

ironySet <- filter(train, class == "irony")

sarcasmSet <- filter(train, class =="sarcasm")

regularSet <- filter(train, class =="regular")
#Collect the words and their frequencies from the set

freq_figurative <- as.data.frame(sort(table(unlist(strsplit(figurativeSet$tweets," "))), decreasing = TRUE), stringsAsFactors = FALSE)
summary(freq_figurative)

freq_irony <- as.data.frame(sort(table(unlist(strsplit(ironySet$tweets," "))), decreasing = TRUE), stringsAsFactors = FALSE)

freq_sarcasm <- as.data.frame(sort(table(unlist(strsplit(sarcasmSet$tweets," "))), decreasing = TRUE), stringsAsFactors = FALSE)

freq_regular <- as.data.frame(sort(table(unlist(strsplit(regularSet$tweets," "))), decreasing = TRUE), stringsAsFactors = FALSE)


#Key objects: frequency sets

#make the word cloud of each list
  #Figurative word cloud
#Since there's over 34,000 tibbles it wouldn't make a wordcloud due to the length in processing
#So take a subset of the data where we select only the words with 100+ frequencies
dfSubsetFigurative <- subset(freq_figurative, Freq > 100, stringsAsFactors = FALSE)

dfSubsetIrony <- subset(freq_irony, Freq > 100, stringsAsFactors = FALSE)

dfSubsetSarcasm <- subset(freq_sarcasm, Freq > 100, stringsAsFactors = FALSE)

dfSubsetRegular <- subset(freq_regular, Freq > 100, stringsAsFactors = FALSE)

#wordclouds

#figurative
set.seed(42)
ggplot(dfSubsetFigurative, aes(label = Var1, size = length(Var1), color = Var1)) +
  geom_text_wordcloud_area(eccentricity = .35) +
  scale_size_area(max_size = 24) +
  theme_minimal() 

#irony 
set.seed(42)
ggplot(dfSubsetIrony, aes(label = Var1, size = length(Var1), color = Var1)) +
  geom_text_wordcloud_area(eccentricity = .35) +
  scale_size_area(max_size = 24) +
  theme_minimal() 

#sarcasm
set.seed(42)
ggplot(dfSubsetSarcasm, aes(label = Var1, size = length(Var1), color = Var1)) +
  geom_text_wordcloud_area(eccentricity = .35) +
  scale_size_area(max_size = 24) +
  theme_minimal() 

#regular
set.seed(42)
ggplot(dfSubsetRegular, aes(label = Var1, size = length(Var1), color = Var1)) +
  geom_text_wordcloud_area(eccentricity = .35) +
  scale_size_area(max_size = 24) +
  theme_minimal() 

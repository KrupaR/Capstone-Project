# Date : 30-Aug-2016

# Define a function which will check if a package is installed
	is_installed <- function(pkg) is.element(pkg, installed.packages()[,1])

# install tm, SnowballC, ggplot2 and wordcloud packages if they are not inctalled yet
	if(!is_installed("tm"))install.packages("tm")
	if(!is_installed("SnowballC"))install.packages("SnowballC")
	if(!is_installed("ggplot2"))install.packages("ggplot2")
	if(!is_installed("wordcloud"))install.packages("wordcloud")

# include tm library
	library(tm)

#Create Corpus from the three text files - news, twitter and blog
	docs <- Corpus(DirSource("D:/Data specialist course/Capstone Project/final/en_US_new"))

# define function toSpace which will replace a special character and convert it into space
	toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})
	docs <- tm_map(docs, toSpace, "-")
	docs <- tm_map(docs, toSpace, "\"")
	docs<- tm_map(docs, toSpace, ":")
	docs <- tm_map(docs, toSpace, "'")
	docs <- tm_map(docs, toSpace, "`")
	docs <- tm_map(docs, toSpace, "-")
	docs <- tm_map(docs, toSpace, "â")
	docs <- tm_map(docs, toSpace, "€")
	docs <- tm_map(docs, toSpace, "œ")

#Remove punctuation - replace punctuation marks with " "
	docs <- tm_map(docs, removePunctuation)

#Transform to lower case (need to wrap in content_transformer)
	docs <- tm_map(docs,content_transformer(tolower))
#Strip digits (std transformation, so no need for content_transformer)
	docs <- tm_map(docs, removeNumbers)
#remove stopwords using the standard list in tm
	docs <- tm_map(docs, removeWords, stopwords("english"))
#Strip whitespace (cosmetic?)
	docs <- tm_map(docs, stripWhitespace)

# Create Document Term Matrix and 
	dtm <- DocumentTermMatrix(docs)
# Calculate frequency of accurance of each word across all the documents
	Frequency <- colSums(as.matrix(dtm))
# Create a data frame with tokens and their frequency for creating plot
	TokenFreq <- data.frame(token=colnames(dtm),freq = Frequency)
#create sort order
	ordr <- TokenFreq[rev(order(TokenFreq$freq)),]
# Max frequency of any token
	MaxFreq <- max(Frequency) 
# subsetting based on 80% criteria
	MostFrequent <- subset(TokenFreq, freq >= MaxFreq*0.2) 

# Count number of tokens
	CountTokens <- length(Frequency)
	TotalTerms <- sum(Frequency)
# Number of tokens with count == 1
	count1 <- sum(Frequency == 1) 


UniqueWordsCount <- function(ordr,percentage)
{
	FreqSum <- 0
	for ( i in 1:nrow(ordr))
	{
		FreqSum <- FreqSum + ordr[i,2]
      	if (FreqSum >= TotalTerms*percentage)return (i)
      }
}


#Displaying metrics
	cat(" \n Number of terms =",CountTokens)
	cat(" \n Max Frequency of the term = ",MaxFreq)
	cat(" \n Number of words with single occurance = ",count1)
	cat(" \n Percentage of words with single occurance = ",count1/CountTokens*100)
	cat(" \n Number of unique words in a the frequency sorted dictionary to cover 50% of all word instances = ",UniqueWordsCount(ordr,0.5))
	cat(" \n Number of unique words in a the frequency sorted dictionary to cover 50% of all word instances = ",UniqueWordsCount(ordr,0.9))


#inspect most frequently occurring terms
	cat(" The top 10 terms with their frequency are:\n")
	for (i in 1:10) cat(MostFrequent[i,1],"\t",MostFrequent[i,2],"\n")

# Set Output folder 
	setwd("D:/Data specialist course/Capstone Project/final/Output")

# Plot bar chart for all words with frequency more than 10
# Save the plot in a jpeg file
	library(ggplot2)
	
	p1 <- ggplot(MostFrequent, aes(token,freq))
	p1 <- p1 + geom_bar(stat="identity")
	p1 <- p1 + theme(axis.text.x=element_text(angle=45, hjust=1))
	p1
	#dev.copy(jpeg,filename="BarPlot.jpg")
	#dev.off ()

# Create wordcloud

	library(wordcloud)
	set.seed(25)

	#limit words by specifying min frequency and add colour
	wordcloud(MostFrequent$token,MostFrequent$freq,colors=brewer.pal(6,"Dark2"))
	#dev.copy(jpeg,filename="WordCloud.jpg")
	#dev.off ()

# Delete objects and release memory
	rm(list = ls(all = TRUE))
	gc()

x = getwd()
setwd(x)
library(rpart)
library(rpart.plot)

#Code to read data and count number of cases of vandalism detected

vdata = read.csv(file = "hw3.csv", header = T, check.names = T, na.strings = "", strip.white = T)
colnames(vdata)
vcount <- subset(vdata, vdata$Vandal == 1)
nrow(vcount) #This tells us there were 1815 counts of vandalism detected

#Preprocessing of text data and creating a corpus from the 'Added' column 
library(tm)
library(NLP)
library(SnowballC)

added = vdata[,c(6)]
added  = as.data.frame(added)
addedNONA = as.data.frame(added[complete.cases(added),])
myCorpus<- Corpus(DataframeSource(addedNONA))
getTransformations()

myCorpus = tm_map(myCorpus, tolower)
myCorpus = tm_map(myCorpus, removeNumbers)
myCorpus = tm_map(myCorpus, removePunctuation)
myCorpus = tm_map(myCorpus, removeWords, stopwords("english"))
myCorpus = tm_map(myCorpus, stemDocument)
myCorpus = tm_map(myCorpus, stripWhitespace)
myCorpus = tm_map(myCorpus, PlainTextDocument)

test = myCorpus
length(test) #This tells us that 2395 documents were finally added to the corpus after preprocessing

#Creating a Document Term Matrix and filtering out sparse terms 

tdm <- DocumentTermMatrix(test)
inspect(tdm) #This tells us there are 2395 documents and 6336 terms in the document term matrix
tm <- as.matrix(tdm) 
length(tm)

notSparse = removeSparseTerms(tdm, 0.99) #Here I realized that chosing a value less than 0.99 always leaves me with no terms to inspect
inspect(notSparse) #This tells us there are 15 terms in 2395 documents which are not sparse
sparseAdded <- as.data.frame(as.matrix(notSparse))
View(sparseAdded)
wordsAdded <- as.data.frame(as.matrix(sparseAdded))

#### Repeating the steps again ####

removecorpus <- Corpus(DataframeSource(addedNONA))
removecorpus <- tm_map(removecorpus, removeWords, stopwords("english"))
removecorpus <- tm_map(removecorpus, stemDocument)
removedoc <- DocumentTermMatrix(removecorpus)
sparseRemoved <- removeSparseTerms(removedoc, 0.99)
wordsRemoved <- as.data.frame(as.matrix(sparseRemoved))

View(wordsAdded)
View(wordsRemoved)

#Creating wikiWords
wikiWords <- cbind(wordsAdded, wordsRemoved)

#Adding the vandal column
wikiWords2 <- cbind(wordsAdded, wordsRemoved, hw3$Vandal)
wikiWords$Vandal <- vdata$Vandal

library(caTools)

#Splitting the data into testing and training sets
set.seed(123)
split <- sample.split(vdata$Vandal, SplitRatio = 0.7)
train <- subset(wikiWords, split == TRUE)
test <- subset(wikiWords, split == FALSE)
table(test$Vandal)

#Building the CART Model

#CART <- rpart(Vandal~.,data = train,method = "class", parms = list(split="gini")






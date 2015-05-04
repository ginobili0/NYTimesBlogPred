NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

library(tm)

## Create corpus
CorpusHeadline = Corpus(VectorSource(c(NewsTrain$Headline, NewsTest$Headline)))

## Pre-processing
CorpusHeadline = tm_map(CorpusHeadline, tolower)
CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)
CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)
CorpusHeadline = tm_map(CorpusHeadline, removeWords, stopwords("english"))
CorpusHeadline = tm_map(CorpusHeadline, stemDocument)

## Build DTM, remove sparse terms and convert to data frame
dtm = DocumentTermMatrix(CorpusHeadline)
sparse = removeSparseTerms(dtm, 0.99)
HeadlineWords = as.data.frame(as.matrix(sparse))
colnames(HeadlineWords) = make.names(colnames(HeadlineWords))

## Splitting back to Test and Train sets
HeadlineWordsTrain = head(HeadlineWords, nrow(NewsTrain))
HeadlineWordsTest = tail(HeadlineWords, nrow(NewsTest))
HeadlineWordsTrain$Popular = NewsTrain$Popular
HeadlineWordsTrain$WordCount = NewsTrain$WordCount
HeadlineWordsTest$WordCount = NewsTest$WordCount

## Create Log Regression model using all the variables
HeadlineWordsLog = glm(Popular ~ ., data=HeadlineWordsTrain, family=binomial)

## Check the significant factors of the model and use those variables to rebuild model
HeadlineWordsLog = glm(Popular ~ WordCount + today + read + rais + news + new + morn + million + first + fashion + deal + day + china + can + busi + billion + art + X2014 + word + senat + report + polit + get + bank, data=HeadlineWordsTrain, family = binomial)

## Make predictions
PredTest = predict(HeadlineWordsLog, newdata=HeadlineWordsTest, type="response")

## Prepare for submission to Kaggle
MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTest)
write.csv(MySubmission, "SubmissionHeadlineLog.csv", row.names=FALSE)

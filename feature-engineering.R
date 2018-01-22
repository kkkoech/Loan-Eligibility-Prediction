train <- read.csv("data/train.csv", header=T, na.strings =c("","NA")) #the na.strings(..) converts blanks to NA's
test <- read.csv("data/test.csv", header=T, na.strings = c("","NA"))

summary(train_set)

#Missing observations?
sapply(train, function(x){sum(is.na(x))})
sapply(test, function(x){sum(is.na(x))})

#Missing observations 
test[is.na(train$Gender),]
#Imputing Missing observations using Mice() package
library(mice)

#Pattern for missing data
md.pattern(train)
#Visual pattern for missing data 
library(VIM)
aggr(train, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), 
     cex.axis=.5, gap=.1, ylab=c("Histogram of missing data","Pattern"))
#Imputing missing data
tempData <- mice(train,m=5,maxit=5,meth='sample') #My seed = is Default
summary(tempData)

#How do the imputed observations look?
tempData$imp$Gender
tempData$imp$Married
tempData$imp$Dependents
tempData$imp$Self_Employed

#Importing Imputed data
completeData <- complete(tempData,4) #I'm using the 4th dataset. Seems like an average of the 5.

#How does does distribution of imputed data differ from original observations?
xyplot(tempData,Gender ~ Married+Dependents+Self_Employed+LoanAmount,pch=18,cex=1)

#Or
densityplot(tempData)
stripplot(tempData, pch = 20, cex = 1.0)

#Pooling. Fitting a model from each of the imputed dataset and pooling results together
#mice_model <- with(completeData,lm(Loan_Status~Gender+Married+Dependents+Education
#                               +Self_Employed+ApplicantIncome+CoapplicantIncome+LoanAmount+Loan_Amount_Term+
#                                 Credit_History+Property_Area+Loan_Status))
#summary(pool(mice_model))



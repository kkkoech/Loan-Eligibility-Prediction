#Since there are are not many variables, we might as well describe them. 
#Variable            Description                     Type of Variable/VariableCategory 

#Loan_ID             Unique Loan ID                    #####       
#Gender              Male/ Female                      Predictor,Character,Categorical 
#Married             Applicant married (Y/N)           Predictor,Character,Categorical
#Dependents          Number of dependents              Predictor,Numeric,Continuous
#Education           (Graduate/UnderGraduate)          Predictor,Character,Categorical
#Self_Employed       Self employed (Y/N)               Predictor,Character,Categorical
#ApplicantIncome     Applicant income                  Predictor,Numeric,Continuous
#CoapplicantIncome   Coapplicant income                Predictor,Numeric,Continuous
#LoanAmount          Loan amount in thousands          Predictor,Numeric,Continuous
#Loan_Amount_Term    Term of loan in months            Predictor,Numeric,Continuous
#Credit_History      credit history meets guidelines   Predictor,Numeric,?
#Property_Area       Urban/ Semi Urban/ Rural          Predictor,Character,Continuous
#Loan_Status         Loan approved (Y/N)               Target Variable,Numeric,Continuous

summary(train_set)

#Missing observations?
sapply(train, function(x){sum(is.na(x))})
sapply(test, function(x){sum(is.na(x))})

#Missing observations with mice
test[is.na(train$Gender),]
#Imputing Missing observations using Mice() 
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
completeData <- complete(tempData,4) #I'm using the 4th dataset. Seems like an average of the 5, and thus and thus a good suggestion for the imputed

#How does does distribution of imputed data differ from original observations?
xyplot(tempData,Gender ~ Married+Dependents+Self_Employed+LoanAmount,pch=18,cex=1)

#Or
densityplot(tempData)
stripplot(tempData, pch = 20, cex = 1.0)

#Now, lets build a random forests model and predict observations for Loan_Status
library(randomForest)

set.seed(200)

rf_model <- randomForest(Loan_Status ~ Gender+Married+Dependents+Education
                         +Self_Employed+ApplicantIncome+CoapplicantIncome+LoanAmount+
                           Loan_Amount_Term+Credit_History+Property_Area, data=completeData,
                         ntree=1500, importance=T)

#Now unto prediction...
rf_prediction <- predict(rf_model,test)
solution_rf <- data.frame(Loan_ID <- test$Loan_ID, Loan_Status <- rf_prediction)

write.csv(solution_rf,file ="solution.csv",row.names = T) 
#Model error?
plot(rf_model, ylim=c(0,0.6))
legend("topright",colnames(rf_model$err.rate),col = 1:3,fill = 1:3)

#Variable importance?
rf_importance <- importance(rf_model)

var_importance <- data.frame(Variables=row.names(rf_importance),
                             Importance=round(rf_importance[,"MeanDecreaseGini"],2))

#Rank variables based on importance
library(dplyr)
rank_importance <- rf_importance%>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

#Visualizing relative importance of Variables with ggplot2
library(ggplot2)
library(ggthemes)

ggplot(rank_importance, aes(x = reorder(Variables, Importance), 
                            y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()



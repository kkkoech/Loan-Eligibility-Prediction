load("EnvironmentData.R")

#Building decision trees with rpart
library(rpart)

rpart_tree <- rpart(Loan_Status ~ Gender+Married+Dependents+Education
                    +Self_Employed+ApplicantIncyome+CoapplicantIncome+LoanAmount+
                      Loan_Amount_Term+Credit_History+Property_Area, data=completeData,
                    method = "class")
plot(rpart_tree)
text(rpart_tree)

library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(rpart_tree)

#Now let's predict...
rpart_predictions <- predict(rpart_tree, newdata = test, type = "class") 

solution_rpart <- data.frame(Loan_ID <- test$Loan_ID, Loan_Status <- rpart_predictions)


#Using Random Forests
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

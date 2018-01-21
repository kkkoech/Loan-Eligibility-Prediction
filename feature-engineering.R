test_set <- read.csv("data/test.csv")
train_set <- read.csv("data/train.csv")

summary(train_set)

#Categorical variables
categorical_vars <- c("Gender","Married","Education","Self_Employed","Property_Area")
eda_vars <- train_set[categorical_vars]


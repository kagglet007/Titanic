setwd("C:/Kaggle - Titanic")

train <- read.csv("C:/Users/admin/Desktop/Other Ventures/Kaggle - Titanic/train.csv")
View(train)

str(train)

table(train$Survived)

prop.table(table(train$Survived))

#Now import and work with the test data set

test <- read.csv("C:/Users/admin/Desktop/Other Ventures/Kaggle - Titanic/test.csv")
View(test)

str(test)

test$Survived <- rep(0, 418) # Add a column

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)


## Tree Based Model ###
install.packages("rpart")
library(rpart)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")

plot(fit)
text(fit)

## Unnecessary visualizations of trees
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)
## Unnecessary visualizations of trees

##Write to CSV
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)
## Tree Based Model ###

## Engineering the data

test$Survived <- NA
combi <- rbind(train, test)

combi$Name <- as.character(combi$Name)
combi$Name[1]

strsplit(combi$Name[1], split='[,.]')

strsplit(combi$Name[1], split='[,.]')[[1]][2]

combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})

combi$Title <- sub(' ', '', combi$Title)

table(combi$Title)

combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'

combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

combi$Title <- factor(combi$Title)

combi$FamilySize <- combi$SibSp + combi$Parch + 1

combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})

combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")

combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")

combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

table(combi$FamilyID)

famIDs <- data.frame(table(combi$FamilyID))

famIDs <- famIDs[famIDs$Freq <= 2,]

combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

train <- combi[1:891,]
test <- combi[892:1309,]

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID, data=train, method="class")

fancyRpartPlot(fit)

Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "enggDTree.csv", row.names = FALSE)


## Finally the Random Forest Approach

##Pretreatment - Cleaning all NAs
summary(combi$Age)

Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], method="anova")

combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

summary(combi$Age)

summary(combi)

summary(combi$Embarked)

which(combi$Embarked == '')

combi$Embarked[c(62,830)] = "S"

combi$Embarked <- factor(combi$Embarked)

summary(combi$Fare)

which(is.na(combi$Fare))

combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

##Pretreatment - Cleaning all NAs

##Pretreatment - Reducing factors with over 32 levels
combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)

str(combi)

##Pretreatment - Reducing factors with over 32 levels

## Using the Random Forest
install.packages('randomForest')
library(randomForest)

set.seed(107)

train <- combi[1:891,]
test <- combi[892:1309,]

fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize +
                      FamilyID2, data=train, importance=TRUE, ntree=2000)
varImpPlot(fit)

Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "firstforest.csv", row.names = FALSE)

##A better Forest Model

install.packages('party')
library(party)

set.seed(420)

fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))

Prediction <- predict(fit, test, OOB=TRUE, type = "response")

submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)

write.csv(submit, file = "cforest.csv", row.names = FALSE)

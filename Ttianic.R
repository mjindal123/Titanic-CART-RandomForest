#titanic
setwd("C:/Users/JINDAL/Downloads")

library(readr)
train <- read_csv("C:\\Users\\JINDAL\\Downloads\\train.csv")

table(train$Survived)
prop.table(table(train$Survived))
test <- read_csv("C:\\Users\\JINDAL\\Downloads\\test.csv")
test$Survived <- rep(0, 418)
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)

#Checking the Survival rate Based on gender
table(train$Sex)
prop.table(table(train$Sex, train$Survived),1)
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
submit1 <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit1, file = "theyallperish1.csv", row.names = FALSE)

#Checking the survivalRate based on Age
summary(train$Age)
train$Child <- 0
train$Child[train$Age < 18] <- 1
aggregate(Survived ~ Child + Sex, data=train, FUN=sum)
aggregate(Survived ~ Child + Sex, data=train, FUN=length)

#Here we divide by length bcz we want the survived percentage
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

# Child    Sex  Survived
# 1     0 female 0.7528958
# 2     1 female 0.6909091
# 3     0   male 0.1657033
# 4     1   male 0.3965517

#Well, it still appears that if a passenger is female most survive, and if they were male most don't, 
#regardless of whether they were a child or not. So we haven't got anything to change our predictions
#on here. 

#Let's bin the fares into less than $10, between $10 and $20, $20 to $30 and more
#than $30 and store it to a new variable:

train$Fare2 <- '30+'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'
aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

# It's a little hard to imagine why someone in third class with an expensive ticket 
# would be worse off in the accident, but perhaps those more expensive cabins were located 
# close to the iceberg impact site, or further from exit stairs? Whatever the cause, let's make 
# a new prediction based on the new insights.
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0
submit2 <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit1, file = "theyallperish2.csv", row.names = FALSE)

library(rpart)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,data=train,method="class")
 install.packages('rattle')
 install.packages('rpart.plot')
 install.packages('RColorBrewer')
 
 library(rattle)
 library(rpart.plot)
 library(RColorBrewer)
 fancyRpartPlot(fit)
 
Prediction <- predict(fit, test, type = "class")
 submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)       
 write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)
 
 
 
 fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
 data=train,method="class", control=rpart.control(minsplit=2, cp=0))
 
 submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)       
 write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)
 
 # Feature engineering is so important to how your model performs, that even 
 # a simple model with great features can outperform a complicated algorithm 
 # with poor ones. In fact, feature engineering has been described as easily 
 # the most important factor in determining the success or failure of your 
 # predictive model.
 
 test$Survived <- NA
 combi <- rbind(train, test)
 
 # strings are automatically imported as factors in R, even if it doesn't 
 # make sense. So we need to cast this column back into a text string. 
 combi$Name <- as.character(combi$Name)
 
 
combi$Name[1]

# We see that there is a comma right after the person's last name, 
# and a full stop after their title. We can easily use the function strsplit,
# which stands for string split, to break apart our original name over these 
# two symbols.

strsplit(combi$Name[1], split = '[,.]')
#Let's go a level deeper into the indexing mess and extract the title.
strsplit(combi$Name[1], split='[,.]')[[1]][2]
# As we had to dig into this container to get the title, simply trying to 
#run combi$Title <- strsplit(combi$Name, split='[,.]')[[1]][2] over the whole 
#name vector would result in all of our rows having the same value of Mr., so 
#we need to work a bit harder. Unsurprisingly applying a function to a lot of 
#cells in a dataframe or vector uses the apply suite of functions of R:


combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
# We feed sapply our vector of names and our function that 
# we just came up with. It runs through the rows of the vector 
# of names, and sends each name to the function. The results of all 
# these string splits are all combined up into a vector as output from 
# the sapply function, which we then store to a new column in our original 
# dataframe, called Title.
combi$Title <- sub(' ', '', combi$Title)
 
table(combi$Title)
 
# Mademoiselle and Madame are pretty similar 
# so let's combine them into a single category:

combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
# So here we are combining two titles, "Mme" and "Mlle", into a new temporary 
# vector using the c() operator and seeing if any of the existing titles in the
# entire Title column match either of them

combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

 
combi$Title <- factor(combi$Title)
# Seems reasonable to assume that a large family might have trouble 
# tracking down little Johnny as they all scramble to get off the sinking 
# ship, so let's combine the two variables into a new one, FamilySize:
combi$FamilySize <- combi$SibSp + combi$Parch + 1
 
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
#Combinig surname and family size
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
table(combi$FamilyID)

#We then need to overwrite any family IDs in our dataset for groups that 
#were not correctly identified and finally convert it to a factor:
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)
 
train <- combi[1:891,]
 test <- combi[892:1309,]
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + 
Embarked + Title + FamilySize + FamilyID,data=train, method="class")
combi$Embarked[c(62,830)] = "S"
 
Prediction <- predict(fit, test, type = "class")
 
combi$Embarked <- factor(combi$Embarked)
 
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)       
write.csv(submit, file = "myfirstdtreefsize.csv", row.names = FALSE)
 
#starting with random forest
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)
combi$FamilyID2 <- combi$FamilyID
 
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)
 
install.packages('randomForest')
library(randomForest)
 
set.seed(415)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
Embarked + Title + FamilySize + FamilyID2,data=train, importance=TRUE, ntree=2000)
 
Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "firstforest.csv", row.names = FALSE)
 
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
 
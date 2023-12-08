cat("\f") # clean the console
rm(list = ls()) # clean the environment
library(readxl)

setwd("/Users/gabi/Desktop/STATFinalProject")

# Hypothesis test
hypTest <- read_excel("statfinaldata.xlsx", sheet=4)
t.test(hypTest$female, hypTest$male, var.equal=TRUE)

#reading in total data
df <- read_excel("statfinaldata.xlsx", sheet = 2)

#Splitting for ROC Curve
sample = sample(nrow(df), nrow(df)*0.80)
train <- df[sample,]
test <- df[-sample,]

# GLM for each variable
age = glm(Survived~Age, data=df, family = binomial)
print(summary(age))

sex = glm(Survived~Sex, data = df, family = "binomial")
print(summary(sex))

pclass = glm(Survived~Pclass, data = df, family = "binomial") #Answers Question 1 in Proposal
print(summary(pclass))

sibsp = glm(Survived~SibSp, data = df, family="binomial")
print(summary(sibsp))

parch = glm(Survived~Parch, data=df, family="binomial")
print(summary(parch))

fare = glm(Survived~Fare, data = df, family="binomial")
print(summary(fare))

#Answering Questions 2 and 3 from the proposal: 
ageAndSex = glm(Survived~Age + Sex, data = df, family = "binomial")
print(summary(ageAndSex))

familial = glm(Survived~SibSp + Parch, data=df, family="binomial")
print(summary(familial))

#Combining them all 
combined = glm(Survived~Age + Pclass + Sex + SibSp + Parch + Fare, data = df, family = "binomial")
print(summary(combined))

#Predictions for each variable to find accuracy of model
totalPeople <- nrow(df) # For calculating accuracy

moddedAge <- read_excel("statfinaldata.xlsx", sheet = 3)
survivedForAge <- moddedAge$Survived
totalPeopleForAge <- nrow(moddedAge)

print("Accuracies: ")

ageProb <- ifelse(predict(age, type = "response") > 0.5, 1, 0)
ageCorrect <- sum(ageProb == survivedForAge)
accuracyAge <- ageCorrect/totalPeopleForAge
print(sprintf("Age: %f%%", accuracyAge*100))

genderProb <- ifelse(predict(sex, type = "response") > 0.5, 1, 0)
genderCorrect <- sum(genderProb == df$Survived)
accuracyGender <- genderCorrect/totalPeople
print(sprintf("Gender: %f%%",  accuracyGender*100))

pclassProb <- ifelse(predict(pclass, type = "response") > 0.5, 1, 0)
pclassCorrect <- sum(pclassProb == df$Survived)
accuracyClass <- pclassCorrect/totalPeople
print(sprintf("Class: %f%%", accuracyClass*100))

sibspProb <- ifelse(predict(sibsp, type="response") > 0.5, 1, 0) 
sibspCorrect <- sum(sibspProb == df$Survived)
accuracySibsp <- sibspCorrect/totalPeople
print(sprintf("Siblings and Spouses: %f%%", accuracySibsp*100))

parchProb <- ifelse(predict(parch, type="response") > 0.5, 1, 0) 
parchCorrect <- sum(parchProb == df$Survived)
accuracyParch <- parchCorrect/totalPeople
print(sprintf("Parents and Children: %f%%", accuracyParch*100))

fareProb <- ifelse(predict(fare, type="response") > 0.5, 1, 0) 
fareCorrect <- sum(fareProb == df$Survived)
accuracyFare <- fareCorrect/totalPeople
print(sprintf("Fare: %f%%", accuracyFare*100))

ageAndSexProb <- ifelse(predict(ageAndSex, typ="response") > 0.5, 1, 0)
asCorrect <- sum(ageAndSexProb == survivedForAge)
accuracyAS <- asCorrect/totalPeopleForAge
print(sprintf("Age and Sex: %f%%", accuracyAS*100))

familialProb <- ifelse(predict(familial, type="response") > 0.5, 1, 0)
familyCorrect <- sum(familialProb == df$Survived)
accuracyFamily <- familyCorrect/totalPeople
print(sprintf("Familial Relations: %f%%", accuracyFamily*100))

combinedProb <- ifelse(predict(combined, type = "response") > 0.5, 1, 0)
combinedCorrect <- sum(combinedProb == survivedForAge)
accuracyCombined <- combinedCorrect/totalPeopleForAge
print(sprintf("All Together: %f%%", accuracyCombined*100))

# Making ROC Curve
library(pROC)
modelGLM = glm(Survived~., data = train, family="binomial")
prob = predict(modelGLM, newdata = test, type="response" )
roc = roc(test$Survived ~ prob, plot = TRUE, print.auc = TRUE)


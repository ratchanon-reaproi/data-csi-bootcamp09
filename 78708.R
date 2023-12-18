setwd("C:/Users/asus/OneDrive - KMITL/Desktop/titanic ds")

titanic.train <- read.csv(file = "train.csv", stringsAsFactors = FALSE, header = TRUE)
titanic.test <- read.csv(file = "test.csv", stringsAsFactors = FALSE, header = TRUE)

# Combine datasets
titanic.train$IsTrainSet <- TRUE
titanic.test$IsTrainSet <- FALSE
titanic.test$Survived <- NA

titanic.full <- rbind(titanic.train, titanic.test)

# Clean missing values
titanic.full[titanic.full$Embarked =='',"Embarked"] <- 'S'

# clean missing value of age & fare
#age.median <- median(titanic.full$Age, na.rm = TRUE)
#titanic.full[is.na(titanic.full$Age), "Age"] <- age.median
upper.whisker <- boxplot.stats(titanic.full$Age)$stats[5]
outlier.filter <- titanic.full$Age < upper.whisker
titanic.full[outlier.filter,]
age.equation = "Age ~ Pclass + Sex + SibSp + Fare + Parch + Embarked"
age.model <- lm( formula = age.equation,
  data = titanic.full[outlier.filter,] )
age.row <- titanic.full[is.na(titanic.full$Age), 
                         c("Pclass",  "Sex", "Fare", "SibSp", "Parch", "Embarked") ]
age.predictions <- predict(age.model, newdata = age.row)
titanic.full[is.na(titanic.full$Age), "Age"] <- age.predictions
#######################################################################################


#fare.median <- median(titanic.full$Fare, na.rm = TRUE)
#titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.median
upper.whisker <- boxplot.stats(titanic.full$Fare)$stats[5]
outlier.filter <- titanic.full$Fare < upper.whisker
titanic.full[outlier.filter,]
fare.equation = "Fare ~ Pclass + Sex + Age + SibSp + Parch + Embarked"
fare.model <- lm(
  formula = fare.equation,
  data = titanic.full[outlier.filter,]
)
fare.row <- titanic.full[is.na(titanic.full$Fare), 
                         c("Pclass",  "Sex", "Age", "SibSp", "Parch", "Embarked") ]

fare.predictions <- predict(fare.model, newdata = fare.row)
titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.predictions
#######################################################################################

# Convert categorical variables to factors
as.factor(titanic.full$Pclass)
as.factor(titanic.full$Sex)
as.factor(titanic.full$Embarked)

# Split data back into train & test
titanic.train <- titanic.full[titanic.full$IsTrainSet==TRUE,]
titanic.test <- titanic.full[titanic.full$IsTrainSet==FALSE,]

titanic.train$Survived <- as.factor(titanic.train$Survived)

Survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
Survived.formula <- as.formula(Survived.equation)
install.packages("randomForest")
library(randomForest)

# Train random forest model
titanic.model <- randomForest(
  formula = Survived.formula,
  data = titanic.train,
  ntree = 1500,  # Adjust the number of trees(1500)
  mtry = 2,      # Adjust the number of variables considered at each split(2)
  nodesize = 0.01 * nrow(titanic.test)
)

features.equation <- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"

# Prediction
Survived <- predict(titanic.model, newdata = titanic.test)

# Prepare output
PassengerId <- titanic.test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- Survived

write.csv(output.df, file = "kaggle_submission03.csv", row.names = FALSE)


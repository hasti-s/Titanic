# Import the training set: train
train_url <- "Documents/Data Mining/HW1/train.csv"
train <- read.csv(train_url)
  
# Import the testing set: test
test_url <- "Documents/Data Mining/HW1/test.csv"
test <- read.csv(test_url)

str(train)
str(test)

library('dplyr')

full <- bind_rows(train, test)

# Two-way comparison
#prop.table(table(train$Child, train$Survived ), 1)

#title
# Grab title from passenger names
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)

# Show title counts by sex
table(full$Sex, full$Title)
# Titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# Also reassign mlle, ms, and mme accordingly
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'

# Show title counts by sex again
table(full$Sex, full$Title)
full$Surname <- sapply(full$Name,  
                       function(x) strsplit(x, split = '[,.]')[[1]][1])


#Sensible value imputation
library('ggplot2')

table(full$Embarked)

pIds <- full[full$Embarked == names(table(full$Embarked))[1],"PassengerId"]

embarke_fare <- full[full$Embarked != names(table(full$Embarked))[1], ]

g <- ggplot(embarke_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() + 
  geom_hline(aes(yintercept=80))

print(g)
#migim ke khate ro mode embarked = 'C' e pas inayi k meghdar nadaran ham 'C' an
train$Embarked[ pIds ] <- 'C' 

#Age Age
library('mice')

sum(is.na(full$Age))

# Make variables factors into factors
factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','Surname')
full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

set.seed(456)

mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Surname','Survived')], method='rf') 

# Save the complete output 
mice_output <- complete(mice_mod)

# Plot age distributions
par(mfrow=c(1,2))
hist(full$Age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))

# Replace Age variable from the mice model.
full$Age <- mice_output$Age

# Show new number of missing Age values
sum(is.na(full$Age))

# Create the column child, and indicate whether child or no child
train$Child <- NA
train$Child[train$Age < 18] <- 1
train$Child[train$Age >= 18] <- 0

library(rpart)
train <- full[1:891,]
test <- full[892:1309,]
# Build the decision tree
my_tree_two <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title, data = train, method = "class")
'
# Visualize the decision tree using plot() and text()
plot(my_tree_two)
text(my_tree_two)

# Load in the packages to build a fancy plot
#library(rattle)
#library(rpart.plot)
library(RColorBrewer)


# Time to plot your fancy tree
fancyRpartPlot(my_tree_two)
'

# my_tree_two and test are available in the workspace

# Make predictions on the test set
my_prediction <- predict(my_tree_two, newdata = test, type = "class")

# Finish the data.frame() call
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)

# Use nrow() on my_solution
nrow(my_solution)

# Finish the write.csv() call
write.csv(my_solution, file = "Documents/Data Mining/HW1/my_solution.csv", row.names = FALSE)
print("Finish")

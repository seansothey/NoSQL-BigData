
#Load the dataset
abalone = read.csv("C:\\Users\\seans\\Desktop\\AI Class\\R Programming\\Abalone.csv", header = TRUE)
head(abalone)
summary(abalone)


# See the histogram of the dataset
library(Hmisc)
hist.data.frame(abalone)

# Check if there're high correlation attributes 
library(corrplot)
corr = cor(abalone[-c(1)])
corrplot(corr, method="number")

#                       Data Exploratory Report
# Interactive data exploratory is available @ https://seansothey.github.io/MyProject/Abalone.html
# There are 9 variables, 8 is numerical and 1 is categorical
# Number of observations is 4177
# There is no  missing cell or duplicated row
# High correlation variables >= 0.9:
#   -Length: Diameter, Whole weight, Shucked weight, Viscera weight, Shell weight 
#   -Diameter: Length, Whole weight, Viscera weight, Shell weight
#   -Whole weight: Length, Diameter, Shucked weight, Viscera weight, Shell weight
#   -Shucked weight: Length, Whole weight, Viscera weight
#   -Viscera weight: Length, Diameter, Whole weight, Shuck weight, Shell weight
#   -Shell weight: Length, Diameter, Whole weight, Viscera weight

# The target Rings is presented in numerical value which is suitable with regression model.
# In this cassification task, we want to predict the value of Rings using decision tree model.
# Therefore, factoring (convert to catgorical) the labels is indispensable.


# Drop some high correlation attributes
keeps = c("Sex", "Length", "Height", "Shucked.weight", "Shell.weight", "Rings")
data = abalone[keeps]

# Factoring/Converting the target Rings
data$Rings = as.factor(data$Rings)
str(data$Rings)

# Create training and testing set with ratio 80:20
# Random sample indexes
set.seed(123)
train = sample(1:nrow(data), 0.8 * nrow(data))
test = setdiff(1:nrow(data), train)

# Build X_train, y_train, X_test, y_test
X_train <- data[train, -6]
y_train <- data[train, "Rings"]

X_test <- data[test, -6]
y_test <- data[test, "Rings"]


######## Rule-Base Model ########
library(C50)
rb_model = C5.0(X_train, y_train, rules=TRUE)
summary(rb_model)

# Make prediction on training set and check accuracy
pred_rb_train = predict(rb_model, X_train, type="class")
accuracy_rb_train = sum(pred_rb_train == y_train) / length(y_train)
paste0((accuracy_rb_train * 100), "% accuracy")

pred_rb_train

# Make the prediction on test set and check accuracy
pred_rb_test = predict(rb_model, X_test, type="class")
accuracy_rb_test = sum(pred_rb_test == y_test) / length(y_test)
paste0((accuracy_rb_test * 100), "% accuracy")


# Confusion Matrix on test set
library(caret)
confusionMatrix(table(pred_rb_test, y_test))


######## Boosting the model with pruning ########

prune_model = C5.0(X_train, y_train, control=C5.0Control(minCases=50), trials=50)
plot(prune_model, main="C5.0 Decision Tree - Pruned")
summary(prune_model)

# Make prediction on training set and check accuracy
pred_prune_train = predict(prune_model, X_train, type="class")
accuracy_prune_train = sum(pred_prune_train == y_train) / length(y_train)
paste0((accuracy_prune_train * 100), "% accuracy")

# Make the prediction on test set and check the accuracy
pred_prune_test = predict(prune_model, X_test, type="class")
accuracy_prune_test = sum(pred_prune_test == y_test) / length(y_test)
paste0((accuracy_prune_test * 100), "% accuracy")

# Confusion Matrix on test set
confusionMatrix(table(pred_prune_test, y_test))


######## Summary & Conclusion ########

result = data.frame("Model" = c("Train Accuracy", "Test Accuracy"),
                    "Rule-Base" = c(accuracy_rb_train, accuracy_rb_test),
                    "Pruned" = c(accuracy_prune_train, accuracy_prune_test),
                    "Status" = c("Overfitting", "Generalized"))
result

# By looking at the result from above experiment, both models seem not perform well beacuse of
# the non-linear separatable observations of dataset, plus 28 different classes to predict.
# As mentioned above, Regression model would perform better. However, we can improve the accuracy
# by reducing the class by creating range for Rings. For example, I have experimented with the ranges of 5
# (1-5, 6-10, 11-15, 16-15, 26-29) of the Rings value, and the accuracy on test set have improved to 70%.
# Rule-Base model tend to be overfitting because the train accuracy is way higher than the test.
# Pruned model has reduced the overfitting by generalize and simplify the tree, and increase the test accuracy. 


##### Plot Result #####
table = data.frame("Type" = c("Train", "Train", "Test", "Test"),
                   "Model" = c("Rule-Base", "Pruned", "Rule-Base", "Pruned"),
                   "Accuracy" = c(accuracy_rb_train,
                                  accuracy_prune_train,
                                  accuracy_rb_test,
                                  accuracy_prune_test))

ggplot(table, aes(x=Model, y=Accuracy, fill=Type)) +
  geom_bar(stat="identity", position=position_dodge())





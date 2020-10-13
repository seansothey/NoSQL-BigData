#Load the dataset
data = read.csv("C:\\Users\\seans\\Desktop\\AI Class\\R Programming\\winequality-red.csv", header = TRUE)
head(data)

# See the histogram of the dataset
library(tidyr)
library(ggplot2)
ggplot(gather(data), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')

# Check if there're high correlation attributes 
library(corrplot)
corr = cor(data[-c(1)])
corrplot(corr, method="number") # found no high correlatin features 

# Even there's no finding of high correlation above;
# however, we can still reduce the less important independent features by checking the score
# See the important features
library(earth)
imp = earth(quality ~ ., data=data) # build model
ev = evimp(imp) # estimate variable importance
ev
plot(ev)

# Let's drop less important features not in the list 
to_drop = c("density", "residual.sugar") 
data = data[, !(names(data) %in% to_drop)]

# Check if there're missing values
sum(is.na(data))  # return 0 means no missing value, good to go


# Create training and testing set with ratio 70:30
# Random sample indexes
set.seed(123)
train_index = sample(1:nrow(data), 0.7 * nrow(data))
test_index = setdiff(1:nrow(data), train_index)

# Build train and test set
train = data[train_index,]
test = data[test_index,]

# Scale independent features in range 0-1
scale = function(x){(x-min(x))/(max(x)-min(x))}
train[,-10] = scale(train[,-10])
test[,-10] = scale(test[,-10])
head(train)

# See the new histogram of after scale
ggplot(gather(train), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')
# The histogram is not much different beacuse the dataset value is not in a huge different range


# Converting the target to binary
library(nnet)
train_nn = cbind(train[, 1:9], class.ind(as.factor(train$quality)))
test_nn = cbind(test[, 1:9], class.ind(as.factor(test$quality)))
head(train_nn)

# Set labels name
names(train_nn) = c(names(train_nn)[1:9],"Q3","Q4","Q5","Q6","Q7","Q8")
n = names(train_nn)
n

# Set up formula for training NN model
f = as.formula(paste("Q3+Q4+Q5+Q6+Q7+Q8 ~", paste(n[!n %in% c("Q3","Q4","Q5","Q6","Q7","Q8")], collapse = " + ")))
f

# Fit NN model with different hidden layers and neurons
library(neuralnet)
set.seed(123)

# 1 hidden layers with 3 neurons
NN1 = neuralnet(f,
               data = train_nn,
               hidden = c(3),
               act.fct = "tanh",
               linear.output = FALSE,
               stepmax = 1e+05,
               threshold = 0.1) #increase threshold & stepmax helps convergence

# Plot NN1 model
plot(NN1, rep = "best")
Error1 = as.data.frame(NN1$result.matrix)[1,]
Error1

# 2 hidden layers with 6,3 neurons
NN2 = neuralnet(f,
                data = train_nn,
                hidden = c(6, 3),
                act.fct = "tanh",
                linear.output = FALSE,
                stepmax = 1e+06,
                threshold = 0.2)

# Plot NN2 model
plot(NN2, rep = "best")
Error2 = as.data.frame(NN2$result.matrix)[1,]
Error2

# 3 hidden layers with 6,3,3 neurons
NN3 = neuralnet(f,
                data = train_nn,
                hidden = c(6, 3, 3),
                act.fct = "tanh",
                linear.output = FALSE,
                stepmax = 1e+06,
                threshold = 0.4)  #increase threshold to 0.4

# Plot NN3 model
plot(NN3, rep = "best")
Error3 = as.data.frame(NN3$result.matrix)[1,]
Error3

  
##### Plot Result #####
result = data.frame("Model" = c("NN1", "NN2", "NN3"),
                   "Error" = c(Error1, Error2, Error3))
result

library(ggplot2)
ggplot(result, aes(x=Model, y=Error, fill=Model)) + 
  geom_bar(stat = "identity")


# We choose less error model, NN3, for the prediction model
# Make prediction on training set
pred_train = compute(NN3, train_nn[-10])$net.result
head(pred_train)

# Make the prediction on test set
pred_test = compute(NN3,test_nn[-10])$net.result
head(pred_test)

###### Extra Credit ######

# Convert binary output back to categorical
maxidx = function(arr) {
  return(which(arr == max(arr)))
}
idx_train = apply(pred_train, c(1), maxidx)
idx_test = apply(pred_test, c(1), maxidx)

### Training Set Findings
prediction_train = c(3,4,5,6,7,8)[idx_train]
head(prediction_train)
accuracy_train = mean(prediction_train == train$quality)*100
table(prediction_train, train$quality) #missed classes predicted (imbalance class dataset) 

#Check Specificity and Sensitivity, from this Recall and F1 can also be found
library(caret)
confusionMatrix(as.factor(prediction_train), as.factor(train$quality))

### Testing Set Findings
prediction_test = c(3,4,5,6,7,8)[idx_test]
head(prediction_test)
accuracy_test = mean(prediction_test == test$quality)*100
table(prediction_test, test$quality) #missed classes predicted (imbalance class dataset) 

#Check Specificity and Sensitivity, from this Recall and F1 can also be found
confusionMatrix(as.factor(prediction_test), as.factor(test$quality))


### Accuracy Report ####
report = data.frame("Set" = c("Train", "Test"),
                    "Accuracy" = c(accuracy_train, accuracy_test))
report

library(ggplot2)
ggplot(report, aes(x=Set, y=Accuracy, fill=Set)) + 
  geom_bar(stat = "identity")

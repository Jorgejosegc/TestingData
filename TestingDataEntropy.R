library(readxl)
library(openxlsx)
library(ggplot2)
library(rpart)
library(knitr)
library(RWeka)

#Question 1 -
DiabetesProject <- read.xlsx("C:/Users/jorge/Desktop/Roosevelt/CST 309 Data Mining/diabetes.xlsx")
project<-DiabetesProject

#It will display the whole table in a better perspective and will show the entry number at the end
project$row_number <- seq(nrow(project))
kable(project, format = "markdown")

#This will be for display specific line to a specific line, we need to change the number of line we wanna see. in this case from 1 to 80
print(project[1:75,])    

#This functions will convert the integers 0 and 1 from the class column into factors.
project$class <- as.factor(project$class)    

#This function will allow me to make sure that the class attribute is now a factor and we will prove it reading the data.
str(project)


#Question 2 - Separate the data into learning set (~2/3 of all records) and test data
set.seed(123)
learning_set <- sample(1:nrow(project), nrow(project) * 2 / 3)
test_set <- setdiff(1:nrow(project), learning_set)

learning_data <- project[learning_set, ]
test_data <- project[test_set, ]

d<-"\n\nNumber of records in learning set -------->"
fm1 <- sprintf("\033[1m%s\033[0m", d)
d1<-"\n\nNumber of records in test data -------->"
fm2 <- sprintf("\033[1m%s\033[0m", d1)
cat(fm1,nrow(learning_data))
cat(fm2,nrow(test_data))

ind<- learning_set


#Question 3 - 
# Classification trees using CART (rpart)
model.dt<-rpart(class~.,data=project,method="class",parms=list(split='information')) 
# Classification trees using  C4.5 (J48)
C45T <- J48(class ~ ., data = project[ind==1,], control = Weka_control(U =TRUE, M = 5));


#Question 4 - Display rpart
#Plot for the rpart
plot(model.dt,uniform=TRUE,branch=0.2,margin=0.1,main="Entropy")
text(model.dt, use.n=TRUE,cex=.7)


#Question 5 and 6 -
#For the rpart
# Obtain predictions from the rpart model
pred.model.dt <- predict(model.dt, test_data, type = "class")

# Compare the actual class with the predicted class using table()
table(test_data$class, pred.model.dt, dnn = c("Actual class", "Predicted class"))
acc.model.dt <- 100*sum(pred.model.dt==test_data$class)/dim(test_data)[1];
b<-"\n\nAccuracy for the rpart Tree ------>"
fm <- sprintf("\033[1m%s\033[0m", b)
cat(fm,acc.model.dt)

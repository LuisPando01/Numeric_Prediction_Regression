#EXTRACT DATA
insurance <- read.csv("insurance.csv",stringsAsFactors = TRUE)

#LIBRARY
library(tidyverse)
install.packages("tidyverse")

#DATA AND DISTRIBUTION
str(insurance)
summary(insurance)
summary(insurance$expenses)
hist(insurance$expenses)

#EXPLOR CATEGORICAL VARIABLES
table(insurance$region)

#CORRELATION BETWEEN VARIABLES
cor(insurance[c("age","bmi","children","expenses")])

#GRAPHIC TO EXPLORE ALL CORRELATIONS
pairs.panels(insurance[c("age","bmi","children","expenses")])
#The rounder the circle in the image, the less correlation there will be


#MULTIPLE LINEAR REGRESSION
ins_model <- lm(expenses ~ age + children + bmi + sex + smoker + region,data =
                  insurance)
ins_model

#START TO TRY THE MODEL
set.seed(123)
#TAKE 5% OF THE ROWS AS THE TEST ROWS
test_sample<-sample(nrow(insurance),0.05*nrow(insurance))
#TEST DATASET
insurance_test<-insurance[test_sample,]
str(insurance_test)
head(insurance_test)
#ROW 415
str(test_sample)
insurance_test[1,]

#MODEL FIRST TRY
predict(ins_model, insurance_test[1,])
#VECTOR IS CREATED WITH THE PREDICTIONS ACCORDING TO THE MODEL AND RANDOMLY TAKEN ROWS
ins_pred <- predict(ins_model, insurance_test)
#TAKE OUT THE ROW NAME
ins_p <- unname(ins_pred)

#CORRELATION BETWEEN REAL EXPENSES AND PREDICTION
cor(insurance_test$expenses,ins_p)

#CHART COMPARING PREDICTION AND REALITY
plot(insurance_test$expenses, type="l", main="Linear Regression:
Predicted versus Actual Expenses", xlab="record",
     ylab="expenses in $",axes=FALSE, frame.plot=TRUE)
axis(1, at=c(0,10,20,30,40,50,60),labels=FALSE)
lines(ins_p,col="blue",lty=2)
legend("topleft",inset=.02,c("actual","predicted"),lty=c(1,2),
       col=c("black","blue"))

#EVALUATE MODEL RESSULTS
summary(ins_model)
#Age and BMI (30+ probably) are more related to the outcome
#Smoking is also related, but smoking and obesity can be even worse

###LETS IMPROVE THE MODEL###
#Enter new column for age (since it increments as a quadratic F)
insurance$age2 <- insurance$age^2

#Insert new column: whether or not over 30 years old
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)

#NEW AND IMPROVED MODEL
ins_model2 <- lm(expenses ~ age + age2 + children + bmi +
                   sex + bmi30*smoker + region, data = insurance)
#VERIFY MODEL
summary(ins_model2)

#Looking at the results, it can be seen that the model improved
head(insurance)
predict(ins_model2, insurance[3,])

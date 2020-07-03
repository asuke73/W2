
Bank1.df = read.csv("BankMarketing2.csv", header = TRUE)
summary(Bank1.df)
str(Bank1.df)
all.empty = rowSums(is.na(Bank1.df))==ncol(Bank1.df)
sum(!complete.cases(Bank1.df))
Bank1.df[Bank1.df=="unknown"]<- NA

## format some variables - to Factors
Bank1.df$Job <- as.factor(Bank1.df$Job)
Bank1.df$Education <- as.factor(Bank1.df$Education)
Bank1.df$Marital <- as.factor(Bank1.df$Marital)
Bank1.df$Default <- as.factor(Bank1.df$Default)
Bank1.df$Housing <- as.factor(Bank1.df$Housing)
Bank1.df$Loan <- as.factor(Bank1.df$Loan)
Bank1.df$Contact<- as.factor(Bank1.df$Contact)
Bank1.df$Month <- as.factor(Bank1.df$Month)
Bank1.df$Poutcome <- as.factor(Bank1.df$Poutcome)
Bank1.df$Campaign <- as.factor(Bank1.df$Campaign)
Bank1.df$Y<- as.factor(Bank1.df$Y)
str(Bank1.df)

## Two way Table
xtabs(~Housing + Previous + Loan, data = Bank1.df)## 2 way Table
# Partition data - train (70%) & test (30%)
set.seed(12345)
sample(2, nrow(Bank1.df), replace = T, prob = c(0.7,0.3))
feedback <- sample(2, nrow(Bank1.df), replace = T, prob = c(0.7, 0.3))
train <- Bank1.df[feedback == 1,]
test <- Bank1.df[feedback == 2,]

## Logistic Model
xtabs(~Age + Previous + Campaign, data = Bank1.df)## 2 way Table
# Partition data - train (70%) & test (30%)
set.seed(12345)
sample(2, nrow(Bank1.df), replace = T, prob = c(0.7,0.3))
feedback <- sample(2, nrow(Bank1.df), replace = T, prob = c(0.7, 0.3))
train <- Bank1.df[feedback == 1,]
test <- Bank1.df[feedback == 2,]
glm(Housing ~ Previous + Campaign, data = Bank1.df, family = "binomial")
mymodel<- glm(Housing~ Previous + Campaign, data = Bank1.df, family = "binomial" )
summary(mymodel)

## Confusion Matrix
p1 <- predict(mymodel,train,type = "response")
head(p1)
p1 <- ifelse(p1>0.5,1,0)
table <- table(predicted = p1, Actual = train$Housing)
table

sum(diag(table))/sum(table)
1-sum(diag(table))/sum(table)
17489/45211

## viewing which attributes would match as the best variables to predict the model
attach(Bank1.df)
library(ggplot2)
plot(Age, Balance, xlab="Age",ylab="Balance")


attach(Bank1.df)
install.packages("ggplot2")
library(ggplot2)
plot(Campaign, Balance, xlab="Poutcome",ylab="Balance")




























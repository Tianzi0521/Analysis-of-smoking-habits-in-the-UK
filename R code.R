#logistic
smoking_log = read.csv('smoking.csv')
View(smoking_log)

## Generate numerical values for type variables
smoking_log$gender <- ifelse(smoking_log$gender == "Male", 1,0)
smoking_log$smoke <- ifelse(smoking_log$smoke == "No", 0, 1)

## Generate new variables
#marital_status
smoking_log$single <- ifelse(smoking_log$marital_status == "Single", 1, 0)
smoking_log$married <- ifelse(smoking_log$marital_status == "Married", 1, 0)  #Divorced, Separated and Widowed are treated as a group 'Other' 
smoking_log$marital_status <- NULL
#ethnicity
smoking_log$asian <- ifelse(smoking_log$ethnicity == "Asian", 1, 0)
smoking_log$black <- ifelse(smoking_log$ethnicity == "Black", 1, 0)
smoking_log$chinese <- ifelse(smoking_log$ethnicity == "Chinese", 1, 0)
smoking_log$mixed <- ifelse(smoking_log$ethnicity == "Mixed", 1, 0)
smoking_log$white <- ifelse(smoking_log$ethnicity == "White", 1, 0)
smoking_log$ethnicity <- NULL
#gross_income  (use the mean in each range)
smoking_log$gross_income <- ifelse(smoking_log$gross_income == "Under 2,600", 1300, 
                                   ifelse(smoking_log$gross_income == "2,600 to 5,200", 3900,
                                          ifelse(smoking_log$gross_income == "5,200 to 10,400", 7800,
                                                 ifelse(smoking_log$gross_income == "10,400 to 15,600", 13000,
                                                        ifelse(smoking_log$gross_income == "15,600 to 20,800", 18200,
                                                               ifelse(smoking_log$gross_income == "20,800 to 28,600", 24700,
                                                                      ifelse(smoking_log$gross_income == "28,600 to 36,400", 32500,36400)))))))

## Delete useless varaibles 
smoking_log[, c('nationality', 'highest_qualification', 'region', 'amt_weekdays','amt_weekends','type')] <- list(NULL)

## Logistic Regression
View(smoking_log)
glm(smoke~., data = smoking_log,family=binomial) -> glm.all
glm(smoke~age, data = smoking_log,family=binomial) -> glm.onlyage
summary(glm.all)
summary(glm.onlyage)

##error rate of glm.all
dim(smoking_log)
predict(glm.all,type="response")->glm.all.probs
glm.all.pred=rep("0",1691)#repeat
glm.all.pred[glm.all.probs>.5]="1"



attach(smoking_log)
class(smoking_log$smoke)
contrasts(as.factor(smoke))## check dummy variable: 1 or 0; 
## in this case: 1=1; 0=0
table(glm.all.pred,smoke)
mean(glm.all.pred==smoke)
mean(glm.all.pred!=smoke)

##error rate of glm.onlyage

predict(glm.onlyage,type="response")->glm.onlyage.probs
glm.onlyage.pred=rep("0",1691)#repeat
glm.onlyage.pred[glm.onlyage.probs>.5]="1"

attach(smoking_log)
table(glm.onlyage.pred,smoke)
mean(glm.onlyage.pred==smoke)
mean(glm.onlyage.pred!=smoke)

##best subset selection

dim(smoking_log)
library(leaps)
regsubsets(smoke~.,data=smoking_log,nvmax=10)->smoke.best
summary(smoke.best)->sum.smoke.best

names(sum.smoke.best)
sum.smoke.best$rsq
sum.smoke.best$adjr2
par(mfrow=c(1,3))
plot(sum.smoke.best$cp)
plot(sum.smoke.best$bic)
plot(sum.smoke.best$adjr2)
which.max(sum.smoke.best$adjr2) # function which.max() returns the index of maximum value
which.min(sum.smoke.best$cp)
which.min(sum.smoke.best$bic)

coef(smoke.best,9)
coef(smoke.best,6)
coef(smoke.best,3)

#best subset selection when considering 9 variables
glm(smoke~.-mixed, data = smoking_log,family=binomial) -> glm.9
summary(glm.9)

#error rate of glm.9

predict(glm.9,type="response")->glm.9.probs
glm.9.pred=rep("0",1691)#repeat
glm.9.pred[glm.9.probs>.5]="1"

attach(smoking_log)
table(glm.9.pred,smoke)
mean(glm.9.pred==smoke)
mean(glm.9.pred!=smoke)


glm(smoke~.-mixed, data = smoking_log,family=binomial) -> glm.9
summary(glm.9)


#best subset selection when considering 9 variables
glm(smoke~.-mixed, data = smoking_log,family=binomial) -> glm.6
summary(glm.6)

#error rate of glm.6

glm(smoke~.-mixed-black-chinese-white, data = smoking_log,family=binomial) -> glm.6
summary(glm.6)

predict(glm.6,type="response")->glm.6.probs
glm.6.pred=rep("0",1691)#repeat
glm.6.pred[glm.6.probs>.5]="1"

attach(smoking_log)
table(glm.6.pred,smoke)
mean(glm.6.pred==smoke)
mean(glm.6.pred!=smoke)

#best subset selection when considering 9 variables
glm(smoke~.-mixed, data = smoking_log,family=binomial) -> glm.6
summary(glm.6)

#error rate of glm.3

glm(smoke~age+gross_income+married, data = smoking_log,family=binomial) -> glm.3
summary(glm.3)

predict(glm.3,type="response")->glm.3.probs
glm.3.pred=rep("0",1691)#repeat
glm.3.pred[glm.3.probs>.5]="1"

attach(smoking_log)
table(glm.3.pred,smoke)
mean(glm.3.pred==smoke)
mean(glm.3.pred!=smoke)



# linear regression
library(ISLR)
library(readr)

smoking_jy <- smoking[complete.cases(smoking), ]
head(smoking_jy)
model_jy <- lm(amt_weekdays~age, data = smoking)

summary(model_jy)
summary(smoking_jy$age)
summary(smoking_jy$amt_weekdays)

# Hypothesis Test
library(openintro)
library(tidyverse)
library(dplyr)

new_smoking <- smoking %>% filter(!is.na(amt_weekdays)) %>% filter(!is.na(amt_weekends))
summary_t <- new_smoking %>% summarise(n = n(), mean = mean(amt_weekdays), sd = sd(amt_weekdays))
t <- (summary_t$mean-15)/(summary_t$sd/sqrt(nrow(new_smoking)))
p_value <- pt(t,nrow(new_smoking)-1)  
p_value

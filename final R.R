#Statistics for Business
#Individual Project
#Assignment 2
#Vasileios Gounaris Bampaletsos - 40314803



#set the working directory
setwd("/Users/basilisgounarismpampaletsos/Desktop/SEMESTER 1/PROJECTS 1/18:12 - final ass")
options(scipen = 9)

#load the libraries
library(readxl)
library(psych)
library(ggplot2)
library(caTools)
library(statsr)
library(dplyr)
library(BAS)
library(car)
library(tidyr)
library(purrr)
library(gridExtra)
library(forcats)
library(corrplot)
library(magrittr)
library(caret)
library(Hmisc)
library(tidyverse)
library(ggpubr)
library(ROCR)
library(broom)

#read the data (test and train)
train_set <- read_excel("bank_train.xlsx")
test_set <- read_excel("bank_test.xlsx")

######################
#DATA UNDERSTANDING
######################

###
#summarise the data
#first view of our data structure and quality
###
summary(train_set)
summary(test_set)


summary(train_set$subscribed)
summary(train_set$duration)
summary(train_set$cons_price_idx)
##
#Visualizing - Categorial Variables
##

ggplot(mutate(train_set, education = fct_infreq(education))) + 
  geom_bar(aes(x = education)) +
  labs(x="Level of Education", y= "count", title="Distribution of education level")

ggplot(train_set, aes(month)) +
  geom_bar(colour="black", mapping = aes(fill = subscribed)) +
  labs(x="No. of contacts in this campaign", y= "count", 
       title="Distribution of the number of contacts performed during this campaign")

ggplot(mutate(train_set, job = fct_infreq(job))) + 
  geom_bar(aes(x = job)) +
  labs(x="Type of job", y= "count", title="Distribution of jobs' types")


ggplot(mutate(train_set, day_of_week = fct_infreq(day_of_week))) + 
  geom_bar(aes(x = day_of_week)) +
  labs(x="Days of the week", y= "count", title="Marketing campaing last contacted day")


ggplot(train_set, aes(default)) +
  geom_bar(colour="black", mapping = aes(fill = subscribed)) +
  labs(fill="subscribed", x="default", y= "count", title="Credit status of the people", 
       caption="3 possible answers of credit status")

ggplot(train_set, aes(housing)) +
  geom_bar(colour="black", mapping = aes(fill = subscribed)) +
  labs(fill="subscribed", x="housing loan", y= "count", title="Answers distribution for  Housing Loan", 
       caption="3 possible answers of housing loan")

ggplot(train_set, aes(loan)) +
  geom_bar(colour="black", mapping = aes(fill = subscribed)) +
  labs(fill="subscribed", x="personal loan", y= "count", title="Answers distribution for Personal Loan", 
       caption="3 possible answers of loan")

ggplot(train_set, aes(marital_status)) +
  geom_bar(colour="black", mapping = aes(fill = subscribed)) +
  labs(fill="subscribed", x="marital_status", y= "count", title="Answers distribution for marital status", 
       caption="3 possible answers of marital status")

ggplot(train_set, aes(contact)) +
  geom_bar(colour="black", mapping = aes(fill = subscribed)) +
  labs(fill="subscribed", x="contact", y= "count", title="Communication type during the marketing campaign", 
       caption="2 types of communication")

ggplot(train_set, aes(poutcome)) +
  geom_bar(colour="black", mapping = aes(fill = subscribed)) + 
  labs(fill="subscribed", x="poutcome", y= "count", title="Outcome of the previous marketing campaign", 
       caption="3 types of outcome")


ggplot(train_set, aes(subscribed)) +
  geom_bar(colour="black", mapping = aes(fill = subscribed)) +
  labs(fill="subscribed", x="subscribed", y= "count", title="Checking the distribution of the target variable", 
       caption="2 types of answers")


##
#Visualizing - Continuous Variables
##
a <- ggplot(train_set, aes(x = age))
a + geom_density(aes(y = ..count..), fill = "lightgray") +
    geom_vline(aes(xintercept = mean(age)), linetype = "dashed", size = 0.6,color = "#FC4E07") +
    labs(fill="subscribed", x="age", y= "count", title="Age distribution")


b <- ggplot(train_set, aes(x = duration))
b + geom_density(aes(y = ..count..), fill = "lightgray") +
    geom_vline(aes(xintercept = mean(duration)), linetype = "dashed", size = 0.6, color = "#FC4E07") +
    labs(fill="subscribed", x="duration", y= "count", title="Marketing campaign last contacted duration(seconds)", 
         caption="With the mean line in red")


c <- ggplot(train_set, aes(x = cons_price_idx))
c + geom_density(aes(y = ..count..), fill = "lightgray") +
    geom_vline(aes(xintercept = mean(cons_price_idx)), linetype = "dashed", size = 0.6, color = "#FC4E07") +
    labs(fill="subscribed", x="Consumer price index", y= "count", title="Distribution of the consumer price index (Monthly Indicator)", 
         caption="With the mean line in red")


d <- ggplot(train_set, aes(x = pdays))
d + geom_density(aes(y = ..count..), fill = "lightgray") +
    geom_vline(aes(xintercept = mean(pdays)), linetype = "dashed", size = 0.6, color = "#FC4E07") +
     labs(fill="subscribed", x="No. of the days", y= "count", title="Number of the days that pasted from a previous campaign", 
          caption="With the mean line in red")

ggplot(train_set) + 
  geom_point(mapping = aes(x = age, y = duration)) + 
  labs(x="age", y= "duration", title="Age by duration of the last contacted duration (seconds)", 
       caption="Scatter plot to see the values distribution")


ggplot(train_set) + 
  geom_point(mapping = aes(x = cons_price_idx, y = cons_conf_idx)) +
  labs(x="consumer price index", y= "consumer confidence index", title="Consumer's Price and Confidence Index", 
       caption="Scatter plot to see the values distribution")


ggplot(train_set) + 
  geom_smooth(mapping = aes(x = emp_var_rate, y = nr_employed)) +
  labs(x="Employment variation rate", y= "Number of employments", title="Employment's values", 
       caption="Scatter plot to see the values distribution")


ggplot(train_set, aes(euribor3m)) +
  geom_bar(colour="black", mapping = aes(fill = euribor3m)) + 
  labs(x="Euribor 3 month rate", y= "count", title="Euribor's distribution")


ggplot(train_set, aes(previous)) +
  geom_bar(colour="black", mapping = aes(fill = subscribed)) +
  labs(x="No. of contacts before this campaing", y= "count", title="Distribution of the previous campaigns")


ggplot(train_set, aes(campaign)) +
  geom_bar(colour="black", mapping = aes(fill = subscribed)) +
  labs(x="No. of contacts in this campaign", y= "count", 
       title="Distribution of the number of contacts performed during this campaign")


###################
#DATA PREPERATION
###################

##
#train set preperation
##
train_set$ID <- NULL
summary(train_set)
train_set <- train_set %>% mutate_if(is.character, as.factor) #set the variables in train_set as factor

#fix the outliers and data issues in train_set
train_set$age[train_set$age < 18] <- NA #fix the age over 18
train_set$marital_status[train_set$marital_status == "m"] <- "married" #convert the m in married
train_set$month[train_set$month == "Mar"] <- "mar" #convert the Mar(data issue) to mar
train_set$month[train_set$month == "Nov"] <- "nov" #convert the Nov(data issue) to nov
train_set$month[train_set$month == "May"] <- "may" #convert the May(data issue) to may
train_set$duration[train_set$duration > 2000] <- NA #fix the outliers over 2500
train_set$campaign[train_set$campaign > 20] <- NA #fix the outliers over 25
train_set$housing[train_set$housing == "NA"] <- "unknown" #convert NAs to unkown value
train_set$loan[train_set$loan == "NA"] <- "unknown" #fix the 999 outlier
train_set$cons_conf_idx[train_set$cons_conf_idx == 999] <- NA #fix the 999 outlier

train_set$pdays <- ifelse(train_set$pdays == 999, 0, 1) #convert 999 to 0 and everything else in 1


##
#test_set preperation
##
test_set$ID <- NULL
summary(test_set)
test_set <- test_set %>% mutate_if(is.character, as.factor) #set the variables in test_set as factor

#fix the outliers and data issues in test_set
test_set$age[test_set$age < 18] <- NA #fix the age over 18
test_set$marital_status[test_set$marital_status == "m"] <- "married" #convert the m in married
test_set$month[test_set$month == "Mar"] <- "mar" #convert the Mar(data issue) to mar
test_set$month[test_set$month == "Nov"] <- "nov" #convert the Nov(data issue) to nov
test_set$month[test_set$month == "May"] <- "may" #convert the May(data issue) to may
test_set$duration[test_set$duration > 2000] <- NA #fix the outliers over 2500
test_set$campaign[test_set$campaign > 20] <- NA #fix the outliers over 20
test_set$housing[test_set$housing == "NA"] <- "unknown" #convert NAs to unkown value
test_set$loan[test_set$loan == "NA"] <- "unknown" #convert NAs to unkown value
test_set$cons_conf_idx[test_set$cons_conf_idx == 999] <- NA #fix the 999 outlier

test_set$pdays <- ifelse(test_set$pdays == 999, 0, 1) #convert 999 to 0 and everything else in 1

train_set <- na.omit(train_set)
test_set <- na.omit(test_set)


#############################################
#Descriptive statistics and associations
##############################################

#Summary Statistics for train_set
summary(train_set)
summary(test_set)
summary(train_set$subscribed)
summary(train_set$month)
describeBy(train_set$age, train_set$cons_price_idx)

##
#Correlations & accosiations
##

#correlations for continuous variables
#build a new frame with the continuous variables
continuous_var <- select(train_set, duration, campaign, pdays, previous,
                        emp_var_rate, cons_price_idx, cons_conf_idx, euribor3m,
                        nr_employed, age)

#correlation matrix
continuous_var.cor = cor(na.omit(continuous_var), method = "pearson")
corrplot(continuous_var.cor)

#different correlation matrix
#half correlation matrix
library(GGally)
ggcorr(na.omit(continuous_var), method = c("everything", "pearson"))



##correlations for categorical variables
chisq.test(train_set$job, train_set$subscribed)
chisq.test(train_set$marital_status, train_set$subscribed)
chisq.test(train_set$education, train_set$subscribed)
chisq.test(train_set$default, train_set$subscribed)
chisq.test(train_set$housing, train_set$subscribed)
chisq.test(train_set$loan, train_set$subscribed)
chisq.test(train_set$contact, train_set$subscribed)
chisq.test(train_set$month, train_set$subscribed)
chisq.test(train_set$day_of_week, train_set$subscribed)
chisq.test(train_set$poutcome, train_set$subscribed)



##
#Data Visualisation
##

#Subscribed distribution by campaign features
ggplot(train_set, mapping = aes(x =age, y = duration, size = campaign, color = subscribed)) +
  geom_point(alpha=.3) + #here we can adjust the shading
  scale_size(range = c(.5, 6), name="campaign") + #in this line we can adjust the size of the bubble
  labs(title = "Subscribed distribution by campaign features", x = "age", y = "duration")

#bar chart to see the euribor3m distribution and subcribed ratio
bar1 <- ggplot(train_set, aes(euribor3m))
bar1 + geom_bar(aes(fill= subscribed), width = 0.8) + 
  theme(axis.text.x = element_text(angle=80, vjust=1))

#bar chart to see the distribution of the duration of the contract by subcribed
bar2 <- ggplot(train_set, aes(duration))
bar2 + geom_bar(aes(fill= subscribed), width = 0.8) + 
  theme(axis.text.x = element_text(angle=80, vjust=1))

#bar chart to see which is the best month
ggplot(train_set, aes(month)) +
  geom_bar(colour="black", mapping = aes(fill = subscribed)) +
  labs(x="Months", y= "count", 
       title="Distribution of the subscription per month")

#bar chart to see how the previous outcome affect ours campaign
ggplot(train_set, aes(poutcome)) +
  geom_bar(colour="black", mapping = aes(fill = subscribed)) +
  labs(x="Previous Outcome", y= "count", 
       title="Distribution of the subscription by previous campaign outcome")

#bar chart to see how job role affect ours campaign
ggplot(train_set, aes(job)) +
  geom_bar(colour="black", mapping = aes(fill = subscribed)) +
  labs(x="Job Role", y= "count", 
       title="Distribution of the job role by subcription")

######################
#Logistic Regression
######################

levels(train_set$subscribed) #checking the possible answers in subscribed variable

#model1
model1 <- glm(formula = subscribed ~ job + marital_status + education + age + duration, 
              family = binomial(link="logit"), data = train_set)
summary(model1)

#model2
model2 <- glm(formula = subscribed ~ job + marital_status + education + age + duration + month + day_of_week + campaign + contact
              + pdays + previous + default, 
              family = binomial(link="logit"), data = train_set)
summary(model2)

#model3
model3 <- glm(formula = subscribed ~ job + marital_status + education + age + duration + month + day_of_week + campaign + contact
              + pdays + previous + default + poutcome + housing + loan, 
              family = binomial(link="logit"), data = train_set)
summary(model3)

#model4
model4 <- glm(formula = subscribed ~ ., family = binomial(link="logit"), data = train_set)
summary(model4)

#model5
model5 <- glm(formula = subscribed ~ job  + education + duration + month + day_of_week + campaign + contact
              + pdays + default + poutcome + emp_var_rate + cons_price_idx + euribor3m + cons_conf_idx + nr_employed,
              family = binomial(link="logit"), data = train_set)
summary(model5)

#model6
model6 <- glm(formula = subscribed ~  job  + education + duration + month  + campaign + contact
                   + pdays + default + poutcome + emp_var_rate + cons_price_idx + euribor3m,
                    family = binomial(link="logit"), data = train_set)
summary(model6)

###############
#Prediction
###############

pred <- predict(model5, test_set, type = "response") #making the prediction with our final model
pred

class_pred <- as.factor(ifelse(pred > 0.5, "yes", "no")) #set the probabilities >0.5 = yes and <0.5 = no

postResample(class_pred, test_set$subscribed) #checking the accuracy and kappa coef for our model

exp(model5$coefficients) #build the odds for the final model's variables

confusionMatrix(data = class_pred, test_set$subscribed) #confusion matrix and other statistics fro our model


#R^2 
logisticPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance 
  nullDev <- LogModel$null.deviance 
  modelN <- length(LogModel$fitted.values)
  R.l <-  1 -  dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2  ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2        ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2           ", round(R.n, 3),    "\n")
}

logisticPseudoR2s(model5)



head(pred)
class(pred)


#ROC
#transform the input data into a standardized format.
pr <- prediction(pred, test_set$subscribed)
#All kinds of predictor evaluations are performed using this function.
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
prf
plot(prf)

#AUC
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]   
auc

#Precision and Recall
precision_recall <- performance(pr, "prec", "rec")
plot(precision_recall)



##########################
#Checking the Assumption
##########################

#assumption 1
#linearity 
train_set$ageLogInt <- log(train_set$age)*train_set$age
train_set$durLogInt <- log(train_set$duration)*train_set$duration
train_set$campLogInt <- log(train_set$campaign)*train_set$campaign
train_set$pdaysLogInt <- log(train_set$pdays)*train_set$pdays
train_set$prevLogInt <- log(train_set$previous)*train_set$previous
train_set$empvarLogInt <- log(train_set$emp_var_rate)*train_set$emp_var_rate
train_set$consprLogInt <- log(train_set$cons_price_idx)*train_set$cons_price_idx
train_set$conscoLogInt <- log(train_set$cons_conf_idx)*train_set$cons_conf_idx
train_set$eurLogInt <- log(train_set$euribor3m)*train_set$euribor3m
train_set$nreLogInt <- log(train_set$nr_employed)*train_set$nr_employed

formula <- subscribed ~  nreLogInt + eurLogInt + consprLogInt + empvarLogInt + 
  campLogInt + durLogInt + ageLogInt +nr_employed + euribor3m + cons_price_idx + cons_conf_idx + emp_var_rate +
  previous + pdays + campaign + duration + age + job + marital_status + education + default + housing + contact + month + 
  day_of_week + poutcome

model <- glm(formula, family = "binomial", data = train_set)
summary(model)


#mydata <- train_set %>% select_if(is.numeric) 
#predictors <- colnames(mydata)
# Bind the logit and tidying the data for plot
#mydata <- mydata %>% mutate(logit = log(pred/(1-pred))) %>% gather(key = "predictors", value = "predictor.value", -logit)

#assumption 2
#infuential values
#cook's distance graph
plot(model5, which = 4, id.n = 3)

# Extract model results
model.data <- augment(model5) %>% mutate(index = 1:n()) 

#the data for the top 3 values as we can see in the cook's graph
model.data %>% top_n(3, .cooksd)

#plot the standardised Residuals
ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = subscribed), alpha = .5) +
  theme_bw()

#filter potential infuential data points
model.data %>% filter(abs(.std.resid) > 3)

#how many variables violate the cook's distance
cook <- cooks.distance(model5)
sum(cook > 1)

#residuals fit
x <- rstandard(model5)
sum(x > 1.96)


#assumption 3
#multicollinearity
vif(model5)
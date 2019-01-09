rm(list=ls(all=T))
setwd("C:/Users/Neelam/Desktop/Workradhika/project")
getwd()

#loading libararies
library(caret)
library(rpart)
library(C50)
library(rattle)
library(partykit)
library(randomForest)
library(ROCR)
library(ggplot2)
library(reshape2)
library(car)
library(corrplot)
library(e1071)
library(scales)
library(psych)
library(gplots)
library(corrgram)
library(DMwR)
library(unbalanced)
library(dummies)
library(Information)
library(MASS)
library(rpart)
library(gbm)
library(ROSE)
library(sampling)
library(DataCombine)
library(inTrees)

#reading the csv files
data =  read.csv("churndata.csv")

#changing variable into numeric form 
data$Churn =  as.integer(data$Churn)
data$international.plan = as.integer(data$international.plan)
data$voice.mail.plan =  as.integer(data$voice.mail.plan)

#changing itnto factorial
data$Churn[data$Churn=="1"] <-0
data$Churn[data$Churn=="2"] <-1
data$international.plan[data$international.plan=="1"] <-0
data$international.plan[data$international.plan=="2"] <-1
data$voice.mail.plan[data$voice.mail.plan=="1"] <-0
data$voice.mail.plan[data$voice.mail.plan=="2"] <-1


######################### missing value analysis#################################
na.omit(data)

summary(data)

#calculating standard deviation
sapply(data, sd)  
#Droping unread variables 
data$state = NULL
data$area.code = NULL
data$phone.number = NULL
data$number.customer.service.calls = NULL

#calculating correlation Matrix
cormtrix = round(cor(data) , digits = 2)


#histogram
ggplot(data , aes(x = data$total.day.minutes ))+
  geom_histogram(binwidth = 1 , fill = "white" ,  colour = "blue")+
  ggtitle("Histogram Analysis") +  theme(text=element_text(size=15))

ggplot(data , aes(x = data$total.day.charge ))+
  geom_histogram(binwidth = 1 , fill = "white" ,  colour = "blue")+
  ggtitle("Histogram Analysis") +  theme(text=element_text(size=15))

ggplot(data , aes(x = data$total.day.calls ))+
  geom_histogram(binwidth = 1 , fill = "white" ,  colour = "blue")+
  ggtitle("Histogram Analysis") +  theme(text=element_text(size=15))

ggplot(data , aes(x = data$total.eve.minutes ))+
  geom_histogram(binwidth = 1 , fill = "white" ,  colour = "purple")+
  ggtitle("Histogram Analysis") +  theme(text=element_text(size=15))

ggplot(data , aes(x = data$total.eve.calls ))+
  geom_histogram(binwidth = 1 , fill = "white" ,  colour = "purple")+
  ggtitle("Histogram Analysis") +  theme(text=element_text(size=15))

ggplot(data , aes(x = data$total.eve.charge ))+
  geom_histogram(binwidth = 1 , fill = "white" ,  colour = "purple")+
  ggtitle("Histogram Analysis") +  theme(text=element_text(size=15))

ggplot(data , aes(x = data$total.intl.minutes))+
  geom_histogram(binwidth = 1 , fill = "white" ,  colour = "navyblue")+
  ggtitle("Histogram Analysis") +  theme(text=element_text(size=15))

ggplot(data , aes(x = data$total.intl.charge))+
  geom_histogram(binwidth = 1 , fill = "white" ,  colour = "navyblue")+
  ggtitle("Histogram Analysis") +  theme(text=element_text(size=15))

ggplot(data , aes(x = data$total.intl.calls))+
  geom_histogram(binwidth = 1 , fill = "white" ,  colour = "navyblue")+
  ggtitle("Histogram Analysis") +  theme(text=element_text(size=15))


ggplot(data , aes(x = data$account.length))+
  geom_histogram(binwidth = 1 , fill = "white" ,  colour = "orange")+
  ggtitle("Histogram Analysis") +  theme(text=element_text(size=15))

ggplot(data , aes(x = data$voice.mail.plan))+
  geom_histogram(binwidth = 1 , fill = "white" ,  colour = "orange")+
  ggtitle("Histogram Analysis") +  theme(text=element_text(size=15))

############################################Outlier Analysis#############################################
# ## BoxPlots - Distribution and Outlier Check
numeric_index = sapply(data,is.numeric) #selecting only numeric

numeric_data = data[,numeric_index]

cnames = colnames(numeric_data)
 
 for (i in 1:length(cnames))
 {
   assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "Churn"), data = subset(data))+ 
            stat_boxplot(geom = "errorbar", width = 0.5) +
            geom_boxplot(outlier.colour="red", fill = "blue" ,outlier.shape=18,
                         outlier.size=0.5, notch=FALSE) +
            theme(legend.position="bottom")+
            labs(y=cnames[i],x="Churn")+
            ggtitle(paste("Boxplot churn for",cnames[i])))
 }
 
# ## Plotting plots together
gridExtra::grid.arrange(gn1,gn2, gn4, ncol=3 )
gridExtra::grid.arrange(gn5, gn6,gn7,ncol=3)
gridExtra::grid.arrange(gn8,gn9,gn10 ,ncol=3)
gridExtra::grid.arrange(gn11,gn12, gn13 ,ncol=3)
gridExtra::grid.arrange(gn14,gn15,gn16 , ncol=3)

#loop to remove from all variables
for(i in cnames){
  print(i)
  val = data[,i][data[,i] %in% boxplot.stats(data[,i])$out]
  print(length(val))
  data = data[which(!train[,i] %in% val),]
}
 

 
gridExtra::grid.arrange(gn1,gn2, gn4, ncol=3 )
gridExtra::grid.arrange(gn5, gn6,gn7,ncol=3)
gridExtra::grid.arrange(gn8,gn9,gn10 ,ncol=3)
gridExtra::grid.arrange(gn11,gn12, gn13 ,ncol=3)
gridExtra::grid.arrange(gn14,gn15,gn16 , ncol=3)
#############oversampling#######
# ##Simple Random Sampling
data_sample = data[sample(nrow(data), 5000, replace = F), ]
# 
# ##Stratified Sampling
# stratas = strata(data, c("profession"), size = c(100, 199, 10, 5), method = "srswor")
# stratified_data = getdata(marketing_train, stratas)
# 
# ##Systematic sampling
# #Function to generate Kth index
sys.sample = function(N,n){
  k = ceiling(N/n)
  r = sample(1:k, 1)
  sys.samp = seq(r, r + k*(n-1), k)
}
# 
lis = sys.sample(5000, 1000) #select the repective rows
# 
# #Create index variable in the data
 data$index = 1:5000
# 
# #Extract subset from whole data
 systematic_data = data[which(data$index %in% lis),]
table(systematic_data$Churn)


#Clean the environment
rmExcept("data" , "data_sample")


#Divide data into train and test using stratified sampling method
#factor_index = sapply(data_sample,is.factor)
set.seed(1234)
index = sample(2 , nrow(data) , replace = TRUE , prob = c(0.7 , 0.3))
train = data[ index==1,]
test  = data[index==2,]

###############Model-1 LOGISTIC REGRESSION MODEL#################
#Logistic Regression
logit_model = glm(Churn ~ ., data = train, family = "binomial")

#summary of the model
summary(logit_model)

#predict using logistic regression
logit_Predictions = predict(logit_model, newdata = test, type = "response")

#convert prob
logit_Predictions = ifelse(logit_Predictions > 0.5, 1, 0)

##Evaluate the performance of classification model
ConfMatrix_RF = table(test$Churn, logit_Predictions)

#accuuracy 
#accuracy : TP+TN/N
#False Negative rate
#FNR = FN/FN+TP 
#Accuracy: 86.23
#FNR:  87.32

######################Model-2 Random Forest#######################################

RF_model = randomForest(Churn ~ . ,data= train, importance = TRUE, ntree = 500 , ntry = 500)
print(RF_model)
importance(RF_model)
summary(RF_model)
#Extract rules fromn random forest
#transform rf object to an inTrees' format
treeList = RF2List(RF_model)  

#Extract rules
exec = extractRules(treeList, train[,-17])  # R-executable conditions
write(capture.output(summary(RF_model)), "Rules.txt")
#Visualize some rules
exec[1:10,]

#Make rules more readable:
readableRules = presentRules(exec, colnames(train))
readableRules[1:10,]

# #Get rule metrics
ruleMetric = getRuleMetric(exec, train[,-17], train$Churn)  # get rule metrics

#evaulate few rules
ruleMetric[1:10,]

#Presdict test data using random forest model
#RF_Predictions1 = predict(RF_model, test[,-17])
RF_Predictions = predict(RF_model, newdata = test, type = "response")
##Evaluate the performance of classification model
ConfMatrix_RF = table(test$Churn , RF_Predictions)
confusionMatrix(ConfMatrix_RF)

#False Negative rate
#FNR = FN/FN+TP 

#Accuracy = 96.28
#FNR = 26.33
#will give the visulization reason on %incMSE
plot.new()
varImpPlot(RF_model , type = 1, pch = 19 , col = 1, cex=1.0, main = " " )
abline(v=45, col = "purple")

#Another type of reason to churn which is incNodePurity
plot.new()
varImpPlot(RF_model , type = 2, pch = 19 , col = 1, cex=1.0, main = " " )
abline(v=45, col = "blue")

##################################plotting ROc curve##################
test$result1 = predict(logit_model, newdata = test, type = "response")
test$result2 = predict(RF_model, newdata = test, type = "response")

pred1 = prediction(test$result1 , test$Churn)
pred2 = prediction(test$result2 , test$Churn)


pref1 = performance(pred1 , "tpr" , "fpr")
pref2 = performance(pred2 , "tpr" , "fpr")

plot.new()
plot(pref1 , col = "purple" , lwd = 2.5)
plot(pref2, col = "red" , add = TRUE , lwd = 2.5)
abline(0,1, col = "green" , lwd = 2.5, lty= 2)

title('Curve')
legend(0.8,0.4,c("Logistic" , "RF"),
       lty=c(1,1),
       lwd=c(1.4,1.4), col = c("purple" ,"red" , "blue"))

#######saving model to file##########
#Accuracy: 86.23
#FNR:  87.32

#Random forest
#Accuracy = 96.28
#FNR = 26.33


save(RF_model , file = "churnmodel.rda")


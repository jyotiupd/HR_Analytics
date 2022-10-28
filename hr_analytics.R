
## Reading the data into a dataframe
data_set<-read.csv('data/HR_comma_sep.csv')

# Check to see if there are any missing values in our data and checking overall summary
summary(data_set)


##Exploratory Analysis:- 

dim(data_set)  # The dataset contains 10 columns and 14999 observations

# Check the type of our features. 
str(data_set)



attrition<-as.factor(data_set$left)
summary(attrition) # Looks like about 76% of employees stayed and 24% of employees left. 

perc_attrition_rate<-sum(data_set$left/length(data_set$left))*100
#percentage of attrition
print(perc_attrition_rate)  ##attrition rate :- 23.80825

# Overview of summary (Turnover V.S. Non-turnover)
cor_vars<-data_set[,c("satisfaction_level","last_evaluation","number_project","average_montly_hours","time_spend_company","Work_accident","left","promotion_last_5years")]

aggregate(cor_vars[,c("satisfaction_level","last_evaluation","number_project","average_montly_hours","time_spend_company","Work_accident","promotion_last_5years")], by=list(Category=cor_vars$left), FUN=mean)

cor(cor_vars)

#Correlation Matrix

#install.packages('reshape2')
#install.packages('ggplot2')
library(reshape2)
library(ggplot2)

trans<-cor(cor_vars)
melted_cormat <- melt(trans)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +theme(axis.text.x = element_text(angle = 90, hjust = 1)) ## Satisfaction level has quite strong negative correlation with left

##Salary vs Turnover
vis_1<-table(data_set$salary,data_set$left)
#print(vis_1)
d_vis_1<-as.data.frame(vis_1)
p<-ggplot(d_vis_1, aes(x=Var1,y=Freq,fill=Var2)) +
  geom_bar(position="dodge",stat='identity')+
  labs(x = "Salary",y ="Frequency")
p  ##Most of employees who left were from low and medium salary range.


##Dept vs Turnover
vis_1<-table(data_set$Department,data_set$left)
#print(vis_1)
d_vis_1<-as.data.frame(vis_1)
library(ggplot2)
p<-ggplot(d_vis_1, aes(x=Var1,y=Freq,fill=Var2)) +
  geom_bar(position="dodge",stat='identity')+labs(x = "Department",y ="Frequency")
p  ## Proportion of employess leaving accross department seems to be same

##Turnover vs projectcount
vis_1<-table(data_set$number_project,data_set$left)
#print(vis_1)
d_vis_1<-as.data.frame(vis_1)
p<-ggplot(d_vis_1, aes(x=Var1,y=Freq,fill=Var2)) +
  geom_bar(position="dodge",stat='identity')+labs(x = "ProjectCount",y ="Frequency")
p  ## More than half of the employees with 2,6, and 7 projects left the company
#Majority of the employees who did not leave the company had 3,4, and 5 projects
#All of the employees with 7 projects left the company

## number_project vs satisfaction_level
vis_1<-table(data_set$number_project,data_set$satisfaction_level<0.1)
#print(vis_1)
d_vis_1<-as.data.frame(vis_1)
p<-ggplot(d_vis_1, aes(x=Var1,y=Freq,fill=Var2)) +
  geom_bar(position="dodge",stat='identity')+labs(x = "ProjectCount",y ="FrequencyOFSatisfactionLevel<0.1",fill="SatisfactionLevel<0.1")
p ##It can bee seen that proportion of employees with lesser satisfaction level is more in Project_number 6 (13%) and 7 (23%)

#KDEPlot: Kernel Density Estimate Plot


left_data<-subset(data_set,left==1)
stay_data<-subset(data_set,left==0)

#Turnover V.S. AverageMonthlyHours
ggplot() + geom_density(aes(x=average_montly_hours), colour="red", data=left_data) + 
  geom_density(aes(x=average_montly_hours), colour="blue", data=stay_data)#Employees who had less hours of work (~150hours or less) left the company more
#Employees who had too many hours of work (~250 or more) left the company

#Turnover V.S. Evaluation
ggplot() + geom_density(aes(x=last_evaluation), colour="red", data=left_data) + 
  geom_density(aes(x=last_evaluation), colour="blue", data=stay_data)

#Employees with low performance tend to leave the company more
#Employees with high performance tend to leave the company more

ggplot() + geom_density(aes(x=last_evaluation), colour="red", data=left_data) + 
  geom_density(aes(x=last_evaluation), colour="blue", data=stay_data)


#ProjectCount VS AverageMonthlyHours [BOXPLOT]
#Looks like the average employees who stayed worked about 200hours/month. Those that had a turnover worked about 250hours/month or 150hours/month

p<-ggplot(data_set, aes(x = factor(number_project), y = average_montly_hours, fill = factor(left))) +
  geom_boxplot() + scale_fill_manual(values = c("yellow", "orange"))
print(p)

#ProjectCount VS Evaluation
#Looks like employees who did not leave the company had an average evaluation of around 70% even with different projectCounts
#There is a huge skew in employees who had a turnover though. It drastically changes after 3 projectCounts. 
#Employees that had two projects and a horrible evaluation left. Employees with more than 3 projects and super high evaluations left
p<-ggplot(data_set, aes(x = factor(number_project), y = last_evaluation, fill = factor(left))) +
  geom_boxplot() + scale_fill_manual(values = c("yellow", "orange"))
print(p)


# Cluster 1 (Hard-working and Sad Employee): Satisfaction was below 0.2 and evaluations were greater than 0.75. Which could be a good indication that employees who left the company were good workers but felt horrible at their job.
# Cluster 2 (Bad and Sad Employee): Satisfaction between about 0.35~0.45 and evaluations below ~0.58. This could be seen as employees who were badly evaluated and felt bad at work.
# Cluster 3 (Hard-working and Happy Employee): Satisfaction between 0.7~1.0 and evaluations were greater than 0.8. Which could mean that employees in this cluster were "ideal". They loved their work and were evaluated highly for their performance.



library(ggplot2)
ggplot(data_set, aes(satisfaction_level, last_evaluation, color = left)) +
  geom_point(shape = 16, size = 5, show.legend = FALSE) +
  theme_minimal() +
  scale_color_gradient(low = "#0091ff", high = "#f0650e")


##Feature selection

##Since Department and salary are in chr , we need to have dummy columnn in place of them

createDummy=function(x,z)##function that excepts 2 parameters:-a data set and the categorical variable
{
  
  for(i in unique(z))#Iterate through the unique values of categorical variable
  {
    ##print(i)
    x[paste("dummy",i,sep ="_")]<-ifelse(z == i,1,0)##creating the dummy columns for each unique value
    
  }
  
  return(x)
}

unique(data_set$Department)

data_set_2=createDummy(data_set,data_set$Department)

data_set_2=createDummy(data_set_2,data_set_2$salary)

data_set_2 = subset(data_set_2, select = -c(dummy_high,dummy_RandD,Department,salary) )


set.seed(2)  #(Setting the seed so that the random sampling is reproducible)

s=sample(1:nrow(data_set_2),0.7*nrow(data_set_2)) #(breaking data into 70% and 30%)

train=data_set_2[s,] #Contaions 70% data
test=data_set_2[-s,] #contains other 30% data

train_lm=train #Assigning train data to train_lm
test_lm=test  #Assigning test data to test_lm


table(train_lm$left)


#2Remove predictor variables with VIF>5 from the train data. [10]

fit=lm(left~.,data=train_lm) #Using linear regressions to identify which VIFs

#install.packages('car')
library(car) # loading the car library which consist of vif function

vif(fit) #no variables found with vif value greater than 5


##Building a logistic regression model

fit=glm(left~.,family=binomial,data=train) 

train$score=predict(fit,newdata=train,type="response")

#predicting the probability score on test data

test$score=predict(fit,newdata=test,type="response") 

##Finding the threshold value for predicting churn

cutoff_data=data.frame(cutoff=0, KS=99) ##Creating the dummy values (to be removed later)
cutoffs=seq(0,1,length=1000)
for (cutoff in cutoffs){
  predicted=as.numeric(train$score>cutoff)
  TP=sum(predicted==1 & train$left==1)
  FP=sum(predicted==1 & train$left==0)
  FN=sum(predicted==0 & train$left==1)
  TN=sum(predicted==0 & train$left==0)
  P=TP+FN
  N=TN+FP
  Sn=TP/P
  KS=Sn - (FP/N)
  cutoff_data=rbind(cutoff_data,c(cutoff,KS))
}
#remove the dummy data cotaining top row
cutoff_data=cutoff_data[-1,]


cutoff_KS=cutoff_data$cutoff[which.max(cutoff_data$KS)][1] ##getting KS cut off

table(test$left,as.numeric(test$score>cutoff_KS))

#   0    1
#0 2726  710
#1  320  744

#Accuracy:- (2726+744)/(2726+744+710+320)-> 
# 0.7711111
#Misclassification Rate:- (710+320)/(2726+744+710+320) ->
#0.2288889


save.image(file="HR_Analytics.RData")







rm(list=ls())
library(readxl)
data = read_excel("HRanalytics.xlsx")
#let us see the structure of Data
str(data)
colnames(data)=tolower(make.names(colnames(data)))    #convert to lowercase variable name
attach(data)

#let us see how the dependent varibales are distributed
hist(perfscoreid)  #normally distributed
hist((empsatisfaction))
hist(log(absences))   #log transform of absences is exponential  



#Checking for missing values
colSums(is.na(data)) 


#Feature Engineering.

d = data[,-c(1,2,3,4,5,6,7,9,12,28)]
str(d)

#Factor conversion
d$termd = as.factor(d$termd)
d$maritaldesc = as.factor(d$maritaldesc)
d$maritaldesc = relevel(d$maritaldesc, "Single")
d$citizendesc = as.factor(d$citizendesc)
d$termreason = as.factor(d$termreason)
d$managername = as.factor(d$managername)
d$recruitmentsource = as.factor(d$recruitmentsource)
d$recruitmentsource = relevel(d$recruitmentsource, "Indeed")
d$position = as.factor(d$position)
d$position = relevel(d$position, "Accountant I")
d$hispaniclatino = as.factor(d$hispaniclatino)
d$sex = as.factor(d$sex)
d$sex = relevel(d$sex, "M")
d$hispaniclatino = relevel(d$hispaniclatino, "No")
d$racedesc = as.factor(d$racedesc)
d$racedesc = relevel(d$racedesc, "White")
d$employmentstatus = as.factor(d$employmentstatus)
d$department = as.factor(d$department)
d$department = relevel(d$department, "IT/IS")
d$performancescore = as.factor(d$performancescore)
d$empsatisfaction = as.factor(d$empsatisfaction)

str(d)






#descriptive analysis

table(employmentstatus) #among the 104 employees who are terminated, 16 are for a cause and 88 are voluntary 
table(empsatisfaction) #many employees are highly satisfied with 300 emoloyees having score >=3
table(perfscoreid) #many of them had performance id = 3 which is "fullymeets"
aggregate(absences~department, data = d, mean) #most number of absences are in sales department 


hist(salary)
table(recruitmentsource)
table(specialprojectscount)
#241 employees are not worked on any special projects


#Correlation Analysis
library(PerformanceAnalytics)
numeric_cols = d[,c(2,26,25,23,21)]
chart.Correlation(numeric_cols)
#no correlation

#Models
#Q1
library("nnet")
#Performance score
#we can see that performancescoreID is an ordered multicalss 
# let us Convert PerformanceScoreID to an ordered factor
d$performancescore = factor(d$performancescore, levels = c("PIP","Needs improvement","Fully meets","Exceeds"), ordered = TRUE)

pm1 <- polr(performancescore ~ empsatisfaction + managername + department +
                dayslatelast30 + recruitmentsource + position , data=d, Hess = TRUE)
summary(pm1)
 #let us run the tobit model with bounding data to analyze what factors affecting performance score
pm_t = tobit(perfscoreid ~ positionid+ empsatisfaction + department + managername  + recruitmentsource+dayslatelast30, left=1, right=4, data=d)
summary(pm_t)

#similarly for employement satisfaction
d$empsatisfaction = as.numeric(d$empsatisfaction)
es_t = tobit(empsatisfaction ~termd+ positionid + department + managername +dayslatelast30+absences+specialprojectscount+recruitmentsource, left=1, right=5, data=d)
summary(es_t)

#poisson model for absents
abs_1 <- glm(absences ~ department+managername+empsatisfaction+dayslatelast30+engagementsurvey+salary, data = d,family = poisson(link = "log"))
summary(abs_1)
library(AER)
dispersiontest(abs_1) #there is dispersion
#let us use negative binomial
abs_nb <- glm.nb(absences ~ department+managername+empsatisfaction+dayslatelast30+engagementsurvey+salary, data = d)
summary(abs_nb)
stargazer(pm_t,es_t, abs_nb, type="text")


#q4
# we need to create a new variable for no of days in the company for all employees including who are terminated and not terminated
#if terminated days = joined - terminated, if not terminated, days = joined - llast performancereview date
library(lubridate)
d$days_in_org = ifelse(d$termd == 1,as.numeric(difftime(ymd(d$dateoftermination), ymd(d$dateofhire),units = "days")),as.numeric(difftime(ymd(d$lastperformancereview_date), ymd(d$dateofhire), units = "days")))


library(survival)

# survival model for department
attach(d)
km_term <- survfit(Surv(days_in_org, termd)~department, data = d)
summary(km_term,1825)
levels(d$department)

#q5
hist(log(salary))

library(AER)
#to answer the question asked, let us run a model with interaction term
q5_p <- glm(salary ~ department*(racedesc + sex) +performancescore+empsatisfaction,data = d,family = poisson(link = "log"))
summary(q5_p)
library(AER)
dispersiontest(pois) 
#there is huge dispersion
#let us use negative binomial 


q5_nb = glm.nb(salary ~ department*(racedesc + sex) +performancescore+empsatisfaction, data = d)
summary(q5_nb)
library('car')
vif(nb)


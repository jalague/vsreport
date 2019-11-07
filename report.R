library('dplyr')
library("tidyverse")
library("readxl")
library('ISLR')
library('mlbench')
library('corrplot')
library(randomForest)


#Reading CSV/Excel files and creating data frames in R
jobs<-read_excel("Jobs.xlsx")
jobs_daily<-read_excel("Jobs_Daily.xlsx")
jobs_daily_status<-read_excel("Jobs_Daily_Status.xlsx")
labor<-read_excel("labor_Report.xlsx")

labor$Department[is.na(labor$Department)]<-"Department not listed"
labor$Job[is.na(labor$Job)]<-"Job not listed"
labor$Project[is.na(labor$Project)]<-"Project not listed"
labor$Division[is.na(labor$Division)]<-"Division not listed"
labor$Time<-format(as.POSIXct(labor$`Timecard Date`,format='%m/%d/%Y %H:%M'),format='%m/%d/%Y')
labor<-labor[,-6]

#Merging tables (Natural Joins in SQL) to have more features for analysis----------

job_daily_merge<-merge(jobs,jobs_daily, by.x = "UUID" ,by.y="Job_Id")
#Merging the first joined table with Jobs_daily_status
job_daily_status_merge<-merge(job_daily_merge,jobs_daily_status, by.x="UUID.y", by.y = "Job_Daily_Id")
#formating the date column
job_daily_status_merge$Date<- format(as.POSIXct(job_daily_status_merge$Date,format='%m/%d/%Y %H:%M'),format='%m/%d/%Y')

#Grouping hours worked by employee and Job (for javascript visualization)
grouper<- labor %>% group_by(Hours) %>% summarise(sum=sum(Hours))
employeehours<- labor %>% group_by(Employee,Job) %>% summarise(median=median(Hours))

employetasks<-labor %>% group_by(Employee,Job) %>% summarise(sum=sum(Hours))
employetasks$Job[is.na(employetasks$Job)]<-"Job not listed"
str_replace_all(employetasks$Job, " ", "")

for (m in 1:length(employetasks$Employee)){
  employetasks$id[m]<-paste("root",employetasks$Employee[m],str_replace_all(employetasks$Job[m]," ",""))
}

second<-data.frame(unique(employetasks$Employee))

for (l in 1:length(second$unique.employetasks.Employee.)){
  second$Employee[l]<-second$unique.employetasks.Employee.[l]
    second$Job[l]<-second$unique.employetasks.Employee.[l]
    second$sum[l]<-second$unique.employetasks.Employee.[l]
    second$id[l]<-paste("root",second$unique.employetasks.Employee.[l])
}

second$Employee<-second$unique.employetasks.Employee.
second$Job<-second$unique.employetasks.Employee.
second$sum<-second$unique.employetasks.Employee.

second<-second[,-1]
Employeetaskfinal<-rbind(data.frame(employetasks),second)
#creating CSV files to be used in my visualizations
#write.csv(Employeetaskfinal,"employeetaskhours.csv")
#write.csv(employeehours,"employeehoursMedian.csv")

sum(labor$Hours)
count(which(labor$Employee != null))

for (i in 1:length(job_daily_status_merge$Delay_Cause)){
  if(job_daily_status_merge$Delay_Cause[i]=="None")
  {
    job_daily_status_merge$Delay_Cause_num[i]<-0
  } else if(job_daily_status_merge$Delay_Cause[i]=="Owner"){
    job_daily_status_merge$Delay_Cause_num[i]<-1
  } else {  job_daily_status_merge$Delay_Cause_num[i]<-2}
}

job_daily_status_merge_groupdaily<- job_daily_status_merge %>% group_by(UUID.y)
job_daily_status_merge_groupdaily %>% summarise(count=count(Delay_Cause_num))

#A final merge that joins all the tables together (joining Labor Report to  the existing joined tables)
jdsl_merge<-merge(job_daily_status_merge, labor, by.x="Date",by.y = 'Time')



#Exploring correlations and creating models------------------------

cor(job_daily_status_merge$`Wood_Status%`, y=factor(job_daily_status_merge$Are_There_Equipment_Requests))

# Total_Work_Time+ Are_There_Noteworthy_Performances + Delay_Cause + Employee_Count + Foreman_Name + Is_Client_Feedback_Present + Materials_Requested + `Wood_Status%`, `Rough_Status%`, `Steel_Status%`, `Finish_Status%`, `Pads_Status%`, 

glm234<-glm(factor(Are_There_Equipment_Requests)~ Total_Work_Time+ Are_There_Noteworthy_Performances + 
              Delay_Cause + Employee_Count + Foreman_Name + Is_Client_Feedback_Present + 
              Materials_Requested + `Wood_Status%`+ `Rough_Status%`+ `Steel_Status%`+
              `Finish_Status%`+ `Pads_Status%`,
            data=job_daily_status_merge, family=binomial)

glm234<-glm(factor(Are_There_Equipment_Requests)~ Total_Work_Time+ Are_There_Noteworthy_Performances + 
              Delay_Cause + Employee_Count  + Is_Client_Feedback_Present + 
              Materials_Requested + `Rough_Status%`+ `Steel_Status%`,
            data=job_daily_status_merge, family=binomial)

glm234<-glm(factor(Are_There_Equipment_Requests)~ Are_There_Noteworthy_Performances + 
              Delay_Cause + Employee_Count  + Is_Client_Feedback_Present
            + `Rough_Status%`+ `Steel_Status%`,
            data=job_daily_status_merge, family=binomial)

glm234.fit<-glm(factor(Materials_Requested)~ Total_Work_Time+ Are_There_Noteworthy_Performances + 
              Delay_Cause + Employee_Count + Foreman_Name + Is_Client_Feedback_Present + 
              Are_There_Equipment_Requests + `Wood_Status%`+ `Rough_Status%`+ `Steel_Status%`+
              `Finish_Status%`+ `Pads_Status%`,
            data=job_daily_status_merge, family=binomial)

size<-floor(.7*length(jdsl_merge$`Job Name`))
train<-sample(1:length(jdsl_merge$`Job Name`),size)

traindata<-data.frame(factor(jdsl_merge$Materials_Requested[train]),jdsl_merge$Hours[train]
                      ,factor(jdsl_merge$Job[train])
                      ,factor(jdsl_merge$Division[train])
                      ,factor(jdsl_merge$Department[train])
                      ,factor(jdsl_merge$`Employee Name`[train])
                      ,factor(jdsl_merge$Are_There_Equipment_Requests[train])
                      ,factor(jdsl_merge$Are_There_Noteworthy_Performances[train])
                      ,jdsl_merge$Employee_Count[train]
                      ,jdsl_merge$`Wood_Status%`[train]
                      , jdsl_merge$`Rough_Status%`[train]
                      ,jdsl_merge$`Steel_Status%`[train]
                      ,jdsl_merge$`Finish_Status%`[train]
                      ,jdsl_merge$`Pads_Status%`[train]
                      ,factor(jdsl_merge$Walls_Complete_Status)[train])

colnames(traindata)<-c("Mat","Hours","Job","Division","Department","Employee_Name","Are_There_Equipment_Requests","Are_There_Noteworthy_Performances","Employee_Count","Wood_Status","Rough_Status","Steel_Status","Finish_Status","Pads_Status","Walls_Complete_Status")

testdata<-data.frame(jdsl_merge$Materials_Requested[-train],jdsl_merge$Hours[-train]
                      ,factor(jdsl_merge$Job[-train])
                      ,factor(jdsl_merge$Division[-train])
                      ,factor(jdsl_merge$Department[-train])
                      ,factor(jdsl_merge$`Employee Name`[-train])
                      ,factor(jdsl_merge$Are_There_Equipment_Requests[-train])
                      ,factor(jdsl_merge$Are_There_Noteworthy_Performances[-train])
                      ,jdsl_merge$Employee_Count[-train]
                      ,jdsl_merge$`Wood_Status%`[-train]
                      , jdsl_merge$`Rough_Status%`[-train]
                      ,jdsl_merge$`Steel_Status%`[-train]
                      ,jdsl_merge$`Finish_Status%`[-train]
                      ,jdsl_merge$`Pads_Status%`[-train]
                      ,factor(jdsl_merge$Walls_Complete_Status)[-train])

colnames(testdata)<-c("Mat","Hours","Job","Division","Department","Employee_Name","Are_There_Equipment_Requests","Are_There_Noteworthy_Performances","Employee_Count","Wood_Status","Rough_Status","Steel_Status","Finish_Status","Pads_Status","Walls_Complete_Status")
testdatay<-factor(jdsl_merge$Materials_Requested[-train])
testdata <- rbind(traindata[1, ] , testdata)
testdata <- testdata[-1,]

materialsmodel<-randomForest(Mat~Hours
                   +Job
                   +Division
                   +Department
                   +Employee_Name
                   +Are_There_Equipment_Requests
                   +Are_There_Noteworthy_Performances
                   +Employee_Count
                   +Wood_Status
                   +Rough_Status
                   +Steel_Status
                   +Finish_Status
                   +Pads_Status
                   +Walls_Complete_Status
                   , data=traindata, importance=T)

materialsmodel$importance
materialsmodel$confusion
preds<-predict(materialsmodel,newdata=testdata)
table(preds,testdata[,1])
length(which(materialsmodel$pred==testdatay))
(89+154)/(1452+2356)

#93% classified correctly
varImpPlot(materialsmodel)

performancemodel<-randomForest(factor(jdsl_merge$Are_There_Noteworthy_Performances)~jdsl_merge$Hours
                             +factor(jdsl_merge$Job)
                             +factor(jdsl_merge$Division)
                             +factor(jdsl_merge$Department)
                             +factor(jdsl_merge$`Employee Name`)
                             +factor(jdsl_merge$Are_There_Equipment_Requests)
                             +factor(jdsl_merge$Materials_Requested)
                             +jdsl_merge$Employee_Count
                             +jdsl_merge$`Wood_Status%`
                             + jdsl_merge$`Rough_Status%`
                             +jdsl_merge$`Steel_Status%`
                             +jdsl_merge$`Finish_Status%`
                             +jdsl_merge$`Pads_Status%`
                             +factor(jdsl_merge$Walls_Complete_Status)
                             ,data=jdsl_merge, importance=T)                  

performancemodel$confusion
(121+112)/(5055+8215)
#96% accuracy for the training set. This does not mean it will be the same for the test set
varImpPlot(performancemodel)

#Linear Regression - modeling/ predicitng hours worked by an employee


#splitting date value into month and day values
labor$day<-format(as.POSIXct(labor$Time,format='%m/%d/%Y'),format='%d')
labor$month<-format(as.POSIXct(labor$Time,format='%m/%d/%Y'),format='%m')



model<-lm(Hours~., data=jdsl_merge)
summary(model)

plot(as.factor(labor$Job),labor$Hours)
abline(model)

View(jdsl_merge)
jdsl_merge$Job[is.na(jdsl_merge$Job)]<-"Job not listed"
jdsl_merge$Project[is.na(jdsl_merge$Project)]<-"Job not listed"
jdsl_merge$day<-format(as.POSIXct(labor$Time,format='%m/%d/%Y'),format='%d')
jdsl_merge$month<-format(as.POSIXct(labor$Time,format='%m/%d/%Y'),format='%m')

linearbasic<-lm(Hours~jdsl_merge$Steel_SqFt_Remaining, data=jdsl_merge)
plot(jdsl_merge$Steel_SqFt_Remaining,jdsl_merge$Hours, xlab="Steel Sqft remaining", ylab="Hours")
abline(linearbasic, col="red")

linearregres<-lm(Hours~factor(jdsl_merge$Job)+factor(jdsl_merge$Division)+factor(jdsl_merge$Department)+factor(jdsl_merge$`Employee Name`)+
                     factor(jdsl_merge$Are_There_Equipment_Requests)+factor(jdsl_merge$Are_There_Noteworthy_Performances)+jdsl_merge$Employee_Count+jdsl_merge$`Wood_Status%`
                   + jdsl_merge$`Rough_Status%`+jdsl_merge$`Steel_Status%`+jdsl_merge$`Finish_Status%`+jdsl_merge$`Pads_Status%`+factor(jdsl_merge$Walls_Complete_Status)+factor(jdsl_merge$Delay_Cause_num)+factor(jdsl_merge$Department), data=jdsl_merge)

compare1<-data.frame(jdsl_merge$Hours,linearregres$fitted.values)
colnames(compare1)<-c("Real Hours","Predicted")
head(compare1)
summary(linearregres)

randomfor<-randomForest(Hours~factor(jdsl_merge$Job)+factor(jdsl_merge$Division)+factor(jdsl_merge$Department)+factor(jdsl_merge$`Employee Name`)+
                     factor(jdsl_merge$Are_There_Equipment_Requests)+factor(jdsl_merge$Are_There_Noteworthy_Performances)+jdsl_merge$Employee_Count+jdsl_merge$`Wood_Status%`
                   + jdsl_merge$`Rough_Status%`+jdsl_merge$`Steel_Status%`+jdsl_merge$`Finish_Status%`+jdsl_merge$`Pads_Status%`+factor(jdsl_merge$Walls_Complete_Status)+factor(jdsl_merge$Delay_Cause_num)+factor(jdsl_merge$Department), data=jdsl_merge, importance=T)

(summary(rfer))

partialPlot(rfer, pred.data=jdsl_merge,x.var = Hours)
varImpPlot(rfer)

rfer$importance
rfer$predicted
compare<-data.frame(jdsl_merge$Hours,rfer$predicted)
colnames(compare)<-c("Real Hours","Predicted")
head(compare)

#Looking at total sqft and time to complete wall---------------------------------------

#compeltion time for wood
woodstarts<-which(job_daily_status_merge$`Wood_Status%`<10)
woodends<-which(job_daily_status_merge$`Wood_Status%`>=99)
wstart<-data.frame(job_daily_status_merge$`Wood_Status%`[woodstarts],job_daily_status_merge$Date[woodstarts],paste(job_daily_status_merge$Wall_Section_Name[woodstarts],job_daily_status_merge$`Job Name`[woodstarts]),woodstarts)
colnames(wstart)<-c("Status","Date","id","index")
wstart<-wstart[order(wstart$Date,decreasing = T),]
wstartindex<-wstart[match(unique(wstart$id), wstart$id),]

wend<-data.frame(job_daily_status_merge$`Wood_Status%`[woodends],job_daily_status_merge$Date[woodends],paste(job_daily_status_merge$Wall_Section_Name[woodends],job_daily_status_merge$`Job Name`[woodends]),woodends)
colnames(wend)<-c("Status","EndDate","id","index")
wend<-wend[order(wend$EndDate),]
wendindex<-wend[match(unique(wend$id), wend$id),]
merge(wstartindex, wendindex,by.x="id", by.y="id")

#time for steel
steelstarts<-which(job_daily_status_merge$`Steel_Status%`<10)
steelends<-which(job_daily_status_merge$`Steel_Status%`>=99)
sstart<-data.frame(job_daily_status_merge$`Steel_Status%`[steelstarts],job_daily_status_merge$Date[steelstarts],paste(job_daily_status_merge$Wall_Section_Name[steelstarts],job_daily_status_merge$`Job Name`[steelstarts]),steelstarts)
colnames(sstart)<-c("Status","Date","id","index")
sstart<-sstart[order(sstart$Date,decreasing = T),]
sstartindex<-sstart[match(unique(sstart$id), sstart$id),]

send<-data.frame(job_daily_status_merge$`Steel_Status%`[steelends],job_daily_status_merge$Date[steelends],paste(job_daily_status_merge$Wall_Section_Name[steelends],job_daily_status_merge$`Job Name`[steelends]),steelends)
colnames(send)<-c("Status","EndDate","id","index")
send<-send[order(send$EndDate),]
sendindex<-wend[match(unique(send$id), send$id),]
merge(sstartindex, sendindex,by.x="id", by.y="id")

#.....
#values exported as a table "data2.csv"

#Create test data frame
##Load the necesary files
activity_names<-read.table("UCI HAR Dataset/activity_labels.txt", header = F)
measurements_names<-read.table("UCI HAR Dataset//features.txt",header = F)
test_subject<-read.table("UCI HAR Dataset/test/subject_test.txt",header = F)
test_activity<-read.table("UCI HAR Dataset/test/y_test.txt",header = F)
test_measurements<-read.table("UCI HAR Dataset//test//X_test.txt",header = F)
##Get the variables in a data frame
##Q3:Use descriptive activity names; Q4: Label data with descriptive variable names
names(test_measurements)<-measurements_names$V2
test_activity$activity<-factor(test_activity$V1,levels = activity_names$V1,labels = activity_names$V2)
test<-data.frame("subject"=test_subject$V1,"activity"=test_activity$activity)
test<-cbind(test,test_measurements)
#Create training data frame
##Load the necesary files
train_subject<-read.table("UCI HAR Dataset/train/subject_train.txt",header = F)
train_activity<-read.table("UCI HAR Dataset/train/y_train.txt",header = F)
train_measurements<-read.table("UCI HAR Dataset/train/X_train.txt",header = F)
##Get the variables in a data frame
##Q3:Use descriptive activity names; Q4: Label data with descriptive variable names
names(train_measurements)<-measurements_names$V2
train_activity$activity<-factor(train_activity$V1,levels = activity_names$V1,labels = activity_names$V2)
train<-data.frame("subject"=train_subject$V1,"activity"=train_activity$activity)
train<-cbind(train,train_measurements)
#Q1:Merge training and test sets in one data set
library(reshape2)
test_melted<- melt(test,id.vars =c("subject","activity"), measure.vars=measurements_names$V2)
train_melted<- melt(train,id.vars =c("subject","activity"), measure.vars=measurements_names$V2)
data<-rbind(test_melted,train_melted) #since no subject ids are shared
#Q2: Extract measurements on mean and standard deviation for each measurement.
data<-arrange(data,subject,activity,variable)
wanted_variables<-c(grep("std()",measurements_names$V2,value=T,fixed = T),grep("mean()",measurements_names$V2,value=T,fixed = T))
filtered_data<-data[which(data$variable %in% wanted_variables),]
#Clear the environment
rm(list=setdiff(ls(), "filtered_data"))
#Q5: Create a tidy data set with the average of each variable for each activity and each subject
tidy_data<-aggregate(value ~ subject + activity + variable, data = filtered_data, FUN = mean)
names(tidy_data)[4]<-"mean"

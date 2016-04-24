### Load up packages
##
library(dplyr)
library(data.table)
library(tidyr)

## Read Input Data and Create Tables
##
FPath <- "C:/Users/IBM_ADMIN/Documents/UCI HAR Dataset"
# Read subject files
dataSubjectTrain <- tbl_df(read.table(file.path(FPath, "train", "subject_train.txt")))
dataSubjectTest  <- tbl_df(read.table(file.path(FPath, "test" , "subject_test.txt" )))

# Read activity files
dataActivityTrain <- tbl_df(read.table(file.path(FPath, "train", "Y_train.txt")))
dataActivityTest  <- tbl_df(read.table(file.path(FPath, "test" , "Y_test.txt" )))

#Read data files.
dataTrain <- tbl_df(read.table(file.path(FPath, "train", "X_train.txt" )))
dataTest  <- tbl_df(read.table(file.path(FPath, "test" , "X_test.txt" )))

## Merge Training and test to create OneTable 
## 
alldataSubject <- rbind(dataSubjectTrain, dataSubjectTest)
setnames(alldataSubject, "V1", "subject")
alldataActivity<- rbind(dataActivityTrain, dataActivityTest)
setnames(alldataActivity, "V1", "activityNum")

#combine the DATA training and test files
oneTable <- rbind(dataTrain, dataTest)

# name variables according to feature e.g.(V1 = "tBodyAcc-mean()-X")
dataFeatures <- tbl_df(read.table(file.path(filesPath, "features.txt")))
setnames(dataFeatures, names(dataFeatures), c("featureNum", "featureName"))
colnames(oneTable) <- dataFeatures$featureName

#column names for activity labels
activityLabels<- tbl_df(read.table(file.path(filesPath, "activity_labels.txt")))
setnames(activityLabels, names(activityLabels), c("activityNum","activityName"))

# Merge columns
alldataSubjAct<- cbind(alldataSubject, alldataActivity)
oneTable <- cbind(alldataSubjAct, oneTable)


## Extract measurement of mean and standard devation for each measurement

# Reading "features.txt" and extracting only the mean and standard deviation
dataFeaturesMeanStd <- grep("mean\\(\\)|std\\(\\)",dataFeatures$featureName,value=TRUE) #var name

# Taking only measurements for the mean and standard deviation and add "subject","activityNum"

dataFeaturesMeanStd <- union(c("subject","activityNum"), dataFeaturesMeanStd)
oneTable<- subset(oneTable,select=dataFeaturesMeanStd) 


## Rename activity names to be more descriptive

##enter name of activity into dataTable
oneTable <- merge(activityLabels, oneTable , by="activityNum", all.x=TRUE)
oneTable$activityName <- as.character(oneTable$activityName)

## create dataTable with variable means sorted by subject and Activity
oneTable$activityName <- as.character(oneTable$activityName)
dataAggr<- aggregate(. ~ subject - activityName, data = oneTable, mean) 
oneTable<- tbl_df(arrange(dataAggr,subject,activityName))


## Lable the data set with better variable names

names(oneTable)<-gsub("std()", "SD", names(oneTable))
names(oneTable)<-gsub("mean()", "Mean", names(oneTable))
names(oneTable)<-gsub("^t", "Time", names(oneTable))
names(oneTable)<-gsub("^f", "Frequency", names(oneTable))
names(oneTable)<-gsub("Acc", "Accelerometer", names(oneTable))
names(oneTable)<-gsub("Gyro", "Gyroscope", names(oneTable))
names(oneTable)<-gsub("Mag", "Magnitude", names(oneTable))
names(oneTable)<-gsub("BodyBody", "Body", names(oneTable))
# Names after
head(str(oneTable),6)

## Create 2nd tidy data set with the average of each variable for each activity and each subject

write.table(oneTable, "TidyData.txt", row.name=FALSE)

